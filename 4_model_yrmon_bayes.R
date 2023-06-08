# 4_model_yrmon_bayes.R
# model the bibliographic data using monthly counts rather than annual counts
# not available for scholar
# results used by 4_power_simulation.Rmd
# version using Winbugs
# July 2022
source('99_functions.R')
library(tidyverse)
library(R2WinBUGS)
library(season) # for adjacency matrix

# done as a function
run_bayes_yrmon = function(indata, # input data
                           bfile,
                     this_debug = FALSE,
                     add_2022 = TRUE, # add binary effect to cover drop in 2022
                     random_intercept = TRUE, # add a random intercept
                     linear_treatment = FALSE, # alternative to smooth treatment using linear `years since funding` x treatment interaction
                     model_with_treatment = FALSE, # include treatment or not
                     smooth_treatment = FALSE, # use smooth CAR treatment
                     smooth_calendar = FALSE,# smooth effect for calendar
                     pilot = TRUE, # pilot for MCMC or not
                     make_residuals = FALSE # calculate residuals
){
  
  ## write the winbugs code, depends on function options
  source('99_bugs_model_bespoke.R')
  
  # set up MCMC
  source('99_mcmc.R')
  debug = this_debug
  
  # prepare data -main difference for monthly is that there is only two databases
  indata = ungroup(indata)
  N = nrow(indata)
  M = length(unique(indata$number)) # number of researchers
  # matrix of summary pre-randomisation totals
  matrix_baseline = as.matrix(dplyr::select(indata, b_scopus, b_researchgate))
  # matrix of post-randomisation counts
  matrix_count = mutate(indata,
                        scopus = scopus / denom_scopus, # scale paper counts by database-specific denominator to adjust for final year of data collection not being a full year
                        researchgate = researchgate / denom_researchgate) %>%
    dplyr::select(scopus, researchgate) # this ordering must be remembered
  matrix_count = as.matrix(matrix_count)
  #
  C = ncol(matrix_count) # number of databases
  min_year = min(indata$year) # earliest year
  R = toeplitz(c(1, rep(0, C-1))) # for correlation between databases
  bdata = list(N = N, 
               C = C, 
               R = R,
               zeros = rep(0, C),
               january = as.numeric(indata$month == 1), # account for more papers being produced in January
               baseline = log2(matrix_baseline + 1), # log-transform (base 2)
               log_counts = log(matrix_count + 1) # log-transform (base e)
  )
  
  # add to data depending on model choices
  source('4_model_additions.R', local = TRUE) # had to switch on local

  ## standardise baseline counts to help with converge
  baseline_means = round(colMeans(bdata$baseline, na.rm=TRUE))
  bdata$baseline = bdata$baseline - matrix(rep(baseline_means, nrow(bdata$baseline)), ncol=2, byrow=TRUE)

  ## initial values
  source('4_make_initial_values.R')

  ## parameters to store
  source('4_make_parameters.R')
  parms = c(parms, 'zeta') # january
  
  ## run (cannot get DIC for MVN models)
  bugs_results = bugs(data = bdata, inits = inits, parameters = parms, model.file = bfile, DIC = FALSE,
                      n.chains = n.chains, n.iter = MCMC*thin*n.chains, n.thin = thin, bugs.seed = seed, debug = debug,
                      bugs.directory = "c:/Program Files/WinBUGS14")
  
  # make table of results
  table = make_bugs_results(bugs_results)
  
  # extract correlation matrix
  matrix = filter(table, str_detect(parameter, pattern = 'Sigma')) 
  varcov = matrix(matrix$mean, nrow = C)
  cormat = cov2cor(varcov)
  cormat = as.data.frame(cormat)
  colnames(cormat) = c('scopus', 'researchgate')
  rownames(cormat) = c('scopus', 'researchgate')
  
  # plot calendar year effect as percent change
  to_plot = filter(table, 
                   str_detect(parameter, pattern='calendar'),
                   !str_detect(parameter, pattern='tau')) %>%
    mutate(year = as.numeric(str_remove_all(parameter, '[^0-9]')) + min_year - 1,
           pc = 100*(exp(mean)- 1), # percent change
           lower = 100*(exp(lower)- 1),
           upper = 100*(exp(upper)- 1))
  cplot = ggplot(data=to_plot, aes(x=year, y=pc, ymin=lower, ymax=upper))+
    geom_hline(lty = 2, yintercept=0)+
    geom_ribbon(alpha = 0.2)+
    geom_line(size = 1.1)+
    xlab('Year')+
    ylab('Percent change')+
    theme_bw()+
    theme(panel.grid.minor = element_blank())
  
  # smoothed treatment effect (add smooth and main effect)
  smoothed = NULL
  if(smooth_treatment==TRUE){
    bugs_mat = bugs_results$sims.matrix
    cnames = colnames(bugs_mat)
    gindex = which(cnames == 'gamma')
    lindex = which(str_detect(cnames, pattern='^lambda'))
    smoothed = NULL
    for (i in 1:length(lindex)){
      combined = bugs_mat[,gindex] + bugs_mat[,lindex[i]] # add overall and CAR
      #
      ## p-values
      pos = combined > 0
      pos = mean(pos)
      pos.dash = 1 - pos
      pval = pmax(2*pmin(pos, pos.dash), 1/(2*MCMC))
      
      #
      dframe = data.frame(months_since = i,
                          mean = mean(combined),
                          lower = quantile(combined, 0.025),
                          upper = quantile(combined, 0.975),
                          pval = pval)
      smoothed = bind_rows(smoothed, dframe)
      
    }
    row.names(smoothed) = NULL
  }
  
  residuals = model_fit = NULL
  if (make_residuals == TRUE){
    ## checking residuals (for model using just 'mu' and just one chain) ##
    parms = 'mu'
    inits = list()
    inits[[1]] = bugs_results$last.values[[1]] # start chain with last values of previous model
    inits[[1]]$Omega = solve(inits[[1]]$Sigma) # invert matrix
    inits[[1]]$Sigma = NULL
    inits[[1]]$alpha_c = NULL
    if(random_intercept==TRUE){
      inits[[1]]$r_int_c = NULL
    }
    if(linear_treatment==TRUE | smooth_treatment == TRUE){
      inits[[1]]$diff = NULL
    }
    
    MCMC = 200; thin = 5; n.chains = 1 # much smaller run
    bugs_residuals = bugs(data = bdata, inits = inits, parameters = parms, model.file = bfile, DIC = FALSE,
                          n.chains = n.chains, n.iter = MCMC*thin*n.chains, n.thin = thin, bugs.seed = seed + 1, debug = debug,
                          bugs.directory = "c:/Program Files/WinBUGS14")
    fitted = make_bugs_results(bugs_residuals) %>% # extract the residuals
      mutate(parameter = str_remove_all(parameter, pattern='[^0-9|,]')) %>%
      separate(col=parameter, into=c('row','database_num'), convert = TRUE) %>%
      dplyr::select(-pvalue) %>%
      mutate(database = case_when(
        database_num == 1 ~ 'scopus', # convert number to name
        database_num == 2 ~ 'researchgate'
      ))
    #
    residuals = mutate(indata, 
                       row = 1:n(),
                       scopus = scopus / denom_scopus, # scale paper counts by database-specific denominator to adjust for final year of data collection not being a full year
                       researchgate = researchgate / denom_researchgate) %>%
      pivot_longer(cols=c('scopus', 'researchgate'),
                   names_to = 'database',
                   values_to = 'observed') %>%
      left_join(fitted, by=c('row','database')) %>%
      mutate(obs = log(observed+1),
             residual = obs - mean)
    ## plot the residuals
    # a) database
    rplot1 = ggplot(data=residuals, aes(x=residual))+
      geom_histogram()+
      facet_wrap(~database)+
      theme_bw()
    # b) person
    rplot2 = ggplot(data=residuals, aes(x=factor(number), y=residual))+
      geom_boxplot()+
      theme_bw()
    # c) calendar year
    rplot3 = ggplot(data=residuals, aes(x=factor(year), y=residual))+
      geom_boxplot()+
      theme_bw()
    # d) career year
    rplot4 = ggplot(data=residuals, aes(x=factor(career_year), y=residual))+
      geom_boxplot()+
      theme_bw()
    
    # R-squared and mean square error
    model_fit = filter(residuals, !is.na(observed)) %>%
      mutate(res2 = residual^2) %>%
      summarise(n=n(), 
                mse = mean(res2),
                rsq = cor(mean, obs)^2)
    
  } # end of residuals if
    
# return
to_return = list()
to_return$cormat = cormat
to_return$model_fit = model_fit
to_return$table = table
to_return$residuals = residuals
to_return$smooth = smoothed
to_return$cplot = cplot
return(to_return)

  } # end of function
  