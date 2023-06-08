# 4_model_citations_bayes.R
# model the citation data (using just one database - scopus) over time
# used by 5_main_analysis.Rmd
# version using Winbugs
# July 2022
source('99_functions.R')
library(tidyverse)
library(R2WinBUGS)
library(season) # for adjacency matrix

## write the winbugs code
source('99_bugs_models_citations.R')

# done as a function
run_bayes_citation = function(indata, # input data
                     this_debug = FALSE,
                     model_with_treatment = FALSE, # include treatment or not
                     smooth_treatment = FALSE, # use smooth CAR treatment
                     linear_time = FALSE, # model linear x treatment instead of CAR
                     pilot = TRUE, # pilot for MCMC or not
                     log_dependent = TRUE, # log dependent variable or not
                     make_residuals = FALSE # calculate residuals
){
  # set up MCMC
  source('99_mcmc.R')
  debug = this_debug
  
  # prepare data
  indata = ungroup(indata)
  N = nrow(indata)
  M = length(unique(indata$number)) # number of researchers
  # CAR prior
  Y = length(unique(indata$year)) # calendar year
  x = rep(NA, Y); x[2] = 1
  V = toeplitz(x)
  Adjacency = season::createAdj(V)
  #
  min_year = min(indata$year) # earliest year
  bdata = list(N = N, 
               Y = Y,
               adj = Adjacency$adj, 
               num = Adjacency$num,
               weights = Adjacency$weight,
             #  person = as.numeric(as.factor(indata$number)), # person, random intercept causes model convergence issues
               year = indata$year - min_year + 1, # year as an integer
               baseline = log2(indata$b_scopus + 1) # log-transform baseline (base 2)
  )
  #
  if(log_dependent == TRUE){
    bdata$outcome = log( (indata$scopus / indata$denom_scopus) + 1)  # log-transform outcome (base e)
  }
  #
  if(log_dependent == FALSE){
    bdata$ outcome = indata$scopus / indata$denom_scopus # outcome for average
  }
  # 
  if(model_with_treatment == TRUE){
    bdata$treatment = as.numeric(indata$funded == 'Funded')
  }
  # linear time by treatment interaction
  if(model_with_treatment == TRUE & smooth_treatment==TRUE & linear_time == TRUE){
    bdata$years_since = indata$years_since
    bdata$years_since = bdata$years_since - median(bdata$years_since) # centre
  }
  # smooth CAR for treatment over time
  if(model_with_treatment == TRUE & smooth_treatment==TRUE & linear_time == FALSE){
    bdata$years_since = indata$years_since
    bdata$years_since = ifelse(bdata$years_since==0, 1, bdata$years_since) # avoid zero index, is fixed in model by treatment = 0
    bdata$T = max(bdata$years_since)
    # adjacency matrix for smooth treatment effect
    x = rep(NA, bdata$T); x[2] = 1
    V = toeplitz(x)
    Adjacency = season::createAdj(V)
    bdata$adj_trt = Adjacency$adj
    bdata$weights_trt = Adjacency$weight
    bdata$num_trt = Adjacency$num
  }
  ## standardise baseline counts to help with converge
  baseline_mean = round(mean(bdata$baseline, na.rm=TRUE)) # overall mean
  bdata$baseline = bdata$baseline - baseline_mean
  
  ## initial values
  inits = list(int = 0, beta = 0, tau.calendar = 1, tau = 1) # start all with no flag for mean or variance
  if(model_with_treatment == TRUE){
    inits$gamma = 0 # initial value for treatment
  }
  if(smooth_treatment==TRUE & linear_time == FALSE){
    inits$lambda = rep(0, bdata$T)
    inits$tau.trt = 1
    inits$delta = rep(0, bdata$T)
    inits$tau.delta = 1
  }
  if(smooth_treatment==TRUE & linear_time == TRUE){
    inits$lambda = 0
    inits$delta = 0
  }
  inits = rep(list(inits), n.chains) # repeat per chains
  
  ## parameters to store
  parms = c('int','beta','calendar','tau','tau.calendar') # 
  if(model_with_treatment == TRUE){
    parms = c(parms, 'gamma')
  }
  if(smooth_treatment == TRUE){
    parms = c(parms, 'lambda', 'delta')
  }
  
  ## model file with or without treatment
  this_model_file = case_when(
    model_with_treatment == FALSE ~ bfile_citations_no_trt, 
    model_with_treatment == TRUE & smooth_treatment == FALSE ~ bfile_citations,
    model_with_treatment == TRUE & smooth_treatment == TRUE & linear_time == FALSE ~ bfile_citations_treatment_car,
    model_with_treatment == TRUE & smooth_treatment == TRUE & linear_time == TRUE ~ bfile_citations_treatment_linear)

  ## run 
  bugs_results = bugs(data = bdata, inits = inits, parameters = parms, model.file = this_model_file, DIC = FALSE,
                      n.chains = n.chains, n.iter = MCMC*thin*n.chains, n.thin = thin, bugs.seed = seed, debug = debug,
                      bugs.directory = "c:/Program Files/WinBUGS14")
  
  # make table of results
  table = make_bugs_results(bugs_results)
  
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
    if(length(gindex) !=1){cat('error, should be single gamma parameter.\n')}
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
      dframe = data.frame(years_since = i,
                         mean = mean(combined),
                         lower = quantile(combined, 0.025),
                         upper = quantile(combined, 0.975),
                         pval = pval)
      smoothed = bind_rows(smoothed, dframe)
      
    }
    row.names(smoothed) = NULL
  }

  # residuals 
  residuals = model_fit = NULL
  if (make_residuals == TRUE){
    ## checking residuals (for model using just 'mu' and just one chain) ##
    parms = 'mu'
    inits = list()
    inits[[1]] = bugs_results$last.values[[1]] # start chain with last values of previous model
    
    MCMC = 200; thin = 5; n.chains = 1 # much smaller run
    bugs_residuals = bugs(data = bdata, inits = inits, parameters = parms, model.file = this_model_file, DIC = FALSE,
                          n.chains = n.chains, n.iter = MCMC*thin*n.chains, n.thin = thin, bugs.seed = seed + 1, debug = debug,
                          bugs.directory = "c:/Program Files/WinBUGS14")
    fitted = make_bugs_results(bugs_residuals) %>% # extract the residuals
      mutate(row = str_remove_all(parameter, pattern='[^0-9]'),
             row = as.numeric(row)) %>%
      dplyr::select(-pvalue)
    #
    residuals = mutate(indata, 
                       row = 1:n(),
                       scopus = scopus / denom_scopus) %>%# scale
      left_join(fitted, by='row') %>% 
      mutate(obs = ifelse(log_dependent ==TRUE, log(scopus+1), scopus),
             residual = obs - mean)
    ## plot the residuals
    # a) person
    rplot1 = ggplot(data=residuals, aes(x=factor(number), y=residual))+
      geom_boxplot()+
      theme_bw()
    # b) calendar year
    rplot2 = ggplot(data=residuals, aes(x=factor(year), y=residual))+
      geom_boxplot()+
      theme_bw()
    # c) career year
    rplot3 = ggplot(data=residuals, aes(x=factor(career_year), y=residual))+
      geom_boxplot()+
      theme_bw()
    
    # R-squared and mean square error
    model_fit = filter(residuals, !is.na(scopus)) %>%
      mutate(res2 = residual^2) %>%
      summarise(n=n(), 
                mse = mean(res2),
                rsq = cor(mean, obs)^2)
    
  } # end of residuals if
    

# return
to_return = list()
to_return$model_fit = model_fit
to_return$table = table
to_return$residuals = residuals
to_return$smooth = smoothed
to_return$cplot = cplot
return(to_return)

  } # end of function
  