# 99_nimble_publications_poisson.R
# using nimble to run Bayesian model of publication counts
# version using separate Poisson models for each bibliographic database
### called by 5_main_analysis_poisson.Rmd ###
# September 2023
seed = TeachingDemos::char2seed('Port vale')
source('99_mcmc.R')

## rename main outcome variable, offset, and baseline; only use data with non-missing dependent variable
non_missing = analysis_ready_imputed[[imp]] # imputed data 
index = which(names(non_missing) == dependent)
names(non_missing)[index] = 'counts' # rename
index = which(names(non_missing) == paste('b_', dependent, sep=''))
names(non_missing)[index] = 'baseline' # rename
index = which(names(non_missing) == paste('denom_', dependent, sep=''))
names(non_missing)[index] = 'offset' # rename
non_missing = filter(non_missing, !is.na(counts))

## define the models
# a) time-varying effect
codePoisson <- nimbleCode({
  for (i in 1:N) {
    counts[i] ~ dpois(mu[i])
    log(mu[i]) <- log(offset[i]) + alpha + r_int[person[i]] + (beta*baseline[i]) + (gamma*treatment[i])
  }
  # random intercept for each researcher - no need to centre
  for(j in 1:P){
    r_int[j] ~ dnorm(0, tau.person)
  }
  tau.person ~ dgamma(0.1, 0.1)
  alpha ~ dnorm(0, sd = 10000)
  beta ~ dnorm(0, sd = 10000)
  gamma ~ dnorm(0, sd = 10000)
})
# b) time-varying effect
codePoissonVarying <- nimbleCode({
  for (i in 1:N) {
    counts[i] ~ dpois(mu[i])
    trt[i] <- (lambda*years_since[i] + gamma)*treatment[i]
    control[i] <- (delta*years_since[i])*(1-treatment[i])
    log(mu[i]) <- log(offset[i]) + alpha + r_int[person[i]] + (beta*baseline[i]) + trt[i] + control[i]
  }
  # random intercept for each researcher - no need to centre
  for(j in 1:P){
    r_int[j] ~ dnorm(0, tau.person)
  }
  tau.person ~ dgamma(0.1, 0.1)
  alpha ~ dnorm(0, sd = 10000)
  beta ~ dnorm(0, sd = 10000)
  lambda ~ dnorm(0, sd = 10000)
  delta ~ dnorm(0, sd = 10000)
  gamma ~ dnorm(0, sd = 10000)
  # for plotting mean treatment effect
  for (j in 1:F){
    trt.fitted[j] <- (lambda*years_since.fitted[j] + gamma)*treatment.fitted[j]
    control.fitted[j] <- (delta*years_since.fitted[j])*(1-treatment.fitted[j])
    log(mu.fitted[j]) <- alpha + (beta*baseline.fitted) + trt.fitted[j] + control.fitted[j] 
  }
})

## constants and data
# make centred baseline
baseline = log2(non_missing$baseline + 1)
median.baseline = median(baseline)
c.baseline = baseline - median.baseline
#
constants <- list(N = nrow(non_missing),
                  offset = non_missing$offset, # adjust for final year that is under a year
                  treatment = as.numeric(non_missing$funded == 'Funded'),
                  person = as.numeric(as.factor(non_missing$number)),
                  P = length(unique(non_missing$number)),
                  baseline = c.baseline)
constants_varying <- constants
constants_varying$years_since = non_missing$years_since
data <- list(counts = non_missing$counts)
# data for fitted values
for_plot = expand.grid(years.fitted = 1:6, treatment = 0:1) 
constants_varying$F = nrow(for_plot)
constants_varying$treatment.fitted = for_plot$treatment
constants_varying$years_since.fitted = for_plot$years.fitted
baseline.fitted = log2(median(non_missing$baseline)+1)
baseline.fitted = baseline.fitted - median.baseline # centre as above
constants_varying$baseline.fitted = baseline.fitted # use median for baseline, will effect intercept for fitted values

## initial values
inits <- list(alpha = 0,
              beta = 0,
              gamma = 0,
              tau.person = 100,
              r_int = rep(0, constants$P))
inits_varying = inits
inits_varying$lambda = 0
inits_varying$delta = 0

# parameters to store, mu for predictions
parms = c('r_int','tau.person','beta','alpha','gamma','mu')
parms_varying = c(parms, 'lambda','delta', 'mu.fitted')

## models
# a) time-fixed
model <- nimbleModel(codePoisson, 
                     data = data, 
                     inits = inits, 
                     constants = constants)
# b) time-varying (same data)
model_varying <- nimbleModel(codePoissonVarying, 
                     data = data, 
                     inits = inits_varying, 
                     constants = constants_varying)

## MCMC samples
# a) time-fixed
mcmc_out <- nimbleMCMC(model = model,
                       inits = inits,
                       monitors = parms,
                       niter = MCMC*2*thin, # times 2 for burn-in 
                       thin = thin,
                       nchains = n.chains, 
                       nburnin = MCMC,
                       summary = TRUE, 
#                       samplesAsCodaMCMC = TRUE, # needed for diagnostics
                       setSeed = seeds, # one for each chain
                       WAIC = TRUE)
# b) time-varying
mcmc_out_varying <- nimbleMCMC(model = model_varying,
                       inits = inits_varying,
                       monitors = parms_varying,
                       niter = MCMC*2*thin, # times 2 for burn-in 
                       thin = thin,
                       nchains = n.chains, 
                       nburnin = MCMC,
                       summary = TRUE, 
                       setSeed = seed,
                       WAIC = TRUE)

# get the summary stats from the chains
# a) time-fixed
table = as.data.frame(mcmc_out$summary$all.chains) %>%
  tibble::rownames_to_column() %>%
  mutate(index = str_remove_all(rowname, '[A-Z|a-z]|\\[|\\]|_'),
         index = as.numeric(index))
# b) time-varying
table_varying = as.data.frame(mcmc_out_varying$summary$all.chains) %>%
  tibble::rownames_to_column() %>%
  mutate(index = str_remove_all(rowname, '[A-Z|a-z]|\\[|\\]|_'),
         index = as.numeric(index))

## add posterior p-values (two-sided)
# a) time-fixed 
pos = rbind(mcmc_out$samples$chain1,mcmc_out$samples$chain2) > 0
pos = colMeans(pos)
pos.dash = 1 - pos
pval = pmax(2*pmin(pos, pos.dash), 1/(2*MCMC))
table = bind_cols(table, pval)
names(table) = c('rowname','mean','median','sd','lower','upper','row','pvalue')
# b) time-varying
pos = rbind(mcmc_out_varying$samples$chain1, mcmc_out_varying$samples$chain2) > 0
pos = colMeans(pos)
pos.dash = 1 - pos
pval = pmax(2*pmin(pos, pos.dash), 1/(2*MCMC))
table_varying = bind_cols(table_varying, pval)
names(table_varying) = c('rowname','mean','median','sd','lower','upper','row','pvalue')

# random intercepts
random_intercept = filter(table, str_detect(rowname, pattern='^r_int'))
random_intercept_varying = filter(table_varying, str_detect(rowname, pattern='^r_int'))

## predictions and residuals
fitted1 = filter(table, str_detect(rowname, pattern='^mu')) %>% # model predictions
  select(row, mean) 
fitted2 = filter(table_varying, str_detect(rowname, pattern='^mu\\[')) %>% # model predictions
  select(row, mean) %>%
  rename('mean_varying' = 'mean')
#
residuals = mutate(non_missing, 
                   row = 1:n(),
                   scaled = counts/offset) %>% # divide by offset
  left_join(fitted1, by=c('row')) %>%
  left_join(fitted2, by=c('row')) %>%
  mutate(residual = scaled - mean,
         residual_varying = scaled - mean_varying)

# data for predictions over time for time-varying model
plot_varying = filter(table_varying, 
                      str_detect(rowname, pattern='^mu\\.')) %>% 
  bind_cols(for_plot)
  
# now remove predictions and intercepts from table
table = filter(table, 
               !str_detect(rowname, pattern='^r_int'),
               !str_detect(rowname, pattern='^mu')) %>%
  rename('parameter' = 'rowname')
table_varying = filter(table_varying, 
               !str_detect(rowname, pattern='^r_int'),
               !str_detect(rowname, pattern='^mu')) %>%
  rename('parameter' = 'rowname')

# WAIC
fit1 = data.frame(model = 'Time-fixed',
                  WAIC = mcmc_out$WAIC$WAIC,
                pWAIC = mcmc_out$WAIC$pWAIC)
fit2 = data.frame(model = 'Time-varying',
                  WAIC = mcmc_out_varying$WAIC$WAIC,
                  pWAIC = mcmc_out_varying$WAIC$pWAIC)
model_fit = bind_rows(fit1, fit2)

### Save chains for gamma for meta-analysis
index = which(colnames(mcmc_out$samples$chain1) %in% c('alpha','beta','gamma','tau.person'))
chain1 = mcmc_out$samples$chain1[,index]
chain2 = mcmc_out$samples$chain2[,index]
chains = list()
chains[[1]] = chain1
chains[[2]] = chain2
#chains = as.mcmc(chains) # not working
#res = ggs(S = chains, burnin=FALSE) # make into object for ggmcmc

# save results as a list
results_main = list()
results_main$model_fit = model_fit
results_main$table = table
results_main$table_varying = table_varying
results_main$plot_varying = plot_varying
results_main$residuals = residuals
results_main$chains = chains
