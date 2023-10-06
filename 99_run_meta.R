# 99_run_meta.R

# used MI combine instead

# run the meta-analysis model to combine the estimates across imputations and bibliographic databases
# Sep 2023
seeds = rep(0,2)
seeds[1] = TeachingDemos::char2seed('Barnsley') # one per chain
seeds[2] = TeachingDemos::char2seed('Exeter')

## Bayesian meta-analysis code ##
# imputed results are correlated as our results from the same database
codeMeta <- nimbleCode({
  for (i in 1:N) {
    mean[i] ~ dnorm(mean = mu[i], sd = sd[i])
    mu[i] <- overall + alpha[database[i]] + beta[imputation[i]]
  }
  for (j in 1:3) {
    alpha[j] ~ dnorm(mean = 0, tau = tau.database)
  }
  for (j in 1:M) {
    beta[j] ~ dnorm(mean = 0, tau = tau.imputation)
  }
  tau.database ~ dgamma(0.001, 0.001)
  tau.imputation ~ dgamma(0.001, 0.001)
  overall ~ dnorm(0, sd = 1000)
})

# data and constants
data_to_run = filter(combined_results, parameter=='gamma') # just interested in the effect of funding
constants <- list(N = nrow(data_to_run),
                  M = max(data_to_run$imputation),
                  database = data_to_run$database_num,
                  imputation = data_to_run$imputation)
data <- list(mean = data_to_run$mean, sd = data_to_run$sd)

## initial values
start = mean(data_to_run$mean)
inits <- list(alpha = rep(0, 3), beta = rep(0,n_impute), overall = start, tau.database = 100, tau.imputation = 100)

# parameters to store, mu for predictions
parms = c('alpha','beta','overall','tau.database','tau.imputation')

## models
model <- nimbleModel(codeMeta, 
                     data = data, 
                     inits = inits, 
                     constants = constants)
## MCMC samples
mcmc_out <- nimbleMCMC(model = model,
                       inits = inits,
                       monitors = parms,
                       niter = MCMC*2*thin, # times 2 for burn-in 
                       thin = thin,
                       nchains = n.chains, 
                       nburnin = MCMC,
                       summary = TRUE, 
                       setSeed = seeds, # one for each chain
                       WAIC = FALSE)

# table of estimates
table = as.data.frame(mcmc_out$summary$all.chains) %>%
  tibble::rownames_to_column() 
names(table) = c('var','mean','median','sd','lower','upper')
table = filter(table, var =='overall')

# standard meta-analysis
library(metaforest)
m_re <- rma(yi = data_to_run$mean,
            sei = data_to_run$sd,
            method = "DL")
m_re
