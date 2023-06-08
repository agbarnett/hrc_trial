# 99_nimble_publications.R
# using nimble to run Bayesian model of publication counts
# from manual "A node following a multivariate distribution must be either entirely observed or entirely missing"
# August 2022
seed = TeachingDemos::char2seed('Colchester')
source('99_mcmc.R')

## define the models
## version with multivariate errors for three databases ##
codeErrors <- nimbleCode({
  for (i in 1:N) {
    for(j in 1:C){
      log_counts[i, j] ~ dnorm(mean = mu[i, j], tau)
      mu[i,j] <- alpha[j] + r_int[person[i]] + (beta*baseline[i,j]) + (gamma*treatment[i]) + errors[i,j]
    }
    errors[i, 1:C] ~ dmnorm(zeros[1:C], cov = Sigma[1:C, 1:C])
  }
  # intercept for each database
  for(j in 1:C){
    alpha[j] ~ dnorm(0, sd = 1000)
  }
  tau ~ dgamma(0.1, 0.1) # overall precision
  # random intercept for each researcher - no need to centre
  for(j in 1:P){
    r_int[j] ~ dnorm(0, tau.person)
  }
  tau.person ~ dgamma(0.1, 0.1)
  beta ~ dnorm(0, sd = 10000)
  gamma ~ dnorm(0, sd = 10000)
  Sigma[1:C, 1:C] ~ dinvwish(R[1:C, 1:C], C)
})
## version without multivariate errors ##
code <- nimbleCode({
  for (i in 1:N) {
    for(j in 1:C){
      log_counts[i, j] ~ dnorm(mean = mu[i, j], tau)
      mu[i,j] <- alpha[j] + r_int[person[i]] + (beta*baseline[i,j]) + (gamma*treatment[i])
    }
  }
  # intercept for each database
  for(j in 1:C){
    alpha[j] ~ dnorm(0, sd = 1000)
  }
  tau ~ dgamma(0.1, 0.1)
  # random intercept for each researcher - no need to centre
  for(j in 1:P){
    r_int[j] ~ dnorm(0, tau.person)
  }
  tau.person ~ dgamma(0.1, 0.1)
  gamma ~ dnorm(0, sd = 10000)
  beta ~ dnorm(0, sd = 10000)
})


## data
# matrix of post-randomisation counts
matrix_count = mutate(analysis_ready,
                      scholar = scholar / denom_scholar, # scale paper counts by database-specific denominator to adjust for final year of data collection not being a full year
                      scopus = scopus / denom_scopus, # scale paper counts by database-specific denominator to adjust for final year of data collection not being a full year
                      researchgate = researchgate / denom_researchgate) %>%
  dplyr::select(scholar, scopus, researchgate) # this ordering must be remembered
matrix_count = as.matrix(matrix_count)
# replace missing baseline with zero, probably does not matter as outcome is NA - to check
matrix_baseline = dplyr::select(analysis_ready, b_scholar, b_scopus, b_researchgate) %>%
  tidyr::replace_na(list(b_scholar=0, b_scopus=0, b_researchgate=0)) %>%
  as.matrix()

## constants and data
R = toeplitz(c(1,0,0))
constantsErrors <- list(N = nrow(analysis_ready),
                  R = R,
                  treatment = as.numeric(analysis_ready$funded == 'Funded'),
                  zeros = rep(0,3), 
                  person = as.numeric(as.factor(analysis_ready$number)),
                  P = length(unique(analysis_ready$number)),
                  baseline = log2(matrix_baseline + 1),
                  C = 3)
constants <- list(N = nrow(analysis_ready),
                  treatment = as.numeric(analysis_ready$funded == 'Funded'),
                  person = as.numeric(as.factor(analysis_ready$number)),
                P = length(unique(analysis_ready$number)),
                baseline = log(matrix_baseline + 1),
                C = 3)
data <- list(log_counts = log(matrix_count + 1))

## initial values
initsErrors <- list(beta = 0.2,
              Sigma = R,
              gamma = 0,
              tau.person = 100,
              r_int = rep(0, constantsErrors$P),
              tau = 1,
              alpha = c(0,0,0))
inits <- list(beta = 0.2,
              tau.person = 100,
              gamma = 0,
              r_int = rep(0, constants$P),
              tau = 1, 
              alpha = c(0,0,0))

# parameters to store
parms = c('r_int','tau','tau.person','beta','alpha','gamma')
parmsErrors = c(parms, 'Sigma', 'mu')

# models
modelErrors <- nimbleModel(codeErrors, 
                     data = data, 
                     inits = initsErrors, 
                     constants = constantsErrors)
model <- nimbleModel(code, 
                    data = data, 
                    inits = inits, 
                    constants = constants)

# MCMC samples
mcmc_out <- nimbleMCMC(model = model,
                       inits = inits,
                       monitors = parms,
                       niter = MCMC*2*thin, # times 2 for burn-in 
                       thin = thin,
                       nchains = n.chains, 
                       nburnin = MCMC,
                       summary = TRUE, 
                       setSeed = seed,
                       WAIC = TRUE)
# MCMC samples for model with Errors
mcmc_out_errors <- nimbleMCMC(model = modelErrors,
                       inits = initsErrors,
                       monitors = parmsErrors,
                       niter = MCMC*2*thin, # times 2 for burn-in 
                       thin = thin,
                       nchains = n.chains, 
                       nburnin = MCMC,
                       summary = TRUE, 
                       setSeed = seed,
                       WAIC = TRUE)

# check predictions
node_names = attr(mcmc_out_errors$samples$chain1, 'dimnames')
is_mu = str_detect(node_names, '^mu')

# convert chains to coda
mcmc = as.mcmc(mcmc_out_errors$samples$chain1) # can only do one chain
#for_plot = ggs(mcmc) # convert reading for printing
#ggmcmc(for_plot) # prints a lot

# extract summary
table = as.data.frame(mcmc_out_errors$summary$all.chains) %>%
  tibble::rownames_to_column() %>%
  mutate(index = str_remove_all(rowname, '[A-Z|a-z]|\\[|\\]')) %>%
  separate(col=index, into=c('row','database'))

# add posterior p-values
pos = mcmc_out_errors$samples$chain1 > 0
pos = colMeans(pos)
pos.dash = 1 - pos
pval = pmax(2*pmin(pos, pos.dash), 1/(2*MCMC))
table = bind_cols(table, pval)
names(table) = c('rowname','mean','median','sd','lower','upper','row','database','pvalue')

# variance-covariance matrix
covmat = filter(table, str_detect(rowname, pattern='^Sigma')) %>%
  select(row, database, mean) %>%
  pivot_wider(values_from = "mean", names_from='database')

# random intercepts
random_intercept = filter(table, str_detect(rowname, pattern='^r_int'))

## predictions and residuals
fitted = filter(table, str_detect(rowname, pattern='^mu')) %>% # model predictions
  select(database, row, mean) %>%
  mutate(database = case_when(
    database == '1' ~ 'scholar',
    database == '2' ~ 'scopus',
    database == '3' ~ 'researchgate'),
    row = as.numeric(row))
#
residuals = mutate(analysis_ready, 
                   row = 1:n(),
                   scholar = scholar / denom_scholar, # scale paper counts by database-specific denominator to adjust for final year of data collection not being a full year
                   scopus = scopus / denom_scopus, # scale paper counts by database-specific denominator to adjust for final year of data collection not being a full year
                   researchgate = researchgate / denom_researchgate) %>%
  pivot_longer(cols=c('scholar', 'scopus', 'researchgate'),
               names_to = 'database',
               values_to = 'observed') %>%
  left_join(fitted, by=c('row','database')) %>%
  mutate(obs = log(observed+1),
         residual = obs - mean)

# now remove from table
table = filter(table, !str_detect(rowname, pattern='^Sigma'),
               !str_detect(rowname, pattern='^r_int'),
               !str_detect(rowname, pattern='^mu')) %>%
  select(-database) %>%
  rename('parameter' = 'rowname')

## model fit
# tested model with and without errors, result: model with errors is clearly better
f1 = data.frame(outcome = 'Publications', 
                model = 'Time fixed, no errors',
                WAIC = mcmc_out$WAIC$WAIC,
                pWAIC = mcmc_out$WAIC$pWAIC)
f2 = data.frame(outcome = 'Publications', 
                model = 'Time fixed, with errors',
                WAIC = mcmc_out_errors$WAIC$WAIC,
                pWAIC = mcmc_out_errors$WAIC$pWAIC)
model_fit = bind_rows(f1, f2)

# save results as a list
results_main = list()
results_main$covmat = covmat
results_main$model_fit = model_fit
results_main$table = table
results_main$residuals = residuals
