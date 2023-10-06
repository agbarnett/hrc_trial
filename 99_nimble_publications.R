# 99_nimble_publications.R
# using nimble to run Bayesian model of publication counts
# from manual "A node following a multivariate distribution must be either entirely observed or entirely missing"
# August 2022
seed = rep(0, 2)
seed[1] = TeachingDemos::char2seed('Colchester')
seed[2] = TeachingDemos::char2seed('Port Vale')
source('99_mcmc.R')

## define the model
## version with multivariate errors for three databases ##
codeErrors <- nimbleCode({
  for (i in 1:N) {
    for(j in 1:C){
      counts[i, j] ~ dpois(mu[i, j])
      log(mu[i,j]) <- log(offset[i,j]) + alpha[j] + r_int[person[i]] + (beta*baseline[i,j]) + (gamma*treatment[i]) + errors[i,j]
    }
    errors[i, 1:C] ~ dmnorm(zeros[1:C], cov = Sigma[1:C, 1:C])
  }
  # intercept for each database
  for(j in 1:C){
    alpha[j] ~ dnorm(0, sd = 1000) 
  }
  # random intercept for each researcher - no need to centre
  for(j in 1:P){
    r_int[j] ~ dnorm(0, tau.person)
  }
  tau.person ~ dgamma(0.01, 0.01)
  beta ~ dnorm(0, sd = 10000)
  gamma ~ dnorm(0, sd = 10000)
  Sigma[1:C, 1:C] ~ dinvwish(R[1:C, 1:C], C)
})
## version without multivariate errors ##
# model is very much worse, so no need to compare #
code <- nimbleCode({
  for (i in 1:N) {
    for(j in 1:C){
      counts[i, j] ~ dpois(mu[i, j])
      log(mu[i,j]) <- log(offset[i,j]) + alpha[j] + r_int[person[i]] + (beta*baseline[i,j]) + (gamma*treatment[i])
    }
  }
  # intercept for each database
  for(j in 1:C){
    alpha[j] ~ dnorm(0, sd = 1000)
  }
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
matrix_count = mutate(analysis_ready_imputed[[imp]],
                      scholar = scholar,
                      scopus = scopus,
                      researchgate = researchgate) %>%
  dplyr::select(scholar, scopus, researchgate) # this ordering must be remembered
matrix_count = as.matrix(matrix_count)
# matrix of offsets
matrix_offset = mutate(analysis_ready_imputed[[imp]],
                       scholar = denom_scholar,
                      scopus = denom_scopus,
                      researchgate = denom_researchgate) %>%
  dplyr::select(scholar, scopus, researchgate) # this ordering must be remembered
# matrix of baseline
matrix_baseline = dplyr::select(analysis_ready_imputed[[imp]], b_scholar, b_scopus, b_researchgate) %>%
  mutate(b_scholar = b_scholar + 0.5,
         b_scopus = b_scopus + 0.5,
         b_researchgate = b_researchgate + 0.5) %>% # because of zero
  as.matrix() %>%
  log2() # transform
# centre baseline counts for better convergence
library(matrixStats)
median.baseline = colMedians(matrix_baseline, na.rm=TRUE)
matrix_baseline = t(t(matrix_baseline) - median.baseline)

## constants and data
R = toeplitz(c(1,0,0))
constantsErrors <- list(N = nrow(analysis_ready_imputed[[imp]]),
                        offset = matrix_offset, # adjust for final year that is under a year
                        R = R,
                  treatment = as.numeric(analysis_ready_imputed[[imp]]$funded == 'Funded'),
                  zeros = rep(0,3), 
                  baseline = matrix_baseline,
                  C = 3,
                  person = as.numeric(as.factor(analysis_ready_imputed[[imp]]$number)),
                  P = length(unique(analysis_ready_imputed[[imp]]$number)))
data <- list(counts = matrix_count)

## initial values
initsErrors <- list(beta = 0,
              Sigma = R,
              tau.person = 100,
              r_int = rep(0, constantsErrors$P),
              gamma = 0,
              alpha = c(0,0,0))

# parameters to store
parms = c('r_int','tau.person','beta','alpha','gamma')
parmsErrors = c(parms, 'Sigma', 'mu') # 

# models
modelErrors <- nimbleModel(codeErrors, 
                     data = data, 
                     inits = initsErrors, 
                     constants = constantsErrors)

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

# selected chains
index = which(colnames(mcmc_out_errors$samples$chain1) %in% c('alpha[1]','alpha[2]','alpha[3]','beta','gamma','tau.person'))
chain1 = mcmc_out_errors$samples$chain1[,index]
chain2 = mcmc_out_errors$samples$chain2[,index]
chains = list()
chains[[1]] = chain1
chains[[2]] = chain2

# extract summary (for model with errors)
table = as.data.frame(mcmc_out_errors$summary$all.chains) %>%
  tibble::rownames_to_column() %>%
  mutate(index = str_remove_all(rowname, '[A-Z|a-z]|\\[|\\]')) %>%
  separate(col=index, into=c('row','database'))

# add posterior p-values
pos = rbind(mcmc_out_errors$samples$chain1,mcmc_out_errors$samples$chain2) > 0
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
residuals = mutate(analysis_ready_imputed[[imp]], 
                   row = 1:n(),
                   scholar = scholar / denom_scholar, # scale paper counts by database-specific denominator to adjust for final year of data collection not being a full year
                   scopus = scopus / denom_scopus, # scale paper counts by database-specific denominator to adjust for final year of data collection not being a full year
                   researchgate = researchgate / denom_researchgate) %>%
  pivot_longer(cols=c('scholar', 'scopus', 'researchgate'),
               names_to = 'database',
               values_to = 'observed') %>%
  left_join(fitted, by=c('row','database')) %>%
  mutate(obs = observed,
         residual = obs - mean)

# now remove from table
table = filter(table, !str_detect(rowname, pattern='^Sigma'),
               !str_detect(rowname, pattern='^r_int'),
               !str_detect(rowname, pattern='^mu')) %>%
  select(-database) %>%
  rename('parameter' = 'rowname')

## model fit
model_fit = data.frame(outcome = 'Publications', 
                model = 'Time fixed, with errors',
                WAIC = mcmc_out_errors$WAIC$WAIC,
                pWAIC = mcmc_out_errors$WAIC$pWAIC)

# save results as a list
results_main = list()
results_main$covmat = covmat
results_main$model_fit = model_fit
results_main$table = table
results_main$residuals = residuals
results_main$chains = chains
