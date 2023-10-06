# 99_nimble_publications_monthly_time_varying.R
# using nimble to run Bayesian model of publication counts
# from manual "A node following a multivariate distribution must be either entirely observed or entirely missing"
# version using monthly data and time-varying treatment effect
# August 2022
seed = rep(0, 2)
seed[1] = TeachingDemos::char2seed('Colchester')
seed[2] = TeachingDemos::char2seed('Port Vale')
source('99_mcmc.R')

## define the models
## version with multivariate errors for three databases ##
codeErrors <- nimbleCode({
  for (i in 1:N) {
    trt[i] <- (lambda*years_since[i] + gamma)*treatment[i]
    control[i] <- (delta*years_since[i])*(1-treatment[i])
    for(j in 1:C){
      log_counts[i, j] ~ dnorm(mean = mu[i, j], tau)
      mu[i,j] <- alpha[j] + zeta*january[i] + r_int[person[i]] + (beta*baseline[i,j]) + trt[i] + control[i] + errors[i,j]
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
  lambda ~ dnorm(0, sd = 10000)
  delta ~ dnorm(0, sd = 10000)
  zeta ~ dnorm(0, sd = 10000)
  gamma ~ dnorm(0, sd = 10000)
  Sigma[1:C, 1:C] ~ dinvwish(R[1:C, 1:C], C)
  # treatment difference (years since is centred at 2?)
  for (k in 1:T){
    diff[k] <- (lambda*(k-2) + gamma) - (delta*(k-2))
  }
  slope.diff <- lambda - delta
})

## data
# matrix of post-randomisation counts
matrix_count = mutate(analysis_ready_monthly,
                      scopus = scopus / denom_scopus, # scale paper counts by database-specific denominator to adjust for final year of data collection not being a full year
                      researchgate = researchgate / denom_researchgate) %>%
  dplyr::select(scopus, researchgate) # this ordering must be remembered
matrix_count = as.matrix(matrix_count)
# replace missing baseline with zero, probably does not matter as outcome is NA - to check
matrix_baseline = dplyr::select(analysis_ready_monthly, b_scopus, b_researchgate) %>%
  tidyr::replace_na(list(b_scopus=0, b_researchgate=0)) %>%
  as.matrix()

## constants and data
R = toeplitz(c(1,0)) # smaller for monthly
constantsErrors <- list(N = nrow(analysis_ready_monthly),
                  R = R,
                  years_since = analysis_ready_monthly$years_since, # fractions are months
                  T = floor(max(analysis_ready_monthly$years_since)),
                  treatment = as.numeric(analysis_ready_monthly$funded == 'Funded'),
                  zeros = c(0,0), 
                  person = as.numeric(as.factor(analysis_ready_monthly$number)),
                  P = length(unique(analysis_ready_monthly$number)),
                  baseline = log2(matrix_baseline + 1),
                  january = as.numeric(analysis_ready_monthly$month == 1),
                  C = 2)
data <- list(log_counts = log(matrix_count + 1))

## initial values
initsErrors <- list(beta = 0.2,
              Sigma = R,
              gamma = 0,
              delta = 0,
              lambda = 0,
              zeta = 0,
              tau.person = 100,
              r_int = rep(0, constantsErrors$P),
              tau = 1,
              alpha = c(0,0))

# parameters to store
parmsErrors = c('r_int','tau','tau.person','beta','alpha','gamma','delta', 'lambda', 'zeta','Sigma', 'diff','slope.diff','mu')

# models
modelErrors <- nimbleModel(codeErrors, 
                     data = data, 
                     inits = initsErrors, 
                     constants = constantsErrors)

# MCMC samples
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
index = which(colnames(mcmc_out$samples$chain1) %in% c('alpha','beta','gamma','tau.person'))
chain1 = mcmc_out$samples$chain1[,index]
chain2 = mcmc_out$samples$chain2[,index]
chains = list()
chains[[1]] = chain1
chains[[2]] = chain2

# extract summary
table = as.data.frame(mcmc_out_errors$summary$all.chains) %>%
  tibble::rownames_to_column() %>%
  mutate(index = str_remove_all(rowname, '[A-Z|a-z]|\\[|\\]')) %>%
  separate(col=index, into=c('row','database'))

# add posterior p-values
pos = rbind(mcmc_out_errors$samples$chain1, mcmc_out_errors$samples$chain2) > 0
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
    database == '1' ~ 'scopus',
    database == '2' ~ 'researchgate'),
    row = as.numeric(row))
#
residuals = mutate(analysis_ready_monthly, 
                   row = 1:n(),
                   scopus = scopus / denom_scopus, # scale paper counts by database-specific denominator to adjust for final year of data collection not being a full year
                   researchgate = researchgate / denom_researchgate) %>%
  pivot_longer(cols=c('scopus', 'researchgate'),
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
model_fit = data.frame(outcome = 'Monthly publications', 
                model = 'Time varying, with errors',
                WAIC = mcmc_out_errors$WAIC$WAIC,
                pWAIC = mcmc_out_errors$WAIC$pWAIC)

# save results as a list
results_yrmon_monthly_varying = list()
results_yrmon_monthly_varying$covmat = covmat
results_yrmon_monthly_varying$model_fit = model_fit
results_yrmon_monthly_varying$table = table
results_yrmon_monthly_varying$residuals = residuals
results_yrmon_monthly_varying$chains = chains
