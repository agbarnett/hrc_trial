# 99_nimble_altmetric.R
# using nimble to run Bayesian model of citation counts
# no need for offset as fewer papers cancel fewer citations
# version with citations per paper outcome
# August 2022
seed = rep(0, 2)
seed[1] = TeachingDemos::char2seed('Colchester')
seed[2] = TeachingDemos::char2seed('Port Vale')
source('99_mcmc.R')

## define the model
code <- nimbleCode({
  for (i in 1:N) {
    score[i] ~ dnorm(mean = mu[i], tau)
    mu[i] <- alpha + r_int[person[i]] + (beta*baseline[i]) + (gamma*treatment[i])
  }
  # random intercept for each researcher - no need to centre
  for(j in 1:P){
    r_int[j] ~ dnorm(0, tau.person)
  }
  tau.person ~ dgamma(0.01, 0.01)
  tau ~ dgamma(0.01, 0.01) # overall precision
  alpha ~ dnorm(0, sd = 10000)
  beta ~ dnorm(0, sd = 10000)
  gamma ~ dnorm(0, sd = 10000)
})

## data
# centre baseline for chain mixing
baseline = log2(analysis_ready_altmetric$b_altmetric + 1)
baseline.centre = baseline - mean(baseline)
#
constants <- list(N = nrow(analysis_ready_altmetric),
                  treatment = as.numeric(analysis_ready_altmetric$funded == 'Funded'),
                  person = as.numeric(as.factor(analysis_ready_altmetric$number)),
                  P = length(unique(analysis_ready_altmetric$number)),
                  baseline = baseline.centre) # log-transform baseline (base 2)
data <- list(score = analysis_ready_altmetric$altmetric)

## initial values
inits <- list(beta = 0.4,
              tau.person = 100,
              gamma = 0,
              r_int = rep(0, constants$P),
              tau = 1, 
              alpha = mean(analysis_ready_altmetric$altmetric)) # start with mean

# parameters to store
parms = c('r_int','tau','tau.person','beta','alpha','gamma', 'mu')

# models
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

# selected chains
index = which(colnames(mcmc_out$samples$chain1) %in% c('alpha','beta','gamma','tau.person'))
chain1 = mcmc_out$samples$chain1[,index]
chain2 = mcmc_out$samples$chain2[,index]
chains = list()
chains[[1]] = chain1
chains[[2]] = chain2

# extract summary
table = as.data.frame(mcmc_out$summary$all.chains) %>%
  tibble::rownames_to_column() %>%
  mutate(index = str_remove_all(rowname, '[A-Z|a-z]|\\[|\\]')) %>%
  separate(col=index, into=c('row','database'))

# add posterior p-values
pos = rbind(mcmc_out$samples$chain1, mcmc_out$samples$chain2) > 0
pos = colMeans(pos)
pos.dash = 1 - pos
pval = pmax(2*pmin(pos, pos.dash), 1/(2*MCMC))
table = bind_cols(table, pval)
names(table) = c('rowname','mean','median','sd','lower','upper','row','database','pvalue')

# random intercepts
random_intercept = filter(table, str_detect(rowname, pattern='^r_int'))

## predictions and residuals
fitted = filter(table, str_detect(rowname, pattern='^mu')) %>% # model predictions
  select(row, mean) %>%
  mutate(row = as.numeric(row))
#
residuals = mutate(analysis_ready_altmetric, 
                   row = 1:n(),
                   observed = altmetric) %>%
  left_join(fitted, by=c('row')) %>%
  mutate(residual = observed - mean) %>%
  select(number, row, year, years_since, observed, mean, residual)

# now remove from table
table = filter(table, 
               !str_detect(rowname, pattern='^r_int'),
               !str_detect(rowname, pattern='^mu')) %>%
  rename('parameter' = 'rowname')

## model fit
model_fit = data.frame(outcome = 'Altmetric', 
                model = 'Time fixed',
                WAIC = mcmc_out$WAIC$WAIC,
                pWAIC = mcmc_out$WAIC$pWAIC)

# save results as a list
results_altmetric = list()
results_altmetric$model_fit = model_fit
results_altmetric$table = table
results_altmetric$residuals = residuals
results_altmetric$chains = chains
