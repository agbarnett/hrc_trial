# 99_nimble_funding.R
# using nimble to run Bayesian model of funding
# did not use offset or baseline

## model is way too uncertain ##

# August 2022
seed = TeachingDemos::char2seed('Colchester')
source('99_mcmc.R')

## define the model
code <- nimbleCode({
  for (i in 1:N) {
    total[i] ~ dnorm(mu[i], tau)
    log(mu[i]) <- alpha + r_int[person[i]] + (gamma*treatment[i])
  }
  # random intercept for each researcher - no need to centre
  for(j in 1:P){
    r_int[j] ~ dnorm(0, tau.person)
  }
  tau ~ dgamma(0.1, 0.1)
  tau.person ~ dgamma(0.1, 0.1)
  alpha ~ dnorm(0, sd = 10000)
  gamma ~ dnorm(0, sd = 10000)
})

## data
constants <- list(N = nrow(analysis_ready_funding),
                  treatment = as.numeric(analysis_ready_funding$funded == 'Funded'),
                  person = as.numeric(as.factor(analysis_ready_funding$number)),
                  P = length(unique(analysis_ready_funding$number)))
data <- list(total = analysis_ready_funding$funding + 0.01) # avoid zero

## initial values
inits <- list(tau.person = 100,
              tau = 1,
              gamma = 0,
              r_int = rep(0, constants$P),
              alpha = 0)

# parameters to store
parms = c('r_int','tau.person','alpha','gamma', 'mu','tau')

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

# convert chains to coda
mcmc = as.mcmc(mcmc_out$samples$chain1) # can only do one chain

# extract summary
table = as.data.frame(mcmc_out$summary$all.chains) %>%
  tibble::rownames_to_column() %>%
  mutate(index = str_remove_all(rowname, '[A-Z|a-z]|\\[|\\]')) %>%
  separate(col=index, into=c('row','database'))

# add posterior p-values
pos = mcmc_out$samples$chain1 > 0
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
residuals = mutate(analysis_ready_funding, 
                   row = 1:n(),
                   observed = funding) %>%
  left_join(fitted, by=c('row')) %>%
  mutate(obs = observed+0.01,
         residual = obs - mean) %>%
  select(number, row, year, years_since, obs, mean, residual)

# now remove from table
table = filter(table, 
               !str_detect(rowname, pattern='^r_int'),
               !str_detect(rowname, pattern='^mu')) %>%
  rename('parameter' = 'rowname')

## model fit
model_fit = data.frame(outcome = 'Funding', 
                model = 'Time fixed',
                WAIC = mcmc_out$WAIC$WAIC,
                pWAIC = mcmc_out$WAIC$pWAIC)

# save results as a list
results_funding = list()
results_funding$model_fit = model_fit
results_funding$table = table
results_funding$residuals = residuals
