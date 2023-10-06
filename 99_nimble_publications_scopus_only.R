# 99_nimble_publications_scopus_only.R
# using nimble to run Bayesian model of publication counts
# sensitivity analysis using Scopus only
# October 2023
seed = rep(0, 2)
seed[1] = TeachingDemos::char2seed('Colchester')
seed[2] = TeachingDemos::char2seed('Port Vale')
source('99_mcmc.R')

## remove those without scopus 
non_missing = filter(analysis_ready_imputed[[imp]], !is.na(scopus))

## define the model
code <- nimbleCode({
  for (i in 1:N) {
    counts[i] ~ dpois(mu[i])
    log(mu[i]) <- log(offset[i]) + alpha + r_int[person[i]] + (beta*baseline[i]) + (gamma*treatment[i]) 
  }
  # intercept
  alpha ~ dnorm(0, sd = 10000) 
  # random intercept for each researcher - no need to centre
  for(j in 1:P){
    r_int[j] ~ dnorm(0, tau.person)
  }
  tau.person ~ dgamma(0.01, 0.01)
  beta ~ dnorm(0, sd = 10000)
  gamma ~ dnorm(0, sd = 10000)
})

## data
# matrix of baseline
baseline = log2(non_missing$b_scopus + 0.5)
# centre baseline counts for better convergence
median.baseline = median(baseline, na.rm=TRUE)
baseline = baseline - median.baseline

## constants and data
constants <- list(N = nrow(non_missing),
                  offset = non_missing$denom_scopus, # adjust for final year that is under a year
                  treatment = as.numeric(non_missing$funded == 'Funded'),
                  baseline = baseline,
                  person = as.numeric(as.factor(non_missing$number)),
                  P = length(unique(non_missing$number)))
data <- list(counts = non_missing$scopus)

## initial values
inits <- list(beta = 0,
              tau.person = 100,
              r_int = rep(0, constants$P),
              gamma = 0,
              alpha = 0)

# parameters to store
parms = c('tau.person','beta','alpha','gamma','mu')

# models
model <- nimbleModel(code, 
                     data = data, 
                     inits = inits, 
                     constants = constants)

# MCMC samples for model with 
mcmc_out_ <- nimbleMCMC(model = model,
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
index = which(colnames(mcmc_out_$samples$chain1) %in% c('alpha','beta','gamma','tau.person'))
chain1 = mcmc_out_$samples$chain1[,index]
chain2 = mcmc_out_$samples$chain2[,index]
chains = list()
chains[[1]] = chain1
chains[[2]] = chain2

# extract summary
table = as.data.frame(mcmc_out_$summary$all.chains) %>%
  tibble::rownames_to_column() %>%
  mutate(index = str_remove_all(rowname, '[A-Z|a-z]|\\[|\\]')) %>%
  separate(col=index, into=c('row','database'))

# add posterior p-values
pos = rbind(mcmc_out_$samples$chain1, mcmc_out_$samples$chain2) > 0
pos = colMeans(pos)
pos.dash = 1 - pos
pval = pmax(2*pmin(pos, pos.dash), 1/(2*MCMC))
table = bind_cols(table, pval)
names(table) = c('rowname','mean','median','sd','lower','upper','row','database','pvalue')

## predictions and residuals
fitted = filter(table, str_detect(rowname, pattern='^mu')) %>% # model predictions
  select(row, mean) %>%
  mutate(row = as.numeric(row))
#
residuals = mutate(non_missing, 
                   row = 1:n(),
                   scopus = scopus / denom_scopus # scale paper counts by database-specific denominator to adjust for final year of data collection not being a full year
      ) %>%
  left_join(fitted, by=c('row')) %>%
  mutate(residual = scopus - mean)

# now remove from table
table = filter(table, !str_detect(rowname, pattern='^mu')) %>%
  rename('parameter' = 'rowname')

## model fit
model_fit = data.frame(outcome = 'Scopus only', 
                model = 'Time fixed, with ',
                WAIC = mcmc_out_$WAIC$WAIC,
                pWAIC = mcmc_out_$WAIC$pWAIC)

# save results as a list
results_scopus_only = list()
results_scopus_only$model_fit = model_fit
results_scopus_only$table = table
results_scopus_only$residuals = residuals
results_scopus_only$chains = chains
