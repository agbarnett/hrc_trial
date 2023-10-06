# 99_nimble_citations.R
# using nimble to run Bayesian model of citation counts
# no need for offset as fewer published papers cancel fewer citations
# September 2023
seed = rep(0, 2)
seed[1] = TeachingDemos::char2seed('Colchester')
seed[2] = TeachingDemos::char2seed('Port Vale')
source('99_mcmc.R')

# converging not working, trying to remove one influential person. Answer = does not help
analysis_ready_citations = filter(analysis_ready_citations, number !=51)

## define the model
code <- nimbleCode({
  for (i in 1:N) {
    counts[i] ~ dpois(mu[i])
    log(mu[i]) <- alpha + r_int[person[i]] + (beta*baseline[i]) + (gamma*treatment[i])
  }
  # random intercept for each researcher - no need to centre
  for(j in 1:P){
    r_int[j] ~ dnorm(0, tau.person)
  }
  tau.person ~ dgamma(0.001, 0.001)
  alpha ~ dnorm(0, sd = 10000)
  beta ~ dnorm(0, sd = 10000)
  gamma ~ dnorm(0, sd = 10000)
})

## data
# centre baseline for mixing
baseline = log(analysis_ready_citations$b_scopus + 1) # log-transform baseline (base 2)
baseline.center = baseline - median(baseline)
#
constants <- list(N = nrow(analysis_ready_citations),
                  treatment = as.numeric(analysis_ready_citations$funded == 'Funded'),
                  person = as.numeric(as.factor(analysis_ready_citations$number)),
                  P = length(unique(analysis_ready_citations$number)),
                  baseline = baseline.center) 
data <- list(counts = analysis_ready_citations$scopus)

## initial values
inits <- list(beta = 0.4, # relatively sensible start
              tau.person = 5,
              gamma = 0,
              r_int = rep(0, constants$P),
              alpha = mean(log(analysis_ready_citations$scopus+1))) # start at mean

# parameters to store
parms = c('r_int','tau.person','beta','alpha','gamma', 'mu')

# models
model <- nimbleModel(code, 
                    data = data, 
                    inits = inits, 
                    constants = constants)

# change updater because default options have poor mixing and strong correlation between alpha and gamma
# for examples see https://groups.google.com/g/nimble-users/c/k63xGcuNbpc?pli=1
thin = 10 # to reduce autocorrelation
mcmcConf <- configureMCMC(model,
                          onlyRW = TRUE,
                          onlySlice = FALSE,
                          monitors = parms,
                          thin = thin,
                          enableWAIC = TRUE,
                          autoBlock = TRUE)
Rmcmc <- buildMCMC(mcmcConf)
Cmodel <- compileNimble(model)
Cmcmc <- compileNimble(Rmcmc, project = model)
MCMC = 10000
samples.first <- runMCMC(Cmcmc, 
                         niter = MCMC*2*thin, # times 2 for burn-in ,
                         inits = inits,
                         nburnin = MCMC,
                         thin = thin, 
                         nchains = n.chains,
                         setSeed = seed,
                         summary = TRUE, 
                         WAIC = FALSE)

# MCMC samples - standard ..
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
residuals = mutate(analysis_ready_citations, 
                   row = 1:n(),
                   observed = scopus) %>%
  left_join(fitted, by=c('row')) %>%
  mutate(residual = observed - mean) %>%
  select(number, row, year, years_since, observed, mean, residual)

# now remove from table
table = filter(table, 
               !str_detect(rowname, pattern='^r_int'),
               !str_detect(rowname, pattern='^mu')) %>%
  rename('parameter' = 'rowname')

## model fit
model_fit = data.frame(outcome = 'Citations', 
                model = 'Time fixed',
                WAIC = mcmc_out$WAIC$WAIC,
                pWAIC = mcmc_out$WAIC$pWAIC)

# save results as a list
results_citations = list()
results_citations$model_fit = model_fit
results_citations$table = table
results_citations$residuals = residuals
results_citations$chains = chains
