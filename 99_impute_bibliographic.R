# 99_impute_bibliographic.R
# impute missing bibliographic data from 3 databases using a multivariate normal
# separate models for baseline and yearly counts
# Sep 2023
seeds = rep(0,2)
seeds[1] = TeachingDemos::char2seed('Barnsley') # one per chain
seeds[2] = TeachingDemos::char2seed('Exeter')

## Bayesian model with multi-variate normal ##
# same code for baseline and yearly numbers (data is different though)
code <- nimbleCode({
  for (i in 1:N) {
    for(j in 1:C){
      log_counts[i, j] ~ dnorm(mean = mu[i, j], tau)
      mu[i,j] <- alpha[j] + errors[i,j]
    }
    errors[i, 1:C] ~ dmnorm(zeros[1:C], cov = Sigma[1:C, 1:C])
  }
  # intercept for each database
  for(j in 1:C){
    alpha[j] ~ dnorm(0, sd = 1000)
  }
  tau ~ dgamma(0.1, 0.1) # overall precision
  Sigma[1:C, 1:C] ~ dinvwish(R[1:C, 1:C], C)
})

## data
# a) baseline
biblio = select(analysis_ready, number, starts_with('b_')) %>%
  unique() %>%
  arrange(number) 
biblio_numbers = pull(biblio, number)
biblio = select(biblio, b_scholar, b_scopus, b_researchgate) %>% # this ordering must be remembered
  as.matrix() 
#plot(log(biblio[,1]+1),log(biblio[,2]+1)) # quick graphical check
# b) yearly
biblio_yearly = select(analysis_ready, number, year, scholar, scopus, researchgate) %>%
  arrange(number, year) 
biblio_index_yearly = select(biblio_yearly, number, year)
biblio_yearly = select(biblio_yearly, scholar, scopus, researchgate) %>% # this ordering must be remembered
  as.matrix() 

## constants and data
R = toeplitz(c(1,0,0))
# a) baseline
constants <- list(N = nrow(biblio),
                        R = R,
                        zeros = rep(0,3), 
                        C = 3)
data <- list(log_counts = log(biblio + 1)) # log transform for better fit
# b) yearly
constants_yearly <- list(N = nrow(biblio_yearly),
                  R = R,
                  zeros = rep(0,3), 
                  C = 3)
data_yearly <- list(log_counts = log(biblio_yearly + 1)) # log transform for better fit

## initial values
inits <- list(Sigma = R,
                    alpha = c(0,0,0))

# parameters to store
parms = c('Sigma', 'mu')

## models
# a) baseline
model <- nimbleModel(code, 
                     data = data, 
                     inits = inits, 
                     constants = constants)
# b) yearly
model_yearly <- nimbleModel(code, 
                     data = data_yearly, 
                     inits = inits, 
                     constants = constants_yearly)

# MCMC samples
pilot = FALSE
source('99_mcmc.R')
# a) baseline
mcmc_out <- nimbleMCMC(model = model,
                       inits = inits,
                       monitors = parms,
                       niter = MCMC*2*thin, # times 2 for burn-in 
                       thin = thin,
                       nchains = n.chains, 
                       nburnin = MCMC,
                       summary = TRUE, 
                       setSeed = seeds,
                       WAIC = FALSE)
# b) yearly
mcmc_out_yearly <- nimbleMCMC(model = model_yearly,
                       inits = inits,
                       monitors = parms,
                       niter = MCMC*2*thin, # times 2 for burn-in 
                       thin = thin,
                       nchains = n.chains, 
                       nburnin = MCMC,
                       summary = TRUE, 
                       setSeed = seeds,
                       WAIC = FALSE)

## get summary stats
# a) baseline
table = as.data.frame(mcmc_out$summary$all.chains) %>%
  tibble::rownames_to_column() %>%
  mutate(index = str_remove_all(rowname, '[A-Z|a-z]|\\[|\\]')) %>%
  separate(col=index, into=c('row','column'))
names(table) = c('rowname','mean','median','sd','lower','upper','row','column')
# b) yearly
table_yearly = as.data.frame(mcmc_out_yearly$summary$all.chains) %>%
  tibble::rownames_to_column() %>%
  mutate(index = str_remove_all(rowname, '[A-Z|a-z]|\\[|\\]')) %>%
  separate(col=index, into=c('row','column'))
names(table_yearly) = c('rowname','mean','median','sd','lower','upper','row','column')

## variance-covariance matrix & correlation
# a) baseline
covmat = filter(table, str_detect(rowname, pattern='^Sigma')) %>%
  select(row, column, mean) %>%
  pivot_wider(values_from = "mean", names_from='column')
cormat = cov2cor(select(covmat, -row)%>% as.matrix()) # check correlation
colnames(cormat) = rownames(cormat) = c('scholar', 'scopus', 'researchgate')
# b) yearly
covmat_yearly = filter(table_yearly, str_detect(rowname, pattern='^Sigma')) %>%
  select(row, column, mean) %>%
  pivot_wider(values_from = "mean", names_from='column')
cormat_yearly = cov2cor(select(covmat_yearly, -row)%>% as.matrix()) # check correlation
colnames(cormat_yearly) = rownames(cormat_yearly) = c('scholar', 'scopus', 'researchgate')

## now select imputations from chains (just using first chain)
# a) baseline
missed = which(is.na(biblio), arr.ind=TRUE) # index of missing values
names = colnames(mcmc_out$samples$chain1) # parameter names
n_samples = dim(mcmc_out$samples$chain1)[1] # number of MCMC samples
sample_index = round(seq(1, n_samples, length.out=n_impute)) # points in the chain at which to take imputations
M = nrow(missed) # total number missing
just_imputed = NULL
for (k in 1:M){ # loop through missing
  this_name = paste('mu[', missed[k,1], ', ', missed[k,2], ']', sep='') # find column corresponding to this chain
  index = which(names == this_name)
  frame = data.frame(number = biblio_numbers[missed[k,1]], # use researcher number from original data
             database = missed[k,2],
             imp = 1:n_impute,
             log_counts = mcmc_out$samples$chain1[sample_index,index]) %>%
    mutate(counts = exp(log_counts) - 1, # back-transform
           counts = round(counts),
           counts = pmax(0, counts)) # tiny number of negative imputations
  just_imputed = bind_rows(just_imputed, frame) # can ignore warnings about row names
}
# b) yearly
missed = which(is.na(biblio_yearly), arr.ind=TRUE) # index of missing values
names = colnames(mcmc_out_yearly$samples$chain1) # parameter names
n_samples = dim(mcmc_out_yearly$samples$chain1)[1] # number of MCMC samples
sample_index = round(seq(1, n_samples, length.out=n_impute)) # points in the chain at which to take imputations
M = nrow(missed) # total number missing
just_imputed_yearly = NULL
for (k in 1:M){ # loop through missing
  this_name = paste('mu[', missed[k,1], ', ', missed[k,2], ']', sep='') # find column corresponding to this chain
  index = which(names == this_name)
  frame = data.frame(number = biblio_index_yearly$number[missed[k,1]], # use researcher number from original data
                     year = biblio_index_yearly$year[missed[k,1]], # use researcher year from original data
                     database = missed[k,2],
                     imp = 1:n_impute,
                     log_counts = mcmc_out_yearly$samples$chain1[sample_index,index]) %>%
    mutate(counts = exp(log_counts) - 1, # back-transform
           counts = round(counts),
           counts = pmax(0, counts)) # tiny number of negative imputations
  just_imputed_yearly = bind_rows(just_imputed_yearly, frame) # can ignore warnings about row names
}

## make imputed data
analysis_ready_imputed = list()
for (k in 1:n_impute){
  imputed = analysis_ready # start with complete case data
  
  ## fill in data using merge
  # a) baseline
  this_impute = filter(just_imputed, imp == k) %>% # select the imputation number
    select(-imp, -log_counts) %>%
    pivot_wider(names_from = 'database', values_from = 'counts') %>%
    rename('i_scholar' = `1`, # from ordering that must be remembered
           'i_scopus' = `2`,
           'i_researchgate' = `3`)
  # merge
  imputed = left_join(imputed, this_impute, by='number') %>%
    mutate(b_scholar = coalesce(b_scholar, i_scholar),
           b_scopus = coalesce(b_scopus, i_scopus),
           b_researchgate = coalesce(b_researchgate, i_researchgate)) %>%
    select(-starts_with('i_')) # no longer needed

  # b) yearly
  this_impute = filter(just_imputed_yearly, imp == k) %>% # select the imputation number
    select(-imp, -log_counts) %>%
    pivot_wider(names_from = 'database', values_from = 'counts') %>%
    rename('i_scholar' = `1`, # from ordering that must be remembered
           'i_scopus' = `2`,
           'i_researchgate' = `3`)
  # merge
  imputed = left_join(imputed, this_impute, by=c('number','year')) %>%
    mutate( scholar = coalesce(scholar, i_scholar),
            scopus = coalesce(scopus, i_scopus),
            researchgate = coalesce(researchgate, i_researchgate)) %>%
    select(-starts_with('i_')) # no longer needed
  
  # fill in missing denominators
  imputed = mutate(imputed,
                   denom_scholar = case_when(
                   year < 2022 ~ 1,
                   year == 2022 ~ 0.537
                   ),
                   denom_scopus = case_when(
                     year < 2022 ~ 1,
                     year == 2022 ~ 0.537
                   ),
                   denom_researchgate = case_when(
                     year < 2022 ~ 1,
                     year == 2022 ~ 0.219
                   )
  )
                   
  # concatenate to list
  analysis_ready_imputed[[k]] = imputed
}

# save
save(analysis_ready_imputed, cormat, n_impute, file='data/5_imputed.RData')
