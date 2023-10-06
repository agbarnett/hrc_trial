# 99_impute_bibliographic_only_baseline.R
# impute missing bibliographic data from 3 databases using a multivariate normal
# version for baseline counts only
# Sep 2023
seeds = rep(0,2)
seeds[1] = TeachingDemos::char2seed('Barnsley') # one per chain
seeds[2] = TeachingDemos::char2seed('Exeter')
n_impute = 10

## Bayesian model with multi-variate normal ##
# same code for baseline and yearly numbers (data is different though)
code <- nimbleCode({
  for (i in 1:N) {
    for(j in 1:C){
      counts[i, j] ~ dpois(mu[i, j])
      log(mu[i,j]) <- alpha[j] + errors[i,j]
    }
    errors[i, 1:C] ~ dmnorm(zeros[1:C], cov = Sigma[1:C, 1:C])
  }
  # intercept for each database
  for(j in 1:C){
    alpha[j] ~ dnorm(0, sd = 1000)
  }
  Sigma[1:C, 1:C] ~ dinvwish(R[1:C, 1:C], C)
})

## data
biblio = select(analysis_ready, number, starts_with('b_')) %>%
  unique() %>%
  arrange(number) 
biblio_numbers = pull(biblio, number)
biblio = select(biblio, b_scholar, b_scopus, b_researchgate) %>% # this ordering must be remembered
  as.matrix() 
#plot(log(biblio[,1]+1),log(biblio[,2]+1)) # quick graphical check

## constants and data
R = toeplitz(c(1,0,0))
constants <- list(N = nrow(biblio),
                  R = R,
                  zeros = rep(0,3), 
                  C = 3)
data <- list(counts = biblio)

## initial values
inits <- list(Sigma = R,
              alpha = c(0,0,0))

# parameters to store
parms = c('Sigma', 'mu')

## models
model <- nimbleModel(code, 
                     data = data, 
                     inits = inits, 
                     constants = constants)

# MCMC samples
pilot = FALSE
source('99_mcmc.R')
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

## get summary stats
table = as.data.frame(mcmc_out$summary$all.chains) %>%
  tibble::rownames_to_column() %>%
  mutate(index = str_remove_all(rowname, '[A-Z|a-z]|\\[|\\]')) %>%
  separate(col=index, into=c('row','column'))
names(table) = c('rowname','mean','median','sd','lower','upper','row','column')

## variance-covariance matrix & correlation
covmat = filter(table, str_detect(rowname, pattern='^Sigma')) %>%
  select(row, column, mean) %>%
  pivot_wider(values_from = "mean", names_from='column')
cormat = cov2cor(select(covmat, -row)%>% as.matrix()) # check correlation
colnames(cormat) = rownames(cormat) = c('scholar', 'scopus', 'researchgate')

## now select imputations from chains (just using first chain)
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
                     counts = round(mcmc_out$samples$chain1[sample_index,index])) # round to counts 
  just_imputed = bind_rows(just_imputed, frame) # can ignore warnings about row names
}

## make imputed data
analysis_ready_imputed = analysis_ready_monthly_imputed = list()
for (k in 1:n_impute){
  imputed = analysis_ready # start with complete case data
  imputed_monthly = analysis_ready_monthly # start with complete case data
  
  ## fill in data using merge
  this_impute = filter(just_imputed, imp == k) %>% # select the imputation number
    select(-imp) %>%
    pivot_wider(names_from = 'database', values_from = 'counts') %>%
    rename('i_scholar' = `1`, # from ordering that must be remembered
           'i_scopus' = `2`,
           'i_researchgate' = `3`)
  ## merge
  # a) annual
  imputed = left_join(imputed, this_impute, by='number') %>% # join by researcher
    mutate(b_scholar = coalesce(b_scholar, i_scholar),
           b_scopus = coalesce(b_scopus, i_scopus),
           b_researchgate = coalesce(b_researchgate, i_researchgate)) %>%
    select(-starts_with('i_')) # no longer needed
  # b) monthly
  imputed_monthly = left_join(imputed_monthly, this_impute, by='number') %>%
    mutate(b_scopus = coalesce(b_scopus, i_scopus),
           b_researchgate = coalesce(b_researchgate, i_researchgate)) %>%
    select(-starts_with('i_')) # no longer needed
  
  ## fill in missing denominators
  # a) annual
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
                   ))
   # b) monthly
   imputed_monthy = mutate(imputed_monthly,
                   denom_scopus = case_when(
                     month == 1 ~ 1.03,
                     month == 2 ~ 0.933,
                     month == 3 ~ 0.967,
                     month == 4 ~ 1,
                     month == 5 ~ 1.03,
                     month == 6 ~ 1,
                     month == 7 ~ 1.03,
                     month == 8 ~ 1.03,
                     month == 9 ~ 1,
                     month == 10 ~ 1.03,
                     month == 11 ~ 1,
                     month == 12 ~ 1.03
                   ),
                   denom_researchgate = case_when(
                     month == 1 ~ 1.03,
                     month == 2 ~ 0.933,
                     month == 3 ~ 0.967,
                     month == 4 ~ 1,
                     month == 5 ~ 1.03,
                     month == 6 ~ 1,
                     month == 7 ~ 1.03,
                     month == 8 ~ 1.03,
                     month == 9 ~ 1,
                     month == 10 ~ 1.03,
                     month == 11 ~ 1,
                     month == 12 ~ 1.03
                   ))

  # concatenate to list
  analysis_ready_imputed[[k]] = imputed
  analysis_ready_monthly_imputed[[k]] = imputed_monthy
}

# save
save(analysis_ready_imputed, analysis_ready_monthly_imputed, cormat, n_impute, file='data/5_imputed.RData')
