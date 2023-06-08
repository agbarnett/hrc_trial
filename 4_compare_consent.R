# 4_compare_consent.R
# compare consent and non-consent for 1) paper counts, 2) years of experience
# results used by 4_summary_data.Rmd
# March 2022
source('99_functions.R')
library(tidyverse)
library(R2WinBUGS)

## get the researcher data
load('data/0_researchers_randomised.RData') # from 0_read_data_randomised.R
load('data/2_biblio_data_scopus.RData')
load('data/2_biblio_data_scholar.RData')
load('data/2b_biblio_data_researchgate.RData')
# decided not to use ORCID, too hard to know if it is being updated or not
#load('data/2_biblio_data_orcid.RData')
#frame_orcid = filter(frame_orcid, 
#                     type != 'patent', # do not include patents
#                     works_available == 'Yes') # only yes for macro below

# get whether they consented in every year
researchers_u = select(researchers, number, year, consent) %>% unique() # unique because some researchers won multiple grants in the same year
N = nrow(researchers_u)

## get the paper data and years of experience
all_data = NULL
for (k in 1:N){
  # orcid - turned off
  #count_orcid = count_papers(in_frame = frame_orcid, 
  #                     in_papers = papers_orcid, 
  #                     in_number = researchers_u$number[k], # researcher number
  #                     in_year = researchers_u$year[k])
  #count_orcid = bind_cols(count_orcid, researchers_u[k,]) %>%# add key information
  #  mutate(database = 'orcid')
  #all_data = bind_rows(all_data, count_orcid)
  # scopus
  count_scopus = count_papers(in_frame = frame_scopus, 
               in_papers = papers_scopus, 
               in_number = researchers_u$number[k], # researcher number
               in_year = researchers_u$year[k])
  count_scopus = bind_cols(count_scopus, researchers_u[k,]) %>%# add key information
    mutate(database = 'scopus')
  all_data = bind_rows(all_data, count_scopus)
  # scholar
  count_scholar = count_papers(in_frame = frame_scholar, 
                              in_papers = papers_scholar, 
                              in_number = researchers_u$number[k], # researcher number
                              in_year = researchers_u$year[k])
  count_scholar = bind_cols(count_scholar, researchers_u[k,]) %>%# add key information
    mutate(database = 'scholar')
  all_data = bind_rows(all_data, count_scholar)
  # researchgate
  count_rgate = count_papers(in_frame = frame_researchgate, 
                               in_papers = papers_researchgate, 
                               in_number = researchers_u$number[k], # researcher number
                               in_year = researchers_u$year[k])
  count_rgate = bind_cols(count_rgate, researchers_u[k,]) %>%# add key information
    mutate(database = 'researchgate')
  all_data = bind_rows(all_data, count_rgate)
}
all_data = mutate(all_data, 
                  ydiff = year - miny) # number of years of experience

### Model 1, paper numbers ###
pilot = FALSE
source('99_mcmc.R')

## transform to wide for multivariate model
wide = select(all_data, number, consent, year, database, n) %>%
  group_by(number, year) %>%
  pivot_wider(values_from = 'n', names_from = 'database') %>%
  ungroup() %>% 
  arrange(number)

# write the code
bfile = 'bugs_model.txt'
bugs = file(bfile, 'w')
cat('model
{
  for(i in 1:N){
      log_counts[i,1:C] ~ dmnorm(mu[i,1:C], Omega[1:C, 1:C])
      mu[i,1] <- alpha[1] + beta*consent[i] + random[number[i]]
      mu[i,2] <- alpha[2] + beta*consent[i] + random[number[i]]
      mu[i,3] <- alpha[3] + beta*consent[i] + random[number[i]]
  }
  for(j in 1:M){ # random intercept
    random[j] ~ dnorm(0, tau.random)
  }
  for(j in 1:C){
    alpha[j] ~ dnorm(0, 0.0001)
  }
  # priors
  tau.random ~ dgamma(0.1, 0.1)
  beta ~ dnorm(0, 0.0001)
  Omega[1:C, 1:C] ~ dwish(R[1:C,1:C],C)
  Sigma[1:C, 1:C] <- inverse(Omega[1:C, 1:C])
}', file=bugs)
close(bugs)

# prepare data
wide = arrange(wide, number, year)
N = nrow(wide)
M = length(unique(wide$number)) # number of researchers
matrix_count = as.matrix(select(wide, scopus, scholar, researchgate))
C = ncol(matrix_count) # number of databases
R = toeplitz(c(1, rep(0, C-1))) # for correlation
bdata = list(N = N, 
             C = C, 
             M = M,
             R = R,
             number = wide$number,
             consent = as.numeric(wide$consent == 'Yes'),
             log_counts = log(matrix_count + 1) # log-transform
             )
# initial values
inits = list(tau.random=10, random=rep(0, M), beta=0, alpha=rep(0,C), Omega=R)  # start all with no flag for mean or variance
inits = rep(list(inits), n.chains) # repeat per chains

# run
parms = c('beta','alpha','Sigma','random','tau.random') 
bugs_res = bugs(data=bdata, inits=inits, parameters=parms, model.file=bfile, DIC=FALSE,
                n.chains=n.chains, n.iter=MCMC*thin*2, n.thin=thin, bugs.seed=seed, debug=debug,
                bugs.directory="c:/Program Files/WinBUGS14")

# make table of results
table = bugs_res$summary[,c(1,3,7)]
table = data.frame(table)
names(table) = c('mean','lower','upper')
table$parameter = row.names(table)
table$parameter[1:4] = c('consent','scopus','scholar','researchgate')

## p-values
pos = bugs_res$sims.matrix>0
pos = colMeans(pos)
pos.dash = 1 - pos
pvals = pmax(2*pmin(pos, pos.dash), 1/(2*MCMC))
table$pvalue = pvals

# extract correlation matrix
matrix = filter(table, str_detect(parameter, pattern='Sigma')) 
varcov = matrix(matrix$mean, nrow=C)
cormat = cov2cor(varcov)

# run standard model as check
#model = glm(n ~ consent, data=all_data, family='poisson')
#summary(model)

### residual check for model 1
# re-run model from previous start
# initial values
bugs_res$last.values[[1]]$Omega = solve(bugs_res$last.values[[1]]$Sigma) # inverse
bugs_res$last.values[[2]]$Omega = solve(bugs_res$last.values[[2]]$Sigma) # inverse
bugs_res$last.values[[1]]$Sigma = NULL
bugs_res$last.values[[2]]$Sigma = NULL
inits = bugs_res$last.values  # from previous
parms = c('mu')  # just mu
bugs_res_residuals = bugs(data=bdata, inits=inits, parameters=parms, model.file=bfile, DIC=FALSE,
                n.chains=n.chains, n.iter=MCMC*thin*2, n.thin=thin, bugs.seed=seed, debug=debug,
                bugs.directory="c:/Program Files/WinBUGS14")
# calculate residual (some missing)
res = bugs_res_residuals$mean$mu - bdata$log_counts %>%
  as.data.frame()
residuals = mutate(res, number = 1:n()) %>% # add researcher number
     pivot_longer(cols= -'number', values_to='res', names_to = 'database')

### Model 2, years of experience ###
source('99_mcmc.R')

# write the code
bfile = 'bugs_model_year.txt'
bugs = file(bfile, 'w')
cat('model
{
  for(i in 1:N){
      diff[i] ~ dnorm(mu[i], tau)
      mu[i] <- alpha + beta*consent[i] + random[number[i]]
  }
  for(j in 1:M){ # random intercept
    random[j] ~ dnorm(0, tau.random)
  }
  # priors
  tau.random ~ dgamma(0.1, 0.1)
  tau ~ dgamma(0.1, 0.1)
  alpha ~ dnorm(0, 0.0001)
  beta ~ dnorm(0, 0.0001)
}', file=bugs)
close(bugs)

# prepare data
for_model = filter(all_data, !is.na(ydiff)) %>%
  select(number, ydiff, consent) %>%
  unique()
N = nrow(for_model)
M = length(unique(for_model$number)) # number of researchers
bdata = list(N = N, 
             M = M,
             number = for_model$number,
             consent = as.numeric(for_model$consent == 'Yes'),
             diff = log(for_model$ydiff+1)) # Log-transform improves chains 
# initial values
inits = list(tau.random=10, tau=10, random=rep(0, M), beta=0, alpha=0)  # start all with no flag for mean or variance
inits = rep(list(inits), n.chains) # repeat per chains

# run
parms = c('beta','alpha','tau','random','tau.random')
bugs_res_year = bugs(data=bdata, inits=inits, parameters=parms, model.file=bfile, DIC=FALSE,
                n.chains=n.chains, n.iter=MCMC*thin*2, n.thin=thin, bugs.seed=seed, debug=debug,
                bugs.directory="c:/Program Files/WinBUGS14")

# make table of results
year_table = bugs_res_year$summary[,c(1,3,7)]
year_table = data.frame(year_table)
names(year_table) = c('mean','lower','upper')
year_table$parameter = row.names(year_table)

## p-values
pos = bugs_res_year$sims.matrix>0
pos = colMeans(pos)
pos.dash = 1 - pos
pvals = pmax(2*pmin(pos, pos.dash), 1/(2*MCMC))
year_table$pvalue = pvals

### save
# reduce size of saved results
bugs_res$sims.array = NULL
bugs_res$sims.list = NULL
bugs_res$last.values = NULL
bugs_res_year$sims.array = NULL
bugs_res_year$sims.list = NULL
bugs_res_year$last.values = NULL
#
outfile = 'results/compare_consent_bayes.RData'
save(bugs_res, bugs_res_year, cormat, year_table, table, residuals, file = outfile)

