## 99_mcmc.R
# MCMC options for winbugs
# January 2022

debug = FALSE
n.chains = 2
thin = 5
MCMC = 10000
seeds = c(1234,5678) # one per chain

if(pilot==TRUE){
  debug = TRUE
  n.chains = 2
  thin = 1
  MCMC = 1000
  seeds = c(1234,5678) # one per chain
}
