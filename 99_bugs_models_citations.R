# 99_bugs_model_citations.R
# bugs model for citations counts; called by 4_model_?_bayes.R
# July 2022

# Model with:
# - random effect for calendar year (smoothed using CAR)
# - pre-randomisation baseline data that is the same across all three databases (I tried a varying model, but there was no evidence of difference)
bfile_citations_no_trt = 'bugs_citations_just_calendar.txt'
bugs = file(bfile_citations_no_trt, 'w')
cat('model
{
  for(i in 1:N){
      outcome[i] ~ dnorm(mu[i], tau)
      mu[i] <- int + calendar[year[i]] + beta*baseline[i]
  }
  # priors
  int ~ dnorm(0, 0.0001)
  beta ~ dnorm(0, 0.0001)
  # smoothed calendar effect
  calendar[1:Y] ~ car.normal(adj[], weights[], num[], tau.calendar)
  tau.calendar ~ dgamma(0.1, 0.1)
  tau ~ dgamma(0.1, 0.1)
}', file=bugs)
close(bugs)

## as above but with
# - treatment effect
bfile_citations = 'bugs_citations_with_treatment.txt'
bugs = file(bfile_citations, 'w')
cat('model
{
  for(i in 1:N){
      outcome[i] ~ dnorm(mu[i], tau)
      mu[i] <- int + calendar[year[i]] + beta*baseline[i] + gamma*treatment[i]
  }
  # priors
  int ~ dnorm(0, 0.0001)
  beta ~ dnorm(0, 0.0001)
  gamma ~ dnorm(0, 0.0001)
  # smoothed calendar effect
  calendar[1:Y] ~ car.normal(adj[], weights[], num[], tau.calendar)
  tau.calendar ~ dgamma(0.1, 0.1)
  tau ~ dgamma(0.1, 0.1)
}', file=bugs)
close(bugs)

## as above 
# - but with smoothed treatment effect over years
bfile_citations_treatment_car = 'bugs_model_with_treatment_car.txt'
bugs = file(bfile_citations_treatment_car, 'w')
cat('model
{
  for(i in 1:N){
      outcome[i] ~ dnorm(mu[i], tau)
      trt[i] <- (lambda[years_since[i]] + gamma)*treatment[i]
      control[i] <- delta[years_since[i]]
      mu[i] <- int  + beta*baseline[i] + control[i] + trt[i]
  }
  # priors
  int ~ dnorm(0, 0.0001)
  beta ~ dnorm(0, 0.0001)
  gamma ~ dnorm(0, 0.0001)
  # smoothed calendar effect
  calendar[1:Y] ~ car.normal(adj[], weights[], num[], tau.calendar)
  tau.calendar ~ dgamma(0.1, 0.1)
  tau ~ dgamma(0.1, 0.1)
  # smoothed time-varying effect for years since funding
  delta[1:T] ~ car.normal(adj_trt[], weights_trt[], num_trt[], tau.delta)
  tau.delta ~ dgamma(0.1, 0.1)
  # smoothed time-varying treatment effect
  lambda[1:T] ~ car.normal(adj_trt[], weights_trt[], num_trt[], tau.trt)
  tau.trt ~ dgamma(0.1, 0.1)
}', file=bugs)
close(bugs)


## as above 
# - using linear time interaction for treatment instead of smooth over time
bfile_citations_treatment_linear = 'bugs_model_with_treatment_linear.txt'
bugs = file(bfile_citations_treatment_linear, 'w')
cat('model
{
  for(i in 1:N){
      outcome[i] ~ dnorm(mu[i], tau)
      trt[i] <- (lambda*years_since[i] + gamma)*treatment[i]
      control[i] <- delta*years_since[i]
      mu[i] <- int + calendar[year[i]] + beta*baseline[i] + control[i] + trt[i]
  }
  # priors
  int ~ dnorm(0, 0.0001)
  beta ~ dnorm(0, 0.0001)
  gamma ~ dnorm(0, 0.0001)
  lambda ~ dnorm(0, 0.0001)
  delta ~ dnorm(0, 0.0001)
  # smoothed calendar effect
  calendar[1:Y] ~ car.normal(adj[], weights[], num[], tau.calendar)
  tau.calendar ~ dgamma(0.1, 0.1)
  tau ~ dgamma(0.1, 0.1)
}', file=bugs)
close(bugs)


