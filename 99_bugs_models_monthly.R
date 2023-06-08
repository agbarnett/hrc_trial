# 99_bugs_model_monthly.R
# bugs model for paper counts; called by 4_model_bibliographic_bayes.R
# versions for monthly data with january effect
# July 2022

# Model with:
# - random intercept per participant
# - random effect for calendar year (smoothed using CAR)
# - pre-randomisation baseline data that is the same across all three databases (I tried a varying model, but there was no evidence of difference)
bfile_calendar_month = 'bugs_model_just_calendar_month.txt'
bugs = file(bfile_calendar_month, 'w')
cat('model
{
  for(i in 1:N){
      log_counts[i,1:C] ~ dmnorm(mu[i,1:C], Omega[1:C, 1:C])
      standard[i] <- int + r_int_c[person[i]] + calendar[year[i]] + zeta*january[i]
      for(j in 1:C){
        mu[i,j] <- alpha_c[j] + beta*baseline[i,j] + standard[i]
      }
      baseline[i,1:C] ~ dmnorm(zeros[1:C], Omega.b[1:C, 1:C]) # for missing
  }
  # priors
  int ~ dnorm(0, 0.0001)
  beta ~ dnorm(0, 0.0001)
  zeta ~ dnorm(0, 0.0001)
  # random intercept for each researcher
  for(j in 1:P){
    r_int[j] ~ dnorm(0, tau.person)
    r_int_c[j] <- r_int[j] - mean.person
  }
  mean.person <- mean(r_int[1:P])
  tau.person ~ dgamma(0.1, 0.1)
  # random intercept for each database
  for(j in 1:C){
    alpha[j] ~ dnorm(0, 0.0001)
    alpha_c[j] <- alpha[j] - mean.alpha
  }
  mean.alpha <- mean(alpha[1:C])
  # smoothed calendar effect
  calendar[1:Y] ~ car.normal(adj[], weights[], num[], tau.calendar)
  tau.calendar ~ dgamma(0.1, 0.1)
  # correlated errors
  Omega.b[1:C, 1:C] ~ dwish(R[1:C, 1:C], C) # for missing baseline
  Omega[1:C, 1:C] ~ dwish(R[1:C, 1:C], C)
  Sigma[1:C, 1:C] <- inverse(Omega[1:C, 1:C])
}', file=bugs)
close(bugs)

## Using this model as random intercept model (above) had convergence issues
# Model with:
# - random effect for calendar year (smoothed using CAR)
# - pre-randomisation baseline data that is the same across all three databases (I tried a varying model, but there was no evidence of difference)
bfile_calendar_month = 'bugs_model_just_calendar_month.txt'
bugs = file(bfile_calendar_month, 'w')
cat('model
{
  for(i in 1:N){
      log_counts[i,1:C] ~ dmnorm(mu[i,1:C], Omega[1:C, 1:C])
      standard[i] <- int + calendar[year[i]] + zeta*january[i]
      for(j in 1:C){
        mu[i,j] <- alpha_c[j] + beta*baseline[i,j] + standard[i]
      }
      baseline[i,1:C] ~ dmnorm(zeros[1:C], Omega.b[1:C, 1:C]) # for missing
  }
  # priors
  int ~ dnorm(0, 0.0001)
  beta ~ dnorm(0, 0.0001)
  zeta ~ dnorm(0, 0.0001)
  # random intercept for each database
  for(j in 1:C){
    alpha[j] ~ dnorm(0, 0.0001)
    alpha_c[j] <- alpha[j] - mean.alpha
  }
  mean.alpha <- mean(alpha[1:C])
  # smoothed calendar effect
  calendar[1:Y] ~ car.normal(adj[], weights[], num[], tau.calendar)
  tau.calendar ~ dgamma(0.1, 0.1)
  # correlated errors
  Omega.b[1:C, 1:C] ~ dwish(R[1:C, 1:C], C) # for missing baseline
  Omega[1:C, 1:C] ~ dwish(R[1:C, 1:C], C)
  Sigma[1:C, 1:C] <- inverse(Omega[1:C, 1:C])
}', file=bugs)
close(bugs)

## as above but with
# - treatment effect
bfile_calendar_treatment_month = 'bugs_model_with_treatment_month.txt'
bugs = file(bfile_calendar_treatment_month, 'w')
cat('model
{
  for(i in 1:N){
      log_counts[i,1:C] ~ dmnorm(mu[i,1:C], Omega[1:C, 1:C])
      standard[i] <- int + calendar[year[i]] + gamma*treatment[i] + zeta*january[i]
      for(j in 1:C){
        mu[i,j] <- alpha_c[j] + beta*baseline[i,j] + standard[i]
      }
      baseline[i,1:C] ~ dmnorm(zeros[1:C], Omega.b[1:C, 1:C]) # for missing
  }
  # priors
  int ~ dnorm(0, 0.0001)
  beta ~ dnorm(0, 0.0001)
  zeta ~ dnorm(0, 0.0001)
  gamma ~ dnorm(0, 0.0001)
  # random intercept for each database
  for(j in 1:C){
    alpha[j] ~ dnorm(0, 0.0001)
    alpha_c[j] <- alpha[j] - mean.alpha
  }
  mean.alpha <- mean(alpha[1:C])
  # smoothed calendar effect
  calendar[1:Y] ~ car.normal(adj[], weights[], num[], tau.calendar)
  tau.calendar ~ dgamma(0.1, 0.1)
  # correlated errors
  Omega.b[1:C, 1:C] ~ dwish(R[1:C, 1:C], C) # for missing baseline
  Omega[1:C, 1:C] ~ dwish(R[1:C, 1:C], C)
  Sigma[1:C, 1:C] <- inverse(Omega[1:C, 1:C])
}', file=bugs)
close(bugs)

## as above 
# - but with smoothed treatment effect over months
bfile_calendar_treatment_car_month = 'bugs_model_with_treatment_car_month.txt'
bugs = file(bfile_calendar_treatment_car_month, 'w')
cat('model
{
  for(i in 1:N){
      log_counts[i,1:C] ~ dmnorm(mu[i,1:C], Omega[1:C, 1:C])
      trt[i] <- (gamma + lambda[months_since[i]])*treatment[i]
      standard[i] <- int + calendar[year[i]] + trt[i] + zeta*january[i]
      for(j in 1:C){
        mu[i,j] <- alpha_c[j] + beta*baseline[i,j] + standard[i]
      }
      baseline[i,1:C] ~ dmnorm(zeros[1:C], Omega.b[1:C, 1:C]) # for missing
  }
  # priors
  int ~ dnorm(0, 0.0001)
  beta ~ dnorm(0, 0.0001)
  gamma ~ dnorm(0, 0.0001)
  zeta ~ dnorm(0, 0.0001)
  # random intercept for each database
  for(j in 1:C){
    alpha[j] ~ dnorm(0, 0.0001)
    alpha_c[j] <- alpha[j] - mean.alpha
  }
  mean.alpha <- mean(alpha[1:C])
  # smoothed calendar effect
  calendar[1:Y] ~ car.normal(adj[], weights[], num[], tau.calendar)
  tau.calendar ~ dgamma(0.1, 0.1)
  # smoothed treatment effect
  lambda[1:T] ~ car.normal(adj_trt[], weights_trt[], num_trt[], tau.trt)
  tau.trt ~ dgamma(0.1, 0.1)
  # correlated errors
  Omega.b[1:C, 1:C] ~ dwish(R[1:C, 1:C], C) # for missing baseline
  Omega[1:C, 1:C] ~ dwish(R[1:C, 1:C], C)
  Sigma[1:C, 1:C] <- inverse(Omega[1:C, 1:C])
}', file=bugs)
close(bugs)
