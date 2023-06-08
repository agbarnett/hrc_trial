# 99_bugs_model.R
# bugs model for paper counts; called by 4_model_bibliographic_bayes.R
# July 2022

# Model with:
# - random intercept per participant
# - no calendar
# - no treatment
# - pre-randomisation baseline data that with the same slope across all three databases (I tried a varying model, but there was no evidence of difference)
bfile_intercept = 'bugs_model_intercept.txt'
bugs = file(bfile_intercept, 'w')
cat('model
{
  for(i in 1:N){
      log_counts[i,1:C] ~ dmnorm(mu[i,1:C], Omega[1:C, 1:C])
      standard[i] <- int + r_int_c[person[i]] 
      for(j in 1:C){
        mu[i,j] <- alpha_c[j] + beta*baseline[i,j] + standard[i]
      }
      baseline[i,1:C] ~ dmnorm(zeros[1:C], Omega.b[1:C, 1:C]) # for missing
  }
  # priors
  int ~ dnorm(0, 0.0001)
  beta ~ dnorm(0, 0.0001)
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
  # correlated errors
  Omega.b[1:C, 1:C] ~ dwish(R[1:C, 1:C], C) # for missing baseline
  Omega[1:C, 1:C] ~ dwish(R[1:C, 1:C], C)
  Sigma[1:C, 1:C] <- inverse(Omega[1:C, 1:C])
}', file=bugs)
close(bugs)

# Model with:
# - random intercept per participant
# - no calendar
# - with treatment
# - pre-randomisation baseline data that with the same slope across all three databases (I tried a varying model, but there was no evidence of difference)
bfile_intercept_treatment = 'bugs_model_intercept_treatment.txt'
bugs = file(bfile_intercept_treatment, 'w')
cat('model
{
  for(i in 1:N){
      log_counts[i,1:C] ~ dmnorm(mu[i,1:C], Omega[1:C, 1:C])
      standard[i] <- int + r_int_c[person[i]] + gamma*treatment[i] + eta*last_year[i]
      for(j in 1:C){
        mu[i,j] <- alpha_c[j] + beta*baseline[i,j] + standard[i]
      }
      baseline[i,1:C] ~ dmnorm(zeros[1:C], Omega.b[1:C, 1:C]) # for missing
  }
  # priors
  int ~ dnorm(0, 0.0001)
  eta ~ dnorm(0, 0.0001)
  beta ~ dnorm(0, 0.0001)
  gamma ~ dnorm(0, 0.0001)
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
  # correlated errors
  Omega.b[1:C, 1:C] ~ dwish(R[1:C, 1:C], C) # for missing baseline
  Omega[1:C, 1:C] ~ dwish(R[1:C, 1:C], C)
  Sigma[1:C, 1:C] <- inverse(Omega[1:C, 1:C])
}', file=bugs)
close(bugs)

# Model with:
# - random intercept per participant
# - no calendar
# - with smooth treatment CAR treatment by years since randomisation
# - pre-randomisation baseline data that with the same slope across all three databases (I tried a varying model, but there was no evidence of difference)
bfile_intercept_treatment_smooth = 'bugs_model_intercept_treatment_smooth.txt'
bugs = file(bfile_intercept_treatment_smooth, 'w')
cat('model
{
  for(i in 1:N){
      log_counts[i,1:C] ~ dmnorm(mu[i,1:C], Omega[1:C, 1:C])
      trt[i] <- (lambda[years_since[i]] + gamma)*treatment[i]
      control[i] <- delta[years_since[i]]
      standard[i] <- int + r_int_c[person[i]] + trt[i] + control[i]
      for(j in 1:C){
        mu[i,j] <- alpha_c[j] + beta*baseline[i,j] + standard[i]
      }
      baseline[i,1:C] ~ dmnorm(zeros[1:C], Omega.b[1:C, 1:C]) # for missing
  }
  # priors
  int ~ dnorm(0, 0.0001)
  beta ~ dnorm(0, 0.0001)
  gamma ~ dnorm(0, 0.0001)
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
  # smoothed time-varying effect for years since randomisation
  delta[1:T] ~ car.normal(adj_trt[], weights_trt[], num_trt[], tau.delta)
  tau.delta ~ dgamma(0.1, 0.1)
  # smoothed time-varying treatment effect
  lambda[1:T] ~ car.normal(adj_trt[], weights_trt[], num_trt[], tau.trt)
  tau.trt ~ dgamma(0.1, 0.1)
  # correlated errors
  Omega.b[1:C, 1:C] ~ dwish(R[1:C, 1:C], C) # for missing baseline
  Omega[1:C, 1:C] ~ dwish(R[1:C, 1:C], C)
  Sigma[1:C, 1:C] <- inverse(Omega[1:C, 1:C])
  # treatment difference
  for (k in 1:T){
     diff[k] <- lambda[k] + gamma - delta[k]
  }

}', file=bugs)
close(bugs)


# Model with:
# - random intercept per participant
# - no calendar
# - with linear treatment by years since randomisation interaction
# - pre-randomisation baseline data that with the same slope across all three databases (I tried a varying model, but there was no evidence of difference)
bfile_intercept_treatment_linear = 'bugs_model_intercept_treatment_linear.txt'
bugs = file(bfile_intercept_treatment_linear, 'w')
cat('model
{
  for(i in 1:N){
      log_counts[i,1:C] ~ dmnorm(mu[i,1:C], Omega[1:C, 1:C])
      trt[i] <- (lambda*years_since[i] + gamma)*treatment[i]
      control[i] <- (delta*years_since[i])*(1-treatment[i])
      standard[i] <- int + r_int_c[person[i]] + trt[i] + control[i] + eta*last_year[i]
      for(j in 1:C){
        mu[i,j] <- alpha_c[j] + beta*baseline[i,j] + standard[i]
      }
      baseline[i,1:C] ~ dmnorm(zeros[1:C], Omega.b[1:C, 1:C]) # for missing
  }
  # priors
  int ~ dnorm(0, 0.0001)
  beta ~ dnorm(0, 0.0001)
  gamma ~ dnorm(0, 0.0001)
  lambda ~ dnorm(0, 0.0001)
  delta ~ dnorm(0, 0.0001)
  eta ~ dnorm(0, 0.0001)
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
    alpha_c[j] <- alpha[j] - mean.alpha # centre
  }
  mean.alpha <- mean(alpha[1:C])
  # correlated errors
  Omega.b[1:C, 1:C] ~ dwish(R[1:C, 1:C], C) # for missing baseline
  Omega[1:C, 1:C] ~ dwish(R[1:C, 1:C], C)
  Sigma[1:C, 1:C] <- inverse(Omega[1:C, 1:C])
  # treatment difference (years since is centred at 2)
  for (k in 1:T){
     diff[k] <- (lambda*(k-2) + gamma) - (delta*(k-2))
  }

}', file=bugs)
close(bugs)

# Model with:
# - random effect for calendar year (smoothed using CAR)
# - pre-randomisation baseline data that is the same across all three databases (I tried a varying model, but there was no evidence of difference)
bfile_calendar = 'bugs_model_just_calendar.txt'
bugs = file(bfile_calendar, 'w')
cat('model
{
  for(i in 1:N){
      log_counts[i,1:C] ~ dmnorm(mu[i,1:C], Omega[1:C, 1:C])
      standard[i] <- int + calendar[year[i]]
      for(j in 1:C){
        mu[i,j] <- alpha_c[j] + beta*baseline[i,j] + standard[i]
      }
      baseline[i,1:C] ~ dmnorm(zeros[1:C], Omega.b[1:C, 1:C]) # for missing
  }
  # priors
  int ~ dnorm(0, 0.0001)
  beta ~ dnorm(0, 0.0001)
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
bfile_calendar_treatment = 'bugs_model_with_treatment.txt'
bugs = file(bfile_calendar_treatment, 'w')
cat('model
{
  for(i in 1:N){
      log_counts[i,1:C] ~ dmnorm(mu[i,1:C], Omega[1:C, 1:C])
      standard[i] <- int + calendar[year[i]] + gamma*treatment[i]
      for(j in 1:C){
        mu[i,j] <- alpha_c[j] + beta*baseline[i,j] + standard[i]
      }
      baseline[i,1:C] ~ dmnorm(zeros[1:C], Omega.b[1:C, 1:C]) # for missing
  }
  # priors
  int ~ dnorm(0, 0.0001)
  beta ~ dnorm(0, 0.0001)
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
# - but with smoothed treatment effect over years
bfile_calendar_treatment_car = 'bugs_model_with_treatment_car.txt'
bugs = file(bfile_calendar_treatment_car, 'w')
cat('model
{
  for(i in 1:N){
      log_counts[i,1:C] ~ dmnorm(mu[i,1:C], Omega[1:C, 1:C])
      trt[i] <- (lambda[years_since[i]] + gamma)*treatment[i]
      control[i] <- delta[years_since[i]]
      standard[i] <- int + calendar[year[i]] + trt[i] + control[i]
      for(j in 1:C){
        mu[i,j] <- alpha_c[j] + beta*baseline[i,j] + standard[i]
      }
      baseline[i,1:C] ~ dmnorm(zeros[1:C], Omega.b[1:C, 1:C]) # for missing
  }
  # priors
  int ~ dnorm(0, 0.0001)
  gamma ~ dnorm(0, 0.0001)
  beta ~ dnorm(0, 0.0001)
  # random intercept for each database
  for(j in 1:C){
    alpha[j] ~ dnorm(0, 0.0001)
    alpha_c[j] <- alpha[j] - mean.alpha
  }
  mean.alpha <- mean(alpha[1:C])
  # smoothed calendar effect
  calendar[1:Y] ~ car.normal(adj[], weights[], num[], tau.calendar)
  tau.calendar ~ dgamma(0.1, 0.1)
  # smoothed time-varying effect for years since randomisation
  delta[1:T] ~ car.normal(adj_trt[], weights_trt[], num_trt[], tau.delta)
  tau.delta ~ dgamma(0.1, 0.1)
  # smoothed time-varying treatment effect
  lambda[1:T] ~ car.normal(adj_trt[], weights_trt[], num_trt[], tau.trt)
  tau.trt ~ dgamma(0.1, 0.1)
  # correlated errors
  Omega.b[1:C, 1:C] ~ dwish(R[1:C, 1:C], C) # for missing baseline
  Omega[1:C, 1:C] ~ dwish(R[1:C, 1:C], C)
  Sigma[1:C, 1:C] <- inverse(Omega[1:C, 1:C])
}', file=bugs)
close(bugs)

## as above 
# - but with smoothed treatment effect over years
# - second version
bfile_calendar_treatment_car = 'bugs_model_with_treatment_car.txt'
bugs = file(bfile_calendar_treatment_car, 'w')
cat('model
{
  for(i in 1:N){
      log_counts[i,1:C] ~ dmnorm(mu[i,1:C], Omega[1:C, 1:C])
      trt[i] <- (lambda[year[i]] + gamma)*treatment[i]
      control[i] <- delta[year[i]]
      standard[i] <- int + trt[i] + control[i]
      for(j in 1:C){
        mu[i,j] <- alpha_c[j] + beta*baseline[i,j] + standard[i]
      }
      baseline[i,1:C] ~ dmnorm(zeros[1:C], Omega.b[1:C, 1:C]) # for missing
  }
  # priors
  int ~ dnorm(0, 0.0001)
  gamma ~ dnorm(0, 0.0001)
  beta ~ dnorm(0, 0.0001)
  # random intercept for each database
  for(j in 1:C){
    alpha[j] ~ dnorm(0, 0.0001)
    alpha_c[j] <- alpha[j] - mean.alpha
  }
  mean.alpha <- mean(alpha[1:C])
  # smoothed time-varying effect for years since funding
  delta[1:Y] ~ car.normal(adj[], weights[], num[], tau.delta)
  tau.delta ~ dgamma(0.1, 0.1)
  # smoothed time-varying treatment effect
  lambda[1:Y] ~ car.normal(adj[], weights[], num[], tau.trt)
  tau.trt ~ dgamma(0.1, 0.1)
  # correlated errors
  Omega.b[1:C, 1:C] ~ dwish(R[1:C, 1:C], C) # for missing baseline
  Omega[1:C, 1:C] ~ dwish(R[1:C, 1:C], C)
  Sigma[1:C, 1:C] <- inverse(Omega[1:C, 1:C])
  # treatment difference
  for (k in 1:Y){
     diff[k] <- lambda[k] + gamma - delta[k]
  }
}', file=bugs)
close(bugs)
