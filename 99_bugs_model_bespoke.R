# 99_bugs_model_bespoke.R
# bugs model for paper counts; called by 4_model_bibliographic_bayes.R
# makes a bespoke model dependent on model choices
# do not use a function
# July 2022

bugs = file(bfile, 'w')
  cat('model
{
  for(i in 1:N){
      log_counts[i,1:C] ~ dmnorm(mu[i,1:C], Omega[1:C, 1:C])
      standard[i] <- int ', file=bugs)
  if(random_intercept==TRUE){
    cat('+ zeta*january[i]', file=bugs)
  }
  if(random_intercept==TRUE){
    cat('+ r_int_c[person[i]]', file=bugs)
  }
  if(smooth_calendar==TRUE){
    cat('+ calendar[year[i]]', file=bugs)
  }
  if(model_with_treatment==TRUE){
    cat('+ gamma*treatment[i]', file=bugs)
  }
  if(add_2022==TRUE){
    cat('+ eta*last_year[i]', file=bugs)
  }
  if(linear_treatment==TRUE){
    cat('+ trt[i] + control[i]
        trt[i] <- (lambda*years_since[i] + gamma)*treatment[i]
        control[i] <- (delta*years_since[i])*(1-treatment[i])', file=bugs)
  }
  if(smooth_treatment==TRUE){
    cat('+ trt[i] + control[i]
        trt[i] <- (lambda[years_since[i]] + gamma)*treatment[i]
        control[i] <- delta[years_since[i]]*(1-treatment[i])', file=bugs)
  }
  cat('
      for(j in 1:C){
        mu[i,j] <- alpha_c[j] + beta*baseline[i,j] + standard[i]
      }
      baseline[i,1:C] ~ dmnorm(zeros[1:C], Omega.b[1:C, 1:C]) # for missing
  }
  ## priors
  # random intercept for each database
  for(j in 1:C){
    alpha[j] ~ dnorm(0, 0.0001)
    alpha_c[j] <- alpha[j] - mean.alpha
  }
  mean.alpha <- mean(alpha[1:C])
  # 
  int ~ dnorm(0, 0.0001)
  beta ~ dnorm(0, 0.0001)', file=bugs)
  if(linear_treatment==TRUE){
    cat('
        delta ~ dnorm(0, 0.0001)
        lambda ~ dnorm(0, 0.0001)', file=bugs)
  }
  if(model_with_treatment==TRUE){
    cat('
        gamma ~ dnorm(0, 0.0001)', file=bugs)
  }
  if(monthly==TRUE){
    cat('
        zeta ~ dnorm(0, 0.0001)', file=bugs)
  }
  if(add_2022==TRUE){
    cat('
        eta ~ dnorm(0, 0.0001)', file=bugs)
  }
  if(random_intercept==TRUE){
    cat('
    # random intercept for each researcher
    for(j in 1:P){
      r_int[j] ~ dnorm(0, tau.person)
      r_int_c[j] <- r_int[j] - mean.person
    }
    mean.person <- mean(r_int[1:P])
    tau.person ~ dgamma(0.1, 0.1)', file=bugs)
  }
  if(smooth_calendar==TRUE){
    cat('
    # smoothed calendar effect
    calendar[1:Y] ~ car.normal(adj[], weights[], num[], tau.calendar)
    tau.calendar ~ dgamma(0.1, 0.1)', file=bugs)
  }
  if(smooth_treatment==TRUE){
    cat('
    # smoothed time-varying effect for years since randomisation
    delta[1:T] ~ car.normal(adj_trt[], weights_trt[], num_trt[], tau.delta)
    tau.delta ~ dgamma(0.1, 0.1)
    # smoothed time-varying treatment effect
    lambda[1:T] ~ car.normal(adj_trt[], weights_trt[], num_trt[], tau.trt)
    tau.trt ~ dgamma(0.1, 0.1)
    # treatment difference
    for (k in 1:T){
       diff[k] <- (lambda[k] + gamma) - delta[k]
    }', file=bugs)
  }
  if(linear_treatment==TRUE){
    cat('
    # treatment difference (years since is centred at 2)
    for (k in 1:T){
       diff[k] <- (lambda*(k-2) + gamma) - (delta*(k-2))
    }', file=bugs)
  }
  cat('
  # correlated errors
  Omega.b[1:C, 1:C] ~ dwish(R[1:C, 1:C], C) # for missing baseline
  Omega[1:C, 1:C] ~ dwish(R[1:C, 1:C], C)
  Sigma[1:C, 1:C] <- inverse(Omega[1:C, 1:C])
}', file=bugs)
close(bugs)
  
