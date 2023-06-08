# 4_model_additions.R
# add to data for winbugs model depending on model choices
# used by 4_model_[].R
# July 2022

# Add binary effect for 2022
if(add_2022 == TRUE){
  bdata$last_year = as.numeric(indata$year == 2022) #
}

# Add random intercept
if(random_intercept == TRUE){
  P = length(unique(indata$number)) # number of researchers
  bdata$person = as.numeric(as.factor(indata$number)) #
  bdata$P = P
}

# Add smooth calendar
if(smooth_calendar == TRUE){
  # CAR prior
  Y = length(unique(indata$year)) # calendar year
  x = rep(NA, Y); x[2] = 1
  V = toeplitz(x)
  Adjacency = season::createAdj(V)
  # add to data:
  bdata$year = indata$year - min_year + 1 # year as an integer
  bdata$Y = Y
  bdata$adj = Adjacency$adj
  bdata$num = Adjacency$num
  bdata$weights = Adjacency$weight
}

# 
if(model_with_treatment == TRUE){
  bdata$treatment = as.numeric(indata$funded == 'Funded')
}
# 
if(model_with_treatment == TRUE & smooth_treatment==TRUE){
  #
  if(monthly==TRUE){
    bdata$years_since = round(indata$years_since*12) # says years, but is months
  }
  bdata$T = max(bdata$years_since)
  # adjacency matrix for smooth treatment effect
  x = rep(NA, bdata$T); x[2] = 1
  V = toeplitz(x)
  Adjacency = season::createAdj(V)
  bdata$adj_trt = Adjacency$adj
  bdata$weights_trt = Adjacency$weight
  bdata$num_trt = Adjacency$num
}
#
if(linear_treatment == TRUE){
  if(monthly==FALSE){
    bdata$years_since = indata$years_since - 2 # centre
  }
  if(monthly==TRUE){
    bdata$years_since = round(indata$years_since*12) - 24 # says years, but is months; centre
  }
  bdata$T = length(unique(bdata$years_since))
}

#cat(str(bdata))
