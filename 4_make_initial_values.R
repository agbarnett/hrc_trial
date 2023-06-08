# 4_make_initial_values.R
# add to initial values for winbugs model depending on model choices
# used by 4_model_[].R
# July 2022

inits = list(int = 0, beta = 0, alpha = rep(0,C), Omega = R, Omega.b = R) # start all with no flag for mean or variance
if(model_with_treatment == TRUE){
  inits$gamma = 0 # initial value for treatment
}
if(random_intercept ==TRUE){
  inits$r_int = rep(0, P); inits$tau.person = 100
}
if(smooth_calendar ==TRUE){
  inits$calendar = rep(0, Y); inits$tau.calendar = 1
}
if(add_2022 ==TRUE){
  inits$eta = 0
}
if(model_with_treatment == TRUE & smooth_treatment==TRUE){
  inits$lambda = rep(0, bdata$T)
  inits$tau.trt = 1
  inits$delta = rep(0, bdata$T)
  inits$tau.delta = 1
}
if(model_with_treatment == TRUE & linear_treatment ==TRUE){
  inits$lambda = 0
  inits$delta = 0
}
inits = rep(list(inits), n.chains) # repeat per chains
