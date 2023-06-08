# 4_make_parameters.R
# add to parameters for winbugs model depending on model choices
# used by 4_model_[].R

parms = c('int','beta','alpha_c','alpha','Sigma','Omega.b') # need alpha & Omega.b for residual model below
if(smooth_calendar == TRUE){
  parms = c(parms, 'calendar','tau.calendar')
}
if(random_intercept == TRUE){
  parms = c(parms, 'r_int','r_int_c','tau.person')
}
if(model_with_treatment == TRUE){
  parms = c(parms, 'gamma')
}
if(smooth_treatment == TRUE | linear_treatment==TRUE){
  parms = c(parms, 'lambda' , 'delta', 'diff')
}
if(add_2022 == TRUE){
  parms = c(parms, 'eta')
}
