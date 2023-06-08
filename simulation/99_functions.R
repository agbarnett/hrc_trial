# 99_functions.R
# jan 2023

# extract estimates from Poisson model
make_nice_estimates = function(inmodel, name, n_applicants, extra_papers, true_increase){
  s = summary(inmodel)
  s = data.frame(s$coefficients) %>%
    clean_names() %>%
    tibble::rownames_to_column() %>%
    mutate(model = name,
           true_increase = true_increase,
           extra_papers = extra_papers,
           n_applicants = n_applicants) 
  ci = data.frame(suppressMessages(confint(inmodel)))
  names(ci) = c('lower','upper')
  s = bind_cols(s, ci)
  # show funding effect as percent increase
  baseline = filter(s, str_detect(pattern='Intercept', rowname)) %>%
    pull(estimate)
  s = filter(s, str_detect(pattern='funded', rowname)) %>%
    mutate(perc = exp(estimate + baseline) / exp(baseline) ,
           perc_lower = exp(baseline + lower) / exp(baseline),
           perc_upper = exp(baseline + upper) / exp(baseline)) %>%
    select(-rowname)
  #
  return(s)
}