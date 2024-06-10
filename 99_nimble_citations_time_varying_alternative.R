# 99_nimble_citations_time_varying_alternative.R
# alternative models of citation counts as Bayes models not converging
# September 2023
library(lme4) # for glmer
library(influence.ME) # for Cooks and df-beta
library(ggplot2)
library(broom.mixed)

## try standard GLM
# remove need for I() in equation
analysis_ready_citations = mutate(analysis_ready_citations,
                                  interaction = as.numeric(funded=='Funded')*years_since,
                                  baseline = log2(b_scopus+1))
#
model = glmer(scopus ~ years_since + interaction + baseline + (1|number), family = poisson(), data = analysis_ready_citations)
summary(model)
i = influence(model, group='number')
plot(i, which ='cook') # plot Cook's distance to find influential observations
# find highest, numbers = 80 and 14.1
tail(cooks.distance(i, parameter=0, sort=TRUE))
# check their data
filter(analysis_ready_citations, number %in% c(14.1,80)) %>%
  dplyr::select(number, year, career_year, b_scopus, funded, scopus)

# estimates
ests = tidy(model, conf.int = TRUE) %>%
  rename('mean' = 'estimate',
         'sd' = 'std.error',
         'lower' = 'conf.low',
         'upper' = 'conf.high',
         'pvalue' = 'p.value') %>%
  mutate(parameter = case_when(
    str_detect(term, '^\\(Intercept') ~ 'alpha',
    str_detect(term, ':|interaction') ~ 'interaction',
    str_detect(term, 'funded') ~ 'gamma',
    str_detect(term, 'b_scopus|baseline') ~ 'beta',
    str_detect(term, 'years_since') ~ 'years_since',
    str_detect(term, '^sd') ~ 'sd.person'
  )) %>%
  dplyr::select(-statistic, -term, -group)

# make residuals ...
res = mutate(analysis_ready_citations,
             fitted = fitted(model),
             residual = scopus - fitted) %>%
  dplyr::select(number, year, scopus, funded, b_scopus, fitted, residual)

# predictions with intervals
predict = expand.grid(years_since = 1:6, funded = c('Funded','Not funded')) %>%
  mutate(baseline = median(analysis_ready_citations$baseline),
         number = 99, # dummy number that does not exist
         interaction = as.numeric(funded=='Funded')*years_since) # to avoid needing I()
pred = predict(model, newdata = predict, re.form = ~0, type='response')#, se)
predict = bind_cols(predict, pred)
names(predict)[6] = 'fitted'
# now add intervals using bootstrap
library(merTools) 
PI <- predictInterval(merMod = model, newdata = predict, which='fixed',
                      level = 0.8, n.sims = 5000, seed = 1233,
                      returnSims = TRUE, 
                      stat = "median", 
                      type="linear.prediction",
                      include.resid.var = FALSE)

#
predict_plus = bind_cols(predict, PI) %>%
  mutate(fit_check = exp(fit), # from Poisson
         upr = exp(upr),
         lwr = exp(lwr))

# for saving the results
results_citations_time_varying = list()
results_citations_time_varying$model = model
results_citations_time_varying$table = ests
results_citations_time_varying$residuals = res
results_citations_time_varying$predict = predict_plus

