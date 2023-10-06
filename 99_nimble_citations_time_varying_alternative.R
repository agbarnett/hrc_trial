# 99_nimble_citations_time_varying_alternative.R
# alternative models of citation counts as Bayes models not converging
# September 2023
library(lme4) # for glmer
library(influence.ME) # for Cooks and df-beta
library(ggplot2)
library(broom.mixed)

# try standard GLM
model = glmer(scopus ~ I(funded=='Funded')*years_since +  I(log2(b_scopus+1)) + (1|number), family = poisson(), data = analysis_ready_citations)
summary(model)
i = influence(model, group='number')
plot(i, which ='cook') # plot Cook's distance to find influential observations
# find highest, numbers = 80 and 14.1
tail(cooks.distance(i, parameter=0, sort=TRUE))
# check their data
filter(analysis_ready_citations, number %in% c(14.1,80)) %>%
  select(number, year, career_year, b_scopus, funded, scopus)

# estimates
ests = tidy(model, conf.int = TRUE) %>%
  rename('mean' = 'estimate',
         'sd' = 'std.error',
         'lower' = 'conf.low',
         'upper' = 'conf.high',
         'pvalue' = 'p.value') %>%
  mutate(parameter = case_when(
    str_detect(term, '^\\(Intercept') ~ 'alpha',
    str_detect(term, ':') ~ 'interaction',
    str_detect(term, 'funded') ~ 'gamma',
    str_detect(term, 'b_scopus') ~ 'beta',
    str_detect(term, 'years_since') ~ 'years_since',
    str_detect(term, '^sd') ~ 'sd.person'
  )) %>%
  select(-statistic, -term, -group)

# make residuals ...
res = mutate(analysis_ready_citations,
             fitted = fitted(model),
             residual = scopus - fitted) %>%
  select(number, year, scopus, funded, b_scopus, fitted, residual)

# predictions
predict = expand.grid(years_since = 1:6, funded = c('Funded','Not funded')) %>%
  mutate(b_scopus = median(analysis_ready_citations$b_scopus))
pred = predict(model, newdata = predict, re.form = ~0, type='response')
predict = bind_cols(predict, pred)
names(predict)[4] = 'fitted'
#

# for saving the results
results_citations_time_varying = list()
results_citations_time_varying$model = model
results_citations_time_varying$table = ests
results_citations_time_varying$residuals = res
results_citations_time_varying$predict = predict
