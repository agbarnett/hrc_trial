# 99_nimble_citations_alternative.R
# alternative models of citation counts as Bayes models not converging
# September 2023
library(lme4) # for glmer
library(influence.ME) # for Cooks and df-beta
library(ggplot2)
library(broom.mixed)

# try standard GLM
model = glmer(scopus ~ I(funded=='Funded') + I(log2(b_scopus+1)) + (1|number), family = poisson(), data = analysis_ready_citations)
summary(model)
i = influence(model, group='number')
plot(i, which ='cook') # plot Cook's distance to find influential observations
# find highest, number = 51
tail(cooks.distance(i, parameter=0, sort=TRUE))
# check their data, person with no baseline citations
filter(analysis_ready_citations, number == 51) %>%
  select(year, career_year, b_scopus, funded, scopus)

# estimates
ests = tidy(model, conf.int = TRUE) %>%
  rename('mean' = 'estimate',
         'sd' = 'std.error',
         'lower' = 'conf.low',
         'upper' = 'conf.high',
         'pvalue' = 'p.value') %>%
  mutate(parameter = case_when(
    str_detect(term, '^\\(Intercept') ~ 'alpha',
    str_detect(term, 'funded') ~ 'gamma',
    str_detect(term, 'b_scopus') ~ 'beta',
    str_detect(term, '^sd') ~ 'sd.person'
  )) %>%
  select(-statistic, -term, -group)

# plot by treatment
gplot = ggplot(data = analysis_ready_citations, aes(x=funded, y=scopus))+
  geom_boxplot()+
  theme_bw()
gplot

# make residuals ...
res = mutate(analysis_ready_citations,
             fitted = fitted(model),
             residual = scopus - fitted) %>%
  select(number, year, scopus, funded, b_scopus, fitted, residual)

# for saving the results
results_citations = list()
results_citations$model = model
results_citations$table = ests
results_citations$residuals = res
