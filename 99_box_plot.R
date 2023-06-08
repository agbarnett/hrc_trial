# 99_box_plot.R
# box plot of yearly counts, inspecting 2022
# July 2022
library(ggplot2)

# data from 5_main_analysis.Rmd
matrix_count = mutate(analysis_ready,
                      scopus = scopus / denom_scopus, # scale paper counts by database-specific denominator to adjust for final year of data collection not being a full year
                      scholar = scholar / denom_scholar,
                      researchgate = researchgate / denom_researchgate)
for_plot = dplyr::select(matrix_count, number, year, 'scopus', 'scholar', 'researchgate') %>%
  pivot_longer(names_to = 'database', values_to = 'papers', cols=c('scopus', 'scholar', 'researchgate'))

bplot = ggplot(data=for_plot, aes(x=factor(year), y=papers))+
  geom_boxplot()+
  facet_wrap(~database)+
  theme_bw()
bplot
