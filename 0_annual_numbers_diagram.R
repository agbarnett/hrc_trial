# 0_annual_numbers_diagram.R
# diagram of annual numbers
# January 2021
library(readxl)
library(dplyr)
library(janitor)
library(ggplot2)
library(tidyr)

### data
# get all the numbers per year
numbers = read_excel('../data/randomised.xlsx', sheet='Numbers', n_max=7, na = "NA", skip = 1) %>%
  clean_names() %>%
  select(-url_for_media_release)

# ~~~ part 1 ~~~ #
# prepare data for plot of numbers over time
numbers1 = select(numbers, year, funded, not_funded) 
columns = names(numbers1)
columns = columns[columns != 'year'] # id column
long1 = pivot_longer(numbers1, cols = all_of(columns))
# text for bars
labels = group_by(long1, year) %>%
  arrange(year, rev(name)) %>% # to match order of bars
  mutate(row = 1:n(),
         total = cumsum(value),
         name = 'funded', # dummy needed for ggplot
         y = ifelse(row==1, value/2, lag(value) + value/2)) %>% # y depends on first row
  filter(y >= 1) # do not add zeros
# plot
tplot = ggplot(data=long1, aes(x=year, y=value, fill=name))+
  geom_bar(position='stack', stat='identity')+
  geom_text(data=labels, aes(x=year, y=y, label=total), col='grey88')+
  scale_fill_manual(NULL, labels=c('Funded','Not funded'), values=c('dodgerblue','darkseagreen'))+
  xlab('Year') + 
  ylab('Count') + 
  theme_bw()+
  theme(panel.grid.minor = element_blank(),
        legend.position = 'top')
tplot

# export
jpeg('figures/numbers_year.jpg', width=5, height=4, units='in', res=300)
print(tplot)
dev.off()

# ~~~ part 2 ~~~ #
# prepare data for plot of consent over time
numbers2 = select(numbers, year, consent, not_consent) 
columns = names(numbers2)
columns = columns[columns != 'year'] # id column
long2 = pivot_longer(numbers2, cols = all_of(columns))
# text for bars
labels = group_by(long2, year) %>%
  arrange(year, rev(name)) %>% # to match order of bars
  mutate(row = 1:n(),
         total = cumsum(value),
         name = 'consent', # dummy needed for ggplot
         y = ifelse(row==1, value/2, lag(value) + value/2)) # y depends on first row
# plot
tplot2 = ggplot(data=long2, aes(x=year, y=value, fill=name))+
  geom_bar(position='stack', stat='identity')+
  geom_text(data=labels, aes(x=year, y=y, label=total), col='grey95')+
  scale_fill_manual(NULL, labels=c('Consented','Not consented'), values=c('indianred2','pink'))+
  xlab('Year') + 
  ylab('Count') + 
  theme_bw()+
  theme(panel.grid.minor = element_blank(),
        legend.position = 'top')
tplot2

# export
jpeg('figures/consent_year.jpg', width=5, height=4, units='in', res=300)
print(tplot2)
dev.off()
