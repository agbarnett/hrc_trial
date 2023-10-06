# 0_read_data_randomised.R
# read the researcher data and the summary data on the number of applications
# data from NZ HRC
# July 2022
library(readxl)
library(janitor)
library(dplyr)
library(stringr)

## part 1: researchers ###

# get list of consented
consent = read_excel(path = '../data/randomised.xlsx', sheet='Researchers') %>%
  clean_names()
# get list of non-consented but funded
non_consent = read_excel(path = '../data/randomised.xlsx', sheet='Non-consenting researchers ', skip=1) %>%
  clean_names() %>%
  mutate(result='Funded')

# process data 
researchers = bind_rows(consent, non_consent, .id='consent') %>%
  mutate(first_named_investigator_title = ifelse(is.na(first_named_investigator_title), '', first_named_investigator_title),
         date = as.Date(as.character(date_announced), format='%Y%m%d', origin='1970-01-01'),
         year = as.numeric(str_sub(date_announced, 1, 4)),
         consent = ifelse(consent==1, 'Yes', 'No')) %>%
  filter(year <= 2021) %>% # needed for paper, this was the last year used
  select(result, 
         consent,
         first_named_investigator_title,
         first_named_investigator_first_name,
         first_named_investigator_surname,
         host,
         date,
         year) %>%
  rename('title' = 'first_named_investigator_title',
         'first_name' = 'first_named_investigator_first_name',
         'surname' = 'first_named_investigator_surname') %>%
  mutate(name=paste(first_name, surname))


# Make linking number for hand-entered data
one_result = arrange(researchers, surname, desc(year)) %>%
  group_by(first_name, surname) %>%
  slice(1) %>% # take most recent
  ungroup() %>%
  select(first_name, surname) %>%
  arrange(surname) %>%
  mutate(number = 1:n()) # linking number
# merge
researchers = left_join(researchers, one_result, by=c('first_name','surname'))

# if a researcher was funded and not funded in same year, then just keep funded; also works for those funded twice in same year
researchers = group_by(researchers, number, year) %>%
  arrange(researchers, number, year, result) %>%
  slice(1) %>%
  ungroup()

# check
check = mutate(researchers, id = paste(number, year, sep='.'))
table(table(check$id)) # should all be 1

## part 2: numbers ###
# n_max to avoid total row
numbers = read_excel(path = '../data/randomised.xlsx', skip=1, n_max=7, sheet='Numbers') %>% 
  clean_names()

## save
save(researchers, numbers, file='data/0_researchers_randomised.RData')
