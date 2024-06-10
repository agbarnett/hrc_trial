# 4_get_data.R
# get the data needed for analysis
# put here in a separate file because it can be used by multiple programs
# June 2022

# manually entered data from research assistant
load('data/1_researchers_post_ra.RData')
# this also has other funding data in `other_funding`

### funding ###

## fill in missing years
all_years = group_by(other_funding, number) %>%
  summarise(start = min(year),
            end = 2022) %>%  # does go to 2022 even though collected in 2021
  mutate(start = ifelse(is.na(start), 2012, start)) %>% # fill one missing with dummy year
  pivot_longer(cols=c('start','end'), values_to = 'year') %>%
  group_by(number) %>% 
  complete(year = min(year):max(year), fill = list(active = FALSE))  %>%
  dplyr::select(-name) %>%
  unique()

#
funding_total = filter(other_funding, !is.na(year)) %>% # can't use missing years
  group_by(number, year) %>%
  summarise(N_awards = n(),
            funding = sum(dollars)) %>% # total per year
  ungroup() %>%
  full_join(all_years, by=c('number','year')) %>%
  arrange(number, year) %>%
  mutate(database = 'altmetric',  # use `altmetric` for function
         N_awards = ifelse(is.na(N_awards), 0, N_awards),
         funding = ifelse(is.na(funding), 0, funding),
         denom = 1 # just up to end of 2021, so no need for denominator
         ) %>%
  rename('n' = 'funding') # to match papers and so help with functions.


### publications ###

## get the publication data, from # 2_publication_data_[database].R
load('data/2_biblio_data_scholar.RData')
scholar_date = date_collected
#load('data/2_biblio_data_orcid.RData') # do not use
load('data/2b_biblio_data_researchgate.RData') # from 2b_publication_data_researchgate.R
researchgate_date = date_collected
load('data/2_biblio_data_scopus.RData')
scopus_date = date_collected
# make frame of dates used by count function below
collection_dates = data.frame(database = c('scholar', 'scopus', 'researchgate'),
                              dates = c(scholar_date, scopus_date, researchgate_date))

## get annual counts per researcher and year
papers = bind_rows(papers_scholar, papers_scopus, papers_researchgate, .id='database') %>%
  mutate(database = case_when(
    database == 1 ~ 'scholar',
    database == 2 ~ 'scopus',
    database == 3 ~ 'researchgate'
  ))
paper_counts = make_counts(indata = papers, 
                           in_dates = collection_dates) # from 99_functions.R
## get monthly counts per researcher and year (for scopus/researchgate only)
paper_counts_monthly = make_counts_monthly(
  indata = filter(papers, database != 'scholar'),
  in_dates = collection_dates) # from 99_functions.R

# earliest year for papers
min_year = min(papers$year)

# randomised group, from 0_read_data_randomised.R
load('data/0_researchers_randomised.RData')
# add yrmon of funding for monthly counts
researchers = mutate(researchers,
                     month = as.numeric(format(date,'%m')),
                     yrmon = year + ((month-1)/12)) %>%
  filter(year <= 2021) %>% # needed for paper, as data were only used up to 2021.
  dplyr::select(-month)

# add start year of papers being published to researcher data (there are some who started late)
start_year = group_by(paper_counts, number) %>%
  summarise(start_year = min(year)) %>%
  ungroup()
researchers = left_join(researchers, start_year, by='number')

### citations ###

## get the citation data, from 2_publication_data_scopus.R
load('data/2_citations.RData')
scopus_date_citations = date_collected
frac = season::yrfraction(scopus_date_citations)
## total citations
# to match names used for papers
citations = mutate(citations, database = 'scopus') %>% # add the database
#  rename('n' = 'total') %>% # to match papers - already done in program
  mutate(denom = case_when(
    year < 2022 ~ 1,
    year == 2022 ~ frac
  ) ) %>%
  dplyr::select(-cum)
## citations per paper
# to match names used for papers
citations_per_paper = mutate(citations, database = 'scopus') %>% # add the database
  dplyr::select(-n) %>%
  rename('n' = 'average') %>% # to match papers
  mutate(denom = case_when(
    year < 2022 ~ 1,
    year == 2022 ~ frac
  ) ) 

### altmetric ###

## get the altmetric data, from 2_data_altmetric.R
load('data/2_altmetric.RData')
altmetric_date = date_collected
frac = season::yrfraction(altmetric_date)

## fill in missing years
all_years = group_by(altmetric, number) %>%
  summarise(start = min(year),
            end = 2022) %>% 
  pivot_longer(cols=c('start','end'), values_to = 'year') %>%
  group_by(number) %>% 
  complete(year = min(year):max(year), fill = list(active = FALSE))  %>%
  select(-name) %>%
  unique()

# get yearly average in altmetric score
altmetric_average = group_by(altmetric, number, year) %>%
  summarise(N_papers = n(),
            altmetric = mean(score)) %>%
  ungroup() %>%
  full_join(all_years, by=c('number','year')) %>%
  arrange(number, year) %>%
  mutate(database = 'altmetric',  # add the database (to match names used for papers)
         N_papers = ifelse(is.na(N_papers), 0, N_papers),
         altmetric = ifelse(is.na(altmetric), 0, altmetric),
    denom = case_when(
      year < 2022 ~ 1,
      year == 2022 ~ frac
  )) %>%
  rename('n' = 'altmetric') # to match papers and so help with functions.

