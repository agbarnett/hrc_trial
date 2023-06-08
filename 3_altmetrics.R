# 3_altmetrics.R
# get altmetrics data on papers
# see https://www.altmetric.com/research-access/
# July 2002
library(dplyr)
library(stringr)
library(rAltmetric)
source('99_functions.R') # has altmetric version to avoid stopping when errors are hit

## waiting on API key from altmetric

# get the researcher data and create one DOI list per researcher
load('data/2_biblio_data_scopus.RData')
#load('data/2_biblio_data_scholar.RData') # no DOIs, so do not load
load('data/2_biblio_data_orcid.RData') # still use, even though completion is poor
load('data/2b_biblio_data_researchgate.RData') # 

## get all available DOIs/pubmeds for a researcher
# prepare to combine
papers_orcid = select(papers_orcid, number, id_type, id_num) %>%
  filter(!is.na(id_num)) 
papers_researchgate = filter(papers_researchgate, !is.na(doi)) %>% # cannot be missing
  mutate(id_type = 'doi') %>% # always DOI
  rename('id_num' = 'doi')
papers_scopus = select(papers_scopus, number, doi) %>%
  mutate(id_type = 'doi') %>% # create new variable, always DOI
  rename('id_num' = 'doi') %>%
  filter(!is.na(id_num))  # doi cannot be missing
# put all three sources together  
for_altmetric = bind_rows(papers_orcid, papers_researchgate, papers_scopus) %>%
  select(number, id_type, id_num) %>%
  filter(id_type %in% c('doi','pubmed')) %>%
  unique() # remove duplicates

## loop through DOIs 
altmetric = NULL
for (k in 1:nrow(for_altmetric)){
  if(for_citation$id_type[k] == 'doi'){
    res <- altmetrics(doi = for_altmetric$id_num[k], apikey=my.key)
  }
  if(for_citation$id_type[k] == 'pubmed '){
    res <- altmetrics(pmid = for_altmetric$id_num[k], apikey=my.key)
  }
  
}
# 

# save
date_altmetric = Sys.Date()
save(date_altmetric, altmetric, file = 'data/3_altmetric.RData')
