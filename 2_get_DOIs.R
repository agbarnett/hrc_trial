# 2_get_DOIs.R
# get the researcher data and create one DOI list per researcher
# used by 2_citation_data_scopus.R and 2_data_altmetric.R
# August 2022

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
for_citation = bind_rows(papers_orcid, papers_researchgate, papers_scopus) %>%
  select(number, id_type, id_num) %>%
  filter(id_type %in% c('doi','pubmed')) %>%
  unique() # remove duplicates