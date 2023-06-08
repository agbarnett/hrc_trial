# 2_publication_data_scopus.R
# get the researchers publication data
# version for Scopus
# January 2022
library(dplyr)
library(janitor)
library(rscopus)
library(stringr)
source('99_functions.R')

# API keys for scopus
source('2_keys_not_for_sharing.R')
rscopus::set_api_key(api_key = Elsevier_API)

# from 1_read_hand_entered.R
load('data/1_researchers_post_ra.RData')

### get data ###
# those with scopus number
with_scopus = filter(researchers, !is.na(scopus))
N = nrow(with_scopus)
papers_scopus = citations_scopus = frame_scopus = NULL
for (k in 1:N){
  # check name on Scopus against our hand-entered data
  author = author_retrieval(au_id = with_scopus$scopus[k])
  name = author$content$`author-retrieval-response`[[1]]$`preferred-name`
  scopus_name = paste(name$`given-name`, name$surname)
  cat(scopus_name, '; ', with_scopus$name[k], '\n', sep='')
  # get affiliation
  affiliation = author$content$`author-retrieval-response`[[1]]$`affiliation-current`
  affiliation = affiliation$`affiliation-name`
  #country = affiliation$`affiliation-country` # not available for all
  
  # citations - done elsewhere
  # cites = citation_retrieval(doi=papers$doi[1])
  # http://api.elsevier.com/content/search/scopus?query=DOI(10.1016/j.stem.2011.10.002)&field=citedby-count

  # publication range
  start_year = author$content$`author-retrieval-response`[[1]]$`publication-range`$start
  end_year = author$content$`author-retrieval-response`[[1]]$`publication-range`$end
  
  # frame
  frame = data.frame(number = with_scopus$number[k], # add researcher number
                     affiliation = affiliation,
                     start_year = start_year,
                     end_year = end_year)
  frame_scopus = bind_rows(frame_scopus, frame)
  
  ## get papers
  # can be multiple IDs (e.g., people who changed name)
  ids = str_split(with_scopus$scopus[k], pattern=';')[[1]]
  for(this_id in ids){ # loop through IDs
    papers = author_df(au_id = this_id, verbose = FALSE, general = FALSE) %>%
      mutate(date = as.Date(cover_date, format='%Y-%m-%d'),
        year = format(date, '%Y')) %>% # extract year
      select(auth_order, n_auth, journal, description, title, doi, date, year) %>%
      unique() %>% # safety net for duplicates
      mutate(number = with_scopus$number[k]) # add researcher number
    papers_scopus = bind_rows(papers_scopus, papers)
  }
  
  # check
  if(nrow(papers)==0){cat('No pubs for number ', with_scopus$number[k], ', scholar', with_scopus$scopus[k], '.\n', sep='')}
  
}

# record date collected
date_collected = Sys.Date()

# final edits
papers_scopus = mutate(papers_scopus, 
                       date = ifelse(date > date_collected, date_collected, date), # move any future publication dates
                       date = as.Date(date, origin='1970-01-01'),
                       year = as.numeric(year)) %>%
                filter(!is.na(year)) # remove missing year

# save
save(date_collected, papers_scopus, citations_scopus, frame_scopus, file='data/2_biblio_data_scopus.RData')
