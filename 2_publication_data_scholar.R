# 2_publication_data_scholar.R
# get the researchers publication and citation data
# version for google scholar
# results for Tran (number 77; k = 63) no longer working. Looks like their page was removed?
# March 2022
library(dplyr)
library(tidyverse)
library(janitor)
library(scholar)
library(stringr)
library(readxl)
source('99_functions.R')

# from 1_read_hand_entered.R
load('data/1_researchers_post_ra.RData')

### get data ###
with_scholar = filter(researchers, !is.na(scholar)) # those with scholar
papers_scholar = frame_scholar = citations_scholar = NULL
N = nrow(with_scholar)
for (k in 64:N){ ### Careful, should be 1 to N ###
  cat('k=', k, '.\r', sep='')
  # papers
  pubs = get_publications(with_scholar$scholar[k]) %>%
    unique() %>% # safety net for duplicates
    mutate(number = with_scholar$number[k]) # add researcher number
  papers_scholar = bind_rows(papers_scholar, pubs)
  
  # citations
  cites = get_citation_history(id = with_scholar$scholar[k]) %>%
    mutate(number = with_scholar$number[k]) # add researcher number
  citations_scholar = bind_rows(citations_scholar, cites)
  
  # check
  if(nrow(pubs)==0){cat('No pubs for number ', with_scholar$number[k], ', scholar', with_scholar$scholar[k], '.\n', sep='')}
  
  # for rate limiter
  Sys.sleep(10) # time in seconds
  
  # frame
  frame = data.frame(number = with_scholar$number[k], # add researcher number
                     papers = nrow(pubs))
  frame_scholar = bind_rows(frame_scholar, frame)
  
}

# final edits
papers_scholar = filter(papers_scholar, 
                        !is.na(year)) # remove missing year
## remove some dubious matches
# find gap in last two papers
gap = arrange(papers_scholar, number, year) %>%
  group_by(number) %>% 
  slice(1:2) %>% # last two papers per researcher
  select(number, year) %>%
  mutate(row=1:n()) %>%
  pivot_wider(values_from='year', names_from='row') %>%
  mutate(diff = `2` - `1`) %>% # difference between last two paper
  filter(diff >= 8) %>% # More than 8 year gap between last two
  select(number, `1`) %>%
  rename('exclude_year' = `1`)
papers_scholar = full_join(papers_scholar, gap, by='number') %>%
  mutate(exclude_year = ifelse(is.na(exclude_year), 0, exclude_year)) %>% # if missing then set exclude year to dummy
  filter(year > exclude_year) %>% # only papers after exclusion
  select(-exclude_year) # no longer needed

# save
date_collected = Sys.Date()
save(date_collected, papers_scholar, frame_scholar, citations_scholar, file='data/2_biblio_data_scholar.RData')
