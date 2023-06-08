# 2_publication_data_researchgate.R
# get the researchers publication data by web-scraping; had to run one at a time
# version for researchgate
# February 2022
library(dplyr)
library(janitor)
library(httr)
library(rvest)
library(stringr)
source('99_functions.R')

# from 1_read_hand_entered.R
load('data/1_researchers_post_ra.RData')

# page ends to take on to researcher`s pages for those with lots of publications
pages = c('', paste('/', 2:10 ,sep='')) # pages to add to end of profile

### get data ###
# those with researchgate page
with_gate = filter(researchers, !is.na(researchgate))
N = nrow(with_gate)
papers_researchgate = frame_researchgate = NULL
paper_count = 0
n_restart = 14 # re-start number due to crashes
n_stop = 14
for (k in n_restart:n_stop){
  
  # loop through pages
  # pages = paste('/', 9:12 ,sep='') # if things go wrong
  for(page_end in pages){
    
    # change proxy - does not work!
  #  proxy_row = k - 3
  #  source('2_proxies.R')
    
    # 1) get researchers' main page
    url = paste('https://www.researchgate.net/profile/', with_gate$researchgate[k], page_end, sep='')
    download.file(url, 'temp.html')
    researcher_page = rvest::read_html('temp.html', options=c('HUGE','NSCLEAN'))
    
    # 2) get all paper pages from links
    links = researcher_page %>% html_nodes(".nova-legacy-e-link") %>% html_attr('href') # just links
    to_loop = which(str_detect(links, pattern='www\\.researchgate\\.net\\/publication')) # find publication links
    if(length(to_loop) ==0){next} # skip this page
    paper_count = paper_count + length(to_loop) # keep a count of papers
    
    # now loop through pages
    #to_loop = to_loop[to_loop>=loop] # manual, to use when needed
    for (loop in to_loop){
      url = links[loop]
      cat(url, '\n') # temporary check
      if(str_detect(string = tolower(url), '_table')){next} # do not count tables
      if(str_detect(string = tolower(url), '_figure')){next} # do not count figures
      if(str_detect(string = tolower(url), '_supplement')){next} # do not count supplements
      if(str_detect(string = tolower(url), '_additional_file')){next} # do not count supplements
      if(str_detect(string = tolower(url), '_file_s[0-9]')){next} # do not count supplements
      download.file(url, 'temp_page.html')
      paper_page = rvest::read_html('temp_page.html', options=c('HUGE','NSCLEAN'))
      meta_nodes = html_nodes(paper_page, 'meta')
      int = html_attrs(meta_nodes) # key text is in attributes
      # DOI
      find_doi = which(str_detect(int, pattern='citation_doi')) # can ignore warnings
      if(length(find_doi) ==0){next} # if no DOI then move on
      text = int[find_doi][[1]]
      doi = as.character(text[2])
      # date
      find_d = which(str_detect(int, pattern='citation_publication_date'))
      date = NA
      if(length(find_d) > 0){
        text = int[find_d][[1]]
        date = as.character(text[2])
      }
      # title 
      find_t = which(str_detect(int, pattern='citation_title'))
      title = NA
      if(length(find_t) > 0){
        text = int[find_t][[1]]
        title = as.character(text[2])
      }
      # journal
      find_j = which(str_detect(int, pattern='citation_journal_title'))
      journal = NA
      if(length(find_j) > 0){
        text = int[find_j][[1]]
        journal = as.character(text[2])
      }
      #
      frame = data.frame(number = with_gate$number[k], doi=doi, journal=journal, title=title, date=date) # error here, number was k!
      papers_researchgate = bind_rows(papers_researchgate, frame)
      
      # needed sleep to avoid getting blocked
      Sys.sleep(60) # for one minute
    }
    
  } # end of page loop for this researcher
  
  # make overall frame
  frame = data.frame(number = with_gate$number[k], # add researcher number
                     papers = paper_count)
  frame_researchgate = bind_rows(frame_researchgate, frame)
  
  # save each researcher as system is slow and prone to crashing
  outfile = paste('data/researchgate_temporary_', k ,'.RData', sep='')
  save(frame_researchgate, papers_researchgate, file=outfile)
  
  # quick final check
  str(frame_researchgate)
  str(papers_researchgate)
  
  #
  frame_researchgate = papers_researchgate = NULL # start again
  paper_count = 0 # reset
  
} # end of big loop of researchers

str(frame_researchgate)
str(papers_researchgate)
