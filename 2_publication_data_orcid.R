# 2_publication_data.R
# get the researchers publication and funding data
# version for ORCID
# February 2022
library(dplyr)
library(janitor)
library(rorcid)
library(stringr)
library(readxl)
source('99_functions.R')

## potential additions:
# get website from orcid? tells institution, bio[[1]]$`researcher-urls`$`researcher-url`$url.value

# API keys for orcid and scopus (not needed for scholar)
source('2_keys_not_for_sharing.R')
Sys.setenv(ORCID_TOKEN = orcid_key) # set token as an environmental variable

# from 1_read_hand_entered.R
load('data/1_researchers_post_ra.RData')

# get manually found duplicates for ORCID
duplicates = read_excel('../data/duplicates_hand_entered.xlsx') %>%
  rename('id_num' = 'doi') # always doi

## get data ##
# those with orcid
with_orcid = filter(researchers, !is.na(orcid))
N = nrow(with_orcid)
funding_orcid = papers_orcid = frame_orcid = NULL
for (k in 1:N){
  # get ORCID data:
  bio = tryCatch(orcid_bio(orcid = with_orcid$orcid[k]) ,
                 error = function(e) print(paste('No bio', with_orcid$orcid[k])))
  public = NA # start with empty
  if(class(bio) != 'character'){
    names(bio) = 'person' # change from ORCID
    public = bio$person$visibility # get whether public or not  
  }
  
  # papers, books, patents, etc
  works = orcid_works(orcid = with_orcid$orcid[k]) 
  names(works) = 'person' # change from ORCID
  works = works$person$works
  works_available = "No"
  if(nrow(works) > 0){
    works_available = "Yes"
    # process papers first
    processed = clean_names(works) 
    # had to add empty journal title due to researchers with just 1 paper that was a preprint
    if('journal_title_value' %in% names(processed) == FALSE){
      processed$journal_title_value = NA
    }
    #
    processed = select(processed, type, external_ids_external_id, publication_date_year_value, title_title_value, journal_title_value) %>% # 
      rename('external_id' = 'external_ids_external_id',
             'year' = 'publication_date_year_value',
             'title' = 'title_title_value',
             'journal' = 'journal_title_value') %>%
      filter(!is.na(year),  # must have year
             year >= 1960, # exclude small number of data entry errors
             !type %in% c('other','working-paper')) %>% # remove these types
      mutate(id_type = '', # set up blank values
             id_num = '',
             journal = my_clean_papers(journal), # to help with duplicates
             title = my_clean_papers(title))
    if(nrow(processed) > 0) {
      # get DOI, pubmed or ISBN from external ID if available
      for (j in 1:nrow(processed)){
        if(class(processed$external_id[j][[1]]) != 'data.frame'){next} # if empty then skip
        any_doi = filter(processed$external_id[j][[1]], `external-id-type`=='doi')
        if(nrow(any_doi) > 0){
          # if(nrow(any_doi) > 1){cat(j, '\n')} # check for doubles
          processed$id_type[j] = 'doi'
          processed$id_num[j] = any_doi$`external-id-value`[1] # take first, can be two for preprints
          next # do not bother with pubmed
        }
        any_pmid = filter(processed$external_id[j][[1]], `external-id-type`=='pmid')
        if(nrow(any_pmid) > 0){
          processed$id_type[j] = 'pubmed'
          processed$id_num[j] = any_pmid$`external-id-value`[1]
          next # do not bother with isbn
        }
        any_isbn = filter(processed$external_id[j][[1]], `external-id-type`=='isbn')
        if(nrow(any_isbn) > 0){
          if(nrow(any_isbn) > 1){cat(j, '\n')} # check for doubles
          processed$id_type[j] = 'isbn'
          processed$id_num[j] = any_isbn$`external-id-value`[1] # take first, can be two for preprints
        }
        
      }
    } # end of if for nrow
    #
    processed_journals = filter(processed, type=='journal-article',
             !is.na(year),  # must have year ...
             !is.na(journal)) %>% # ... and title
      select(-external_id) %>% # no longer needed
      distinct_at(vars('title'), .keep_all=TRUE) # remove duplicates, just use title as duplicates can occur by year (e.g., electronic vs print dates) and journals with slightly diffrent titles (e.g, abbreviation vs full)
    # repeat duplicates based on doi/pubmed
    if(nrow(processed_journals) > 0){
      no_doi = filter(processed_journals, is.na(id_num)) # keep all with missing
      with_doi = filter(processed_journals, !is.na(id_num)) %>%
        distinct_at(vars('id_num'), .keep_all=TRUE )
      processed_journals = bind_rows(with_doi, no_doi) # put back together
    }
  
    # process books, book chapter, edited book; journal title is book title?
    processed_books = filter(processed, str_detect(type, pattern='book'),
          tolower(title) != 'preface') %>% # exclude prefaces
      select(-external_id) %>% # no longer needed
      distinct_at(vars('title'), .keep_all=TRUE) # remove duplicates, just use title as duplicates can occur by year (e.g., electronic vs print dates) and journals with slightly diffrent titles (e.g, abbreviation vs full)
    # repeat duplicates based on isbn/doi
    if(nrow(processed_books) > 0){
      no_isbn = filter(processed_books, is.na(id_num)) # keep all with missing
      with_isbn = filter(processed_books, !is.na(id_num)) %>%
        distinct_at(vars('id_num'), .keep_all=TRUE )
      processed_books = bind_rows(with_isbn, no_isbn) # put back together
    }
    
    # process patents
    processed_patents = filter(processed, str_detect(type, pattern='patent')) %>%
      distinct_at(vars('title'), .keep_all=TRUE ) %>%
      mutate(journal = 'patent') # put patent in publication type
    
    # concatenate papers, patents, etc
    concat = bind_rows(processed_journals, processed_books, processed_patents) %>%
      select(-external_id) %>%
      mutate(number = with_orcid$number[k]) # add researcher number

    if(nrow(concat)> 0 ){
    
      # remove duplicates found by manual checking
      these_duplicates = filter(duplicates, number==number) 
      if(nrow(these_duplicates) > 0){
        concat = anti_join(concat, these_duplicates, by=c('number','type','id_num'))
      }
    
      # check for duplicates using fuzzy matches
      check = fuzzy_duplicates(intext = concat$title, distance = 0.1)
    
      # concatenate
      papers_orcid = bind_rows(papers_orcid, concat)
      
    } # end of concat if
  
  }
  
  # funding
  funding = orcid_fundings(orcid = with_orcid$orcid[k])
  names(funding) = 'person' # change from ORCID
  funding = funding$person$group$`funding-summary` # still a list
  if(length(funding) > 0) {
    funding_frame = NULL
    # get DOI, pubmed or ISBN from external ID if available
    for (j in 1:length(funding)){
      this_funding = funding[[j]]
      this_frame = data.frame(number = with_orcid$number[k], # add researcher number
                         funder = this_funding$organization.name,
                         title = this_funding$title.title.value,
                         year = as.numeric(this_funding$`start-date.year.value`) ) # note, dollars not available - although it is on the web
      funding_orcid = bind_rows(funding_orcid, this_frame)
    }
  }
  
  ## check name match with ORCID and RA data
  bio = orcid_person(orcid = with_orcid$orcid[k])
  orcid_name = paste(bio[[1]]$name$`given-names`$value, bio[[1]]$name$`family-name`$value)
  cat(orcid_name, '; ', with_orcid$name[k], '\n', sep='')
  
  ## Find Scopus ID if it is available and cross-check with our records
  scopus = NULL
  external = bio[[1]]$`external-identifiers`$`external-identifier`
  if(class(external) == 'data.frame'){
    external = clean_names(external)
    scopus = filter(external, str_detect(pattern='scopus', string = tolower(external_id_type))) %>%
      pull(external_id_value )
  }
  # back up source for Scopus
  if(is.null(scopus)){
    external = orcid_external_identifiers(with_orcid$orcid[k]) 
    names(external) = 'person'
    if(class(external$person$`external-identifier`) == 'data.frame'){
      eframe = external$person$`external-identifier` %>%
        clean_names()
      scopus = filter(eframe, str_detect(pattern='scopus', string = tolower(external_id_type))) %>%
        pull(external_id_value )
    }
  }
  # display for visual checking
  if(length(scopus>0)){
    cat('Scopus: ', paste(scopus, collapse=','), '; ', with_orcid$scopus[k], '\n', sep='')
    if(length(scopus) > 1){cat('Multiple scopus.\n')}
  }

  # overall frame of information
  frame = data.frame(number = with_orcid$number[k], # add researcher number
                     works_available = works_available,
                     public = public)
  frame_orcid = bind_rows(frame_orcid, frame)
  
  #
  cat('\n')
}

# final edits
papers_orcid = mutate(papers_orcid, 
                     year = as.numeric(year)) %>%
  filter(year > 1960, # had to add this flag for poorly entered years
   !is.na(year)) # remove missing year

# save
date_collected = Sys.Date()
save(date_collected, funding_orcid, papers_orcid, frame_orcid, file='data/2_biblio_data_orcid.RData')

