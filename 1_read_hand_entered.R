# 1_read_hand_entered.R
# read the hand-entered summary data on researchers (entered by the research assistant- Ollie)
# June 2022
library(dplyr)
library(stringr)
source('99_functions.R')

# word document saved as txt (no other edits made)
file = 'NZ Researchers Data collection - Complete_edited_by_AGB.txt' # version with edits by me
# remove blank lines (ignore warning)
raw = read.table(paste('../data/', file, sep=''), quote='', blank.lines.skip=TRUE, sep='~') # nonsense separator to put all data in one column
raw$V1 = str_replace_all(pattern='\\t', replacement = ' ', string=raw$V1) # replace tabs

# search for start of template
starts = which(str_detect(raw$V1, pattern='Researcher Info'))

# loop and get data
researchers = bibliometrics = other_funding = NULL
for (i in 1:(length(starts)-1)){ # ignore template, so -1
  # find start and stop
  r = starts[i]
  next_r = starts[i+1] - 1
  to_search = raw[r:next_r,]
  
  #
  note = other_places = NA
  name = raw[r-1,]
  if(name == 'TEMPLATE'){next}
  # notes from research assistant
  if(str_detect(tolower(name), '^note|appears to also be')==TRUE){
    note = raw[r-1,] # extract note
    name = raw[r-2,] # move to previous row
  }
  # other names
  if(str_detect(tolower(name), 'goes by|also go by|also known')==TRUE){
    note = raw[r-1,] # extract other names
    name = raw[r-2,] # move to previous row
  }
  # other places
  if(str_detect(tolower(name), 'also worked|no longer work')==TRUE){
    other_places = raw[r-1,] # extract other places
    name = raw[r-2,] # move to previous row
  }
  # one specific one
  if(str_detect(tolower(name), 'the two papers presented in researchgate')==TRUE){
    note = paste(raw[(r-3):(r-1),], collapse=' ') # extract notes
    name = raw[r-4,] # move to previous row
  }

  # extract name and institution
  split = str_split(name, pattern = ' [^A-Z|0-9] ')[[1]] # split on hyphen
  name = split[1]
  number = str_remove(str_split(name, pattern = ' ')[[1]][1], '[^0-9]') # number assigned by RA
  number = as.numeric(number)
  name = str_remove_all(string=name, pattern='[0-9]\\. |[0-9][0-9]\\. ')
  name = str_squish(name)
  institute = str_squish(split[2])
  
  # get gender
  index = which(str_detect(to_search, pattern='^Gender'))
  gender = str_squish(to_search[index+1])
  
  ## paper sources
  # scopus, can be multiple, e.g., researcher changed name
  scopus = my_clean(to_search = to_search, pattern='scopus\\.com')
  scopus_multiple = NULL
  if(length(scopus) > 0){
    for(w in 1:length(scopus)){
      this_scopus = str_split(scopus, pattern='authorId=')[[w]][2] # just get number
      scopus_multiple = c(scopus_multiple, this_scopus)
    }
  }
  scopus_multiple = paste(scopus_multiple, collapse = ';')
  # google scholar
  scholar = my_clean(to_search = to_search, pattern='scholar\\.google')
  scholar = str_split(scholar, pattern='user=')[[1]][2] # just get number
  scholar = str_remove_all(scholar, pattern='&hl=en|&oi=sra|&oi=ao')
  # orcid
  orcid = my_clean(to_search = to_search, pattern='orcid\\.org')
  orcid = str_split(orcid, pattern='orcid.org/')[[1]][2]
  # researchgate
  researchgate = my_clean(to_search = to_search, pattern='researchgate\\.net')
  researchgate = str_split(researchgate, pattern='researchgate.net/profile/')[[1]][2]
  # remove page number for research gate
  if(is.na(researchgate) == FALSE){
    has_page = str_detect(researchgate, pattern='/')
    if(has_page == TRUE){
      researchgate = str_split(researchgate, pattern='/')[[1]][1]
    }
  }
  
  ## social media
  # narrow to social media section as twitter can be quoted elsewhere
  index = which(str_detect(to_search, pattern='Social media handles'))
  to_search_social = to_search[index:length(to_search)]
  #cat(i, ', ', length(to_search_social), '\n', sep='') # check
  twitter = my_clean(to_search = to_search_social, pattern='twitter\\.com')
  twitter = str_split(twitter, pattern='twitter.com/')[[1]][2]
  twitter = str_remove_all(twitter, pattern='\\?lang=en')
  
  ## papers and citations estimated by RA
  ra_estimate = NULL
  # a) papers
  index = which(str_detect(to_search, pattern='No\\. papers published'))
  index_cites = which(str_detect(to_search, pattern='Total citations'))
  for (j in (index+1):(index_cites-1)){
    papers = str_squish(to_search[j])
    split = str_split(papers, ' ')[[1]]
    counts = as.numeric(str_remove_all(split[1], pattern=',')) # remove commas from numbers
    frame = data.frame(type = 'papers', count = counts, source = split[2])
    ra_estimate = bind_rows(ra_estimate, frame)
  }
  # b) citations
  index_next = which(str_detect(to_search, pattern='Total papers cited by patents|Other research funding'))
  index_next = min(index_next) # if matched to both
  for (j in (index_cites+1):(index_next-1)){
    citations = str_squish(to_search[j])
    split = str_split(citations, ' ')[[1]]
    counts = as.numeric(str_remove_all(split[1], pattern=','))
    frame = data.frame(type = 'citations', count = counts, source = split[2])
    ra_estimate = bind_rows(ra_estimate, frame)
  }
  
  ## still employed
  index = which(str_detect(to_search, pattern='Still employed\\/living'))
  index_next = which(str_detect(to_search, pattern='^Research ID')) # can be multiple lines...
  employed = str_squish(tolower(paste(to_search[(index+1):(index_next-1)], collapse=' '))) # ... so combine
  # look for key phrases
  is_employed = case_when( # create three categories
    str_detect(employed, pattern = 'present') ~ 'Confirmed',
    str_detect(employed, pattern = 'still employed') ~ 'Confirmed',
    str_detect(employed, pattern = 'appears') ~ 'Appears',
    str_detect(employed, pattern = '') ~ 'Unknown',
    str_detect(employed, pattern = 'unknown') ~ 'Unknown'
  )
  
  ## other funding
  index = which(str_detect(to_search, pattern='Other research funding'))
  index_next = which(str_detect(to_search, pattern='^Still employed'))[1] # just the first
  #cat(number, index, index_next, '\n') # check
  n_funding = 0
  if(index_next - index > 1){ # only search if there's any information
    funding_raw = str_squish(to_search[(index+1):(index_next-1)])
    n_funding = length(funding_raw) # number of grants
    for (j in 1:n_funding){
      # search for explorer grant and skip (exclude)
      is_explorer = str_locate(tolower(funding_raw[j]), pattern = '\\bexplorer\\b')
      if(any(is.na(is_explorer)==FALSE)){next}

    # flag very short text (may be missing carriage return)
      if(nchar(str_squish(funding_raw[j])) < 100){
        cat('Short result, number = ', number, ', result = ', funding_raw[j], '\n', sep='')
      }
    
      # search for dollars (one in euros, number 76; one USD number 85)
      dollars = NA
      is_dollars = str_locate(funding_raw[j], pattern = '\\$')
      if(any(is.na(is_dollars)==FALSE)){
        to_search_dollars = str_squish(str_sub(funding_raw[j], is_dollars[2]+1, nchar(funding_raw[j])))
        to_search_dollars = str_split(to_search_dollars, pattern=' ')[[1]][1] # take text straight after dollar
#        cat(number, is_dollars, to_search_dollars, '\n') # check
        to_search_dollars = str_remove_all(to_search_dollars, pattern=',')
        if(to_search_dollars != 'unknown'){dollars = as.numeric(to_search_dollars)}
        # check for euros or usd
        euros = str_detect(str_squish(tolower(funding_raw[j])), pattern = '\\beuros\\b')
        if(euros){
          cat('euros', number, '\n') # check
          dollars = dollars * 1.6 # rough exchange rate
        }
        usd = str_detect(str_squish(tolower(funding_raw[j])), pattern = '\\bus dollars\\b')
        if(usd){
          cat('us dollars', number, '\n') # check
          dollars = dollars * 1.5 # rough exchange rate
        }
        
      }
      # search for year
      year = NA
      is_year = str_locate_all(funding_raw[j], pattern='\\b19[8-9][0-9]\\b|\\b2[0-2][0-9][0-9]\\b')[[1]] # take first; just search recent decades 1980s onwards
      if(any(is.na(is_year)==FALSE)){
        is_year = is_year[1,] # first year
        year = as.numeric(str_sub(funding_raw[j], is_year[1], is_year[2]))
        #cat(number, is_year, year, '\n') # check
      }
      # make frame
      f_frame = data.frame(number = number, year = year, dollars = dollars)
      other_funding = bind_rows(other_funding, f_frame)
    
      # search for medal/prize
      medal = str_detect(tolower(funding_raw[j]), pattern = '\\bmedal\\b|\\bprize\\b')
      if(medal){cat('Warning, medal/prize in funding for ', number, '\n', sep='')}
    }
  } # end of no funding if
  
  ## store
  # a) RA
  ra_estimate = mutate(ra_estimate, 
                       source = tolower(source), # clean up source for consistency
                       source = str_remove_all(source, pattern='[^a-z]'),
                       number = number) %>%
    filter(!is.na(count)) # exclude few missing
  bibliometrics = bind_rows(bibliometrics, ra_estimate)
  # b) funding
  # c) make data frame to combine elements
  overall_frame = data.frame(number = number,
                    name = name,
                    gender = gender, 
                    institute = institute, 
                    other_places = other_places,
                    note = note,
                    scopus = scopus_multiple,
                    scholar = scholar,
                    orcid = orcid,
                    researchgate = researchgate,
                    twitter = twitter,
                    n_funding = n_funding,
                    employed = is_employed)
  researchers = bind_rows(researchers, overall_frame)
}
# two warnings about bibliometrics str_remove_all are okay

## final edits
#
researchers = mutate(researchers,
  scopus = ifelse(scopus=='NA', NA, scopus),
  institute = ifelse(institute == 'University of Auckland', 'The University of Auckland', institute),
  institute = ifelse(institute == 'the University of Auckland', 'The University of Auckland', institute),
  institute = ifelse(institute == 'University of [AB20]Auckland', 'The University of Auckland', institute))

#
bibliometrics = group_by(bibliometrics, number, type, source) %>%
  summarise(count = max(count)) %>% # one result per source (multiple for 35.)
  ungroup() %>%
  arrange(number, type, source)

## save
# date RA did searching (took a few months at part-time)
ra_date = 'December 2021'
# save data for further processing by 2_publication_data.R
save(ra_date, bibliometrics, researchers, other_funding, file='data/1_researchers_post_ra.RData')

# some checks
# source('1_read_hand_entered_check.R')
table(table(researchers$number)) # should all be 1
table(nchar(researchers$scholar)) # should all be 12
