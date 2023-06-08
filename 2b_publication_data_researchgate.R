# 2b_publication_data_researchgate.R
# combine the researchgate data from 2_publication_data_researchgate.R
# and make some edits
# March 2022
library(dplyr)
library(stringr)

# load the temporary data
files = dir('data', pattern='_temporary_')
all_data = all_frame = NULL
for (f in files){
  infile = paste('data/', f, sep='')
  load(infile)
  papers_researchgate$number = frame_researchgate$number # person number in paper file is wrong, need to change to frame
  if(frame_researchgate$papers > 0){ # only if some papers
    if(length(unique(papers_researchgate$number))>1){cat('Multiple people in papers for', f, '\n')} # check
    all_data = bind_rows(all_data, papers_researchgate)
    all_frame = bind_rows(all_frame, frame_researchgate)
    cat(f, '\n')
    cat(frame_researchgate$number, ', papers = ', nrow(papers_researchgate), ', listed = ', frame_researchgate$papers, '\n')
  }
}
# rename
papers_researchgate = all_data
frame_researchgate = all_frame

# final edits
papers_researchgate = mutate(papers_researchgate, 
                      date = as.Date(date, format='%Y/%m/%d'),
                      year = as.numeric(format(date, '%Y'))) %>%
  filter(year > 1960, # had to add this flag for poorly entered years
         #number != 14, # only seemed to start researchgate after 2016 - should not need now this data has been fixed
         !is.na(year), # remove missing year
         !str_detect(doi, pattern='10.1007/978-3-540-68706-1')) %>% # remove odd book for researcher #23 (hundreds of papers)
  arrange(number, date)

## check dates as found at least one issue with wildly wrong date
# look at gaps in dates
gaps = group_by(papers_researchgate, number) %>%
  mutate(daten = as.numeric(date),
         lag = daten - lag(daten)) %>%
  ungroup() %>%
  filter(lag > (365.25 * 6)) # more than 6 year gap
# checked numbers 20 and 30 in Scopus, they both had two career gaps

## change date for number 4 where date is definitely wrong
index = papers_researchgate$doi == '10.1109/CASSET.2004.1322956'
papers_researchgate$date[index] = as.Date("2004-06-02") # from https://ieeexplore.ieee.org/document/1322956

# save
date_collected = Sys.Date() # to fix, this is wrong, use file dates instead
save(date_collected, papers_researchgate, frame_researchgate, file='data/2b_biblio_data_researchgate.RData')

# randomly check a few people
load('data/1_researchers_post_ra.RData')
s = sample_n(papers_researchgate, 1)
s
filter(researchers, number == s$number) %>% select(number, name)

