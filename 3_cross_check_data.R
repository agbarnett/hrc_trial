# 3_cross_check_data.R
# cross-check the publication and citation data with that from Research Assistant
# March 2022
library(dplyr)
library(ggplot2)
library(stringr)

## get the publication data from cites, from # 2_publication_data_[database].R
load('data/2_biblio_data_scholar.RData')
load('data/2_biblio_data_researchgate.RData') # from 2b_publication_data_researchgate.R
load('data/2_biblio_data_scopus.RData')
papers = bind_rows(papers_scholar, papers_scopus, papers_researchgate, .id='source') %>%
  mutate(source = case_when(
    source == 1 ~ 'scholar',
    source == 2 ~ 'scopus',
    source == 3 ~ 'researchgate'
  )) %>% 
  group_by(number, source) %>% # count per person
  tally() %>%
  ungroup()

## get the RA data, from 1_read_hand_entered.R
load('data/1_researchers_post_ra.RData')
remove(ra_date, researchers, other_funding) # tidy up, just keep ra_date, bibliometrics, researchers, other_funding

## merge two paper sources
ra_papers = filter(bibliometrics, type=='papers') %>%
  select(-type)
merged = full_join(papers, ra_papers, by=c('number','source')) %>%
  filter(source !='pubmed') %>%
  mutate(diff = count - n,
         av = (count+n)/2)
# plot
gplot = ggplot(data=merged, aes(x=n, y=count))+
  geom_point()+
  xlab('Database')+
  ylab('Research assistant')+
  facet_wrap(~source)
# bland-altman
baplot = ggplot(data=merged, aes(x=av, y=diff))+
  geom_point()+
  xlab('Average count')+
  ylab('Difference (RA - database)')+
  facet_wrap(~source, scales='free')
baplot

## check odd results
load('data/1_researchers_post_ra.RData')
# scholar
filter(merged, source=='scholar', diff>20)
filter(researchers, number==67)
filter(bibliometrics, number==67)
# researchgate
filter(merged, source=='researchgate', diff>50, diff<100)
filter(merged, source=='researchgate', diff>100)
num_to_check = 23 
filter(researchers, number==num_to_check)
filter(bibliometrics, number==num_to_check)
filter(papers, number==num_to_check)
f = filter(papers_researchgate, number==num_to_check) %>% arrange(date)
filter(f, str_detect(title, pattern='^Inhibition'))

# the differences for scholar (more for RA) are because scholar includes papers with missing counts
# the large differences for researchgate are hard to explain

# large differences for researchgate seem to be supplements, conference papers, book chapters, etc

## compare citations

