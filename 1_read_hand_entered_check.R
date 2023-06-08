# 1_read_hand_entered_check.R
# Check that all researchers in randomised are in hand data entry
# Check RA number of papers against database entries
# January 2022
# run from 1_read_hand_entered.R

# check for any funding that is exactly 150000, might be explorer
filter(other_funding, dollars==150000)

#
load('data/1_researchers_post_ra.RData')
from_ra = select(researchers, number, name) %>%
  mutate(name_match = '', # create empty column
         name = str_remove_all(string=name, pattern='Distinguished|\\.'),
         name = str_replace_all(string=name, pattern='^Professor', replacement='Prof'), # for consistency
         name = str_replace_all(string=name, pattern='Associate Professor', replacement='Aprof'),
         name = str_replace_all(string=name, pattern='A. Prof.', replacement='Aprof'),
         name = str_replace_all(string=name, pattern='^Dr Aprof', replacement='Aprof')) 
from_ra = mutate(from_ra, name = str_replace(string=name, pattern=' [A-Z] ', replacement=' ')) # remove initials
from_ra = mutate(from_ra, name = str_replace(string=name, pattern=' [A-Z] ', replacement=' '))
load('data/0_researchers_randomised.RData') # from 0_read_data_randomised.R
# just one row per researcher
researchers = select(researchers, title, first_name, surname) %>%
  unique() %>%
  mutate(name_match = '',
         title = str_replace_all(string=title, pattern='^Professor', replacement='Prof'), # for consistency
         title = str_replace_all(string=title, pattern='Associate.?Professor', replacement='Aprof'),
         title = str_replace_all(string=title, pattern='A. Prof.', replacement='Aprof'),
         name_orignal = paste(title, first_name, surname), # make single string
         name_orignal = str_squish(name_orignal))
## fuzzy match based on names
for (i in 1:nrow(from_ra)) {
  x <- agrep(from_ra$name[i], researchers$name_orignal,
             ignore.case=TRUE, value=TRUE,
             max.distance = 0.05, useBytes = TRUE)
  x <- paste0(x, "")
  from_ra$name_match[i] <- x
} 
View(from_ra)
# check missing
filter(researchers, str_detect(string=name_orignal, 'Abbott'))
## fuzzy match based on names (reverse direction)
for (i in 1:nrow(researchers)) {
  x <- agrep(researchers$name_orignal[i], from_ra$name,
             ignore.case=TRUE, value=TRUE,
             max.distance = 0.05, useBytes = TRUE)
  x <- paste0(x, "")
  researchers$name_match[i] <- x
} 
View(researchers)
# check missing
filter(from_ra, str_detect(string=name, 'Justin'))
# (mismatches all on title)


### check that bibliometric entry from RA roughly matches observed data ###
# get paper data
load('data/2_biblio_data_scopus.RData')
load('data/2_biblio_data_scholar.RData')
load('data/2_biblio_data_orcid.RData')
load('data/2b_biblio_data_researchgate.RData')
#
all_data = NULL
N = max(frame_scopus$number)
for (k in 1:N){
this_ra= filter(bibliometrics, 
                number==k, 
                type=='papers') # cannot use yet
if(nrow(this_ra) ==0){next}
for (r in 1:nrow(this_ra)){ # loop through frames
  if(this_ra$source[r]=='scopus'){count = nrow(filter(papers_scopus, number==k))}
  if(this_ra$source[r]=='scholar'){count = nrow(filter(papers_scholar, number==k))}
  if(this_ra$source[r]=='orcid'){count = nrow(filter(papers_orcid, number==k))}
  if(this_ra$source[r]=='researchgate'){count = nrow(filter(papers_researchgate, number==k))}
  cross = 
  frame = data.frame(number=k, db=this_ra$source[r], count=count, crosscheck = this_ra$count[r])
  all_data = bind_rows(all_data, frame)
}
} # end of researcher loop
# check
library(ggplot2)
all_data = filter(all_data, db!='pubmed')
gplot = ggplot(all_data, aes(x=count, y=crosscheck))+
  geom_point()+
  geom_abline(intercept=0, slope=1)+
  facet_wrap(~db, scales='free')+
  ylab('Research Assistant')+
  xlab('Automated')+
  theme_bw()
gplot
jpeg('figures/cross_check_automated_research_assistant.jpg', width=5, height=4, units='in', res=400)
print(gplot)
dev.off()
# looks perfect for scholar and scopus
# some bad for researchgate, but that is researchers with lots of data files, tables, etc