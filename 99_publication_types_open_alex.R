# 99_publication_types_open_alex.R
# get the publication types for all DOIs for researchgate and scopus
# October 2023
library(openalexR)
library(dplyr)
options(openalexR.mailto = "a.barnett@qut.edu.au")

# get the bibliographic data
load('data/2b_biblio_data_researchgate.RData')
load('data/2_biblio_data_scopus.RData')
#load('data/2_biblio_data_scholar.RData') # does not have DOIs

# make combined dataset, just use DOI
combined = bind_rows(papers_researchgate,
                     papers_scopus) %>%
  select(doi) %>%
  unique()
# make start and stop numbers
N = nrow(combined)
max_request = 50
start = floor(seq(1, N, length.out = N/max_request))
stop = c(start-1, N)
stop = stop[-1]

# loop
results = NULL
for (k in 1:length(start)){
  dois = combined$doi[start[k]:stop[k]]
  oa_result = oa_fetch(entity = 'works', 
                     doi = dois,
                     count_only = FALSE,
                     verbose = TRUE)
  results = bind_rows(results, oa_result)
  Sys.sleep(60) #
}

# save for time reasons
save(results, file='data/99_open_alex.RData')

# make table for paper
library(tableone)
# summarise host organisation into top ten
top_five = filter(results, !is.na(host_organization)) %>%
  group_by(host_organization) %>%
  tally() %>%
  arrange(-n) %>%
  slice(1:5) %>%
  ungroup() %>%
  pull(host_organization)
results = mutate(results, 
                top_five = case_when(
                  is.na(host_organization) ~ 'NA',
                  host_organization %in% top_five ~ host_organization,
                  .default = 'Other'
                ),
                english = language=='en')

# variables to use, 'host_organization' = too many
vars <- c("publication_year", 'type','top_five','is_oa','english')
# Categorical variables
catVars <- c('type','is_oa','top_five','english')
# Create a TableOne object
tab1 <- CreateTableOne(data = results, 
                       vars = vars, 
                       factorVars = catVars, 
                       test = FALSE, 
                       includeNA = TRUE,
                       addOverall = FALSE)
#flextable(data.frame(tab1))
print(tab1, 
      catDigits = 1, # will no longer allow 0
      contDigits = 1, 
      explain = TRUE, 
      nonnormal = 'publication_year',
      showAllLevels = TRUE,
      formatOptions = list(big.mark = ","))
