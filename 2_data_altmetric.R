# 2_data_altmetric.R
# get the Altmetric data
# August 2022
library(dplyr)
library(rAltmetric)
source('98_altmetric_key.R') # not for sharing

# get the researcher data and create one DOI list per researcher
source('2_get_DOIs.R')

N_papers = nrow(for_citation)
altmetric = NULL
for (k in 1:N_papers){
  
  # call depends on id type
  if(for_citation$id_type[k] == 'doi'){
    res = tryCatch(altmetrics(doi = for_citation$id_num[k], apikey = my_altmetric_key), 
                     error = function(e) print(paste('Did not work')))
  }
  if(for_citation$id_type[k] == 'pubmed'){
    res = tryCatch(altmetrics(pmid = for_citation$id_num[k], apikey = my_altmetric_key), 
                   error = function(e) print(paste('Did not work')))
  }
  if(class(res) != 'altmetric'){next} # skip if DOI did not resolve
  
  # store results, final score and open access flag
  if(is.null(res$published_on)){
    cat('Missing date\n')
    next
  }
  frame = data.frame(number = for_citation$number[k], date = res$published_on, score = res$score, oa = res$is_oa)
  altmetric = bind_rows(altmetric, frame)
  
}
  
# convert dates
altmetric = mutate(altmetric,
                   date = as.Date(date/86400, origin = "1970-01-01"))
# add year
altmetric = mutate(altmetric,
                   year = as.numeric(format(date, '%Y')))

# save
date_collected = Sys.Date()
save(date_collected, N_papers, altmetric, file='data/2_altmetric.RData')
