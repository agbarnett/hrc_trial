# 2_citation_data_scopus.R
# get the researchers' citation data; needs to loop through each paper for each researcher
# version for scopus
## see Citation Overview API
# https://dev.elsevier.com/documentation/AbstractCitationAPI.wadl ; web page for help
# July 2022
library(dplyr)
library(rvest)
library(xml2)
library(httr)
library(jsonlite)

# key constants
start_year = 1990 # year range for citations; an early enough start to get the trajectory prior to funding
end_year = 2022

# API keys for scopus
source('2_keys_not_for_sharing.R')
rscopus::set_api_key(api_key = Elsevier_API)

# get the researcher data and create one DOI list per researcher
source('2_get_DOIs.R')

# big loop  
cites_per_year = cites_per_paper = NULL
N_papers = nrow(for_citation)
for (k in 1:N_papers){
  if(for_citation$id_type[k] == 'doi'){
    url = paste('https://api.elsevier.com/content/abstract/citations?doi=', for_citation$id_num[k], '&date=', start_year, '-', end_year, '&apiKey=', Elsevier_API, sep='')
  }
  if(for_citation$id_type[k] == 'pubmed'){
    url = paste('https://api.elsevier.com/content/abstract/citations?pubmed_id=', for_citation$id_num[k], '&date=', start_year, '-', end_year, '&apiKey=', Elsevier_API, sep='')
  }
  tried = tryCatch(download_xml(url, file='cite_temp2.xml'), # saves it in JSON format
           error = function(e) print(paste('Did not work')))
  if(tried == 'Did not work'){next} # skip if DOI did not resolve
  in_jason = fromJSON(txt='cite_temp2.xml', simplifyVector = FALSE) # read JSON
  # citations per year
  table = in_jason$`abstract-citations-response`$citeColumnTotalXML$citeCountHeader
  year = as.numeric(unlist(table$columnHeading)) # years
  cites = as.numeric(unlist(table$columnTotal)) # citations per year
  frame = data.frame(number = for_citation$number[k], year = year, cites = cites)
  cites_per_year = bind_rows(cites_per_year, frame)
  # citations per paper
  paper_id = in_jason$`abstract-citations-response`$citeInfoMatrix$citeInfoMatrixXML$citationMatrix$citeInfo[[1]]$`dc:identifier`
  pubyear = in_jason$`abstract-citations-response`$citeInfoMatrix$citeInfoMatrixXML$citationMatrix$citeInfo[[1]]$`sort-year`
  frame2 = data.frame(number = for_citation$number[k], paper = paper_id, pubyear = pubyear, cites = sum(cites))
  cites_per_paper = bind_rows(cites_per_paper, frame2)
}

## outcome 1
# get total citation numbers per researcher per year
citations = group_by(cites_per_year, number, year) %>%
  summarise(total = sum(cites)) %>%
  ungroup() %>%
  rename('n' = 'total')
# make cumulative citations over time
citations = group_by(citations, number) %>% # per researcher per database
  arrange(number, year) %>%
  mutate(cum = cumsum(n)) %>% # cumulative per person
  ungroup()

## outcome 2 - divide above by number of papers per researcher per year
# counts of papers per year
papers_per_year = mutate(cites_per_paper, pubyear = as.numeric(pubyear)) %>%
  group_by(number, pubyear) %>%
  tally() %>%
  arrange(number, pubyear) %>%
  group_by(number) %>%
  mutate(cumpapers = cumsum(n)) %>% # cumulative per person
  ungroup() %>%
  select(-n)
# merge
citations = full_join(citations, papers_per_year, 
                      by=c('number'='number', 'year'='pubyear')) %>%
  tidyr::fill(cumpapers) # fill in missing papers

# needs to be from first year for every researcher
earliest_year = group_by(cites_per_paper, number) %>%
  summarise(start = min(pubyear))
#
citations = full_join(citations, earliest_year, by='number') %>%
  filter(year >= start) %>%
  mutate(average = cum/cumpapers) %>%
  dplyr::select(-start)

# save
date_collected = Sys.Date()
save(date_collected, N_papers, citations, file='data/2_citations.RData')

# quick check
with(filter(citations, number==42), plot(year, cumpapers))
library(ggplot2)
check = ggplot(citations, aes(x=year, y=n, group=number))+
  geom_line()
check
