# 0_pubmed_search
# pubmed search for DCE project
# January 2023
library(rentrez)

# dates
years = '2012:2023'
dates = paste(years,'[pdat]', sep='')

# Authorship 
mesh_words = c("Authorship", "Journal Impact Factor", "Publishing", "Career Mobility")
authorship = paste(mesh_words, '[MeSH]', sep='')
authorship = paste(authorship, collapse=' OR ')

# reward - "evaluation" and `impact` are too vague?
reward_words = c('reward','preference','incentive','prestige','hiring','policy','promotion',"'research output'","'research impact'")
reward = paste(reward_words, '[tiab]', sep='')
reward = paste(reward, collapse=' OR ')

# other
other_words1 = c("bibliometric*", "scientometric*", "peer review")
other_words2 = c('preprint', 'pre-print')
other_words1a = paste(other_words1, '[tiab]', sep='')
other_words1b = paste(other_words1, '[MeSH]', sep='')
other_words2a = paste(other_words2, '[tiab]', sep='')
other_words2b = paste(other_words2, '[All Fields]', sep='')
other = paste(c(other_words1a, other_words1b, other_words2a, other_words2b), collapse=' OR ')

# create queries
query1 = paste(dates, ' AND (', authorship, ')', sep='')
query2 = paste(dates, ' AND (', reward, ')', sep='')
query3 = paste(dates, ' AND (', other, ')', sep='')
query_123 = paste(dates, ' AND (', authorship, ') AND (', reward, ') AND (', other, ')', sep='')

# make searchers
search1 = entrez_search(db = 'pubmed', term = query1, retmax=40000)
search2 = entrez_search(db = 'pubmed', term = query2, retmax=40000)
search3 = entrez_search(db = 'pubmed', term = query3, retmax=40000)
search_123 = entrez_search(db = 'pubmed', term = query_123, retmax=40000)

# random selection of 20 titles
n_random = 20
pmids = sample(search_123$ids, size = n_random, replace = FALSE)
records = entrez_fetch(db="pubmed", id = pmids, rettype="xml", parsed=TRUE) # api_key = my.api.key
parsed = parse_pubmed_xml(records) # parse from XML
titles = NULL
for (k in 1:n_random){
  this_title = parsed[[k]]$title
  titles = c(titles, this_title)
}

#
must_have = as.character(c(25837969,27964810))
must_have %in% search_123$ids
