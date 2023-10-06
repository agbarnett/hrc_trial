# 99_functions.R
# functions for lottery study
# July 2022

# make file names for results and check if file already exists
make_file = function(infile, only_consent){
  if(only_consent == FALSE){
    ret = paste(paste(infile[1:2], collapse='/'), '.RData', sep='', collapse='')
    ret_short = paste(infile[2], '.RData', sep='', collapse='')
  }
  if(only_consent == TRUE){
    ret = paste(paste(infile[1:2], collapse='/'), '_consent.RData', sep='', collapse='')
    ret_short = paste(infile[2], '_consent.RData', sep='', collapse='')
  }
  exists = length(dir(infile[1], pattern = ret_short)) > 0
  #
  to_return = list()
  to_return$file = ret
  to_return$exists = exists
  return(to_return)
}

# standardise estimates for the fractional polynomial and scale to 0 to 1
fpc_stan = function(x, power){
  y = x^power
  y = y/max(y)
  return(y)
}

# standardise 
stan = function(x){
  y = (x - mean(x))/sd(x)
  return(y)
}

# function for rounding numbers with zeros kept
roundz = function(x, digits=0){
  dformat = paste('%.', digits, 'f', sep='')
  x = sprintf(dformat, round(x, digits))
  return(x)
}

# clean up web addresses (1_read_hand_entered.R)
my_clean = function(to_search, pattern){
  intext = which(str_detect(string=tolower(to_search), pattern = pattern))
  intext = str_squish(to_search[intext])
  intext = str_remove_all(string=intext, pattern='^\\(|\\)$')
  if(length(intext)==0){intext = NA}
  return(intext)
}

# clean up papers and journals to identify duplicates
my_clean_papers = function(intext){
  intext = tolower(intext)
  intext = str_remove_all(intext, pattern='\\<[a-z]\\>|\\<\\/[a-z]\\>') # remove subscripts etc
  intext = str_remove_all(intext, pattern='[^a-z|0-9| ]') # replace all punctuation, symbols
  intext = str_squish(intext)
  return(intext)
}

## fuzzy check of duplicated papers
fuzzy_duplicates = function(intext, distance){
  matches = NULL
  n = length(intext)
  for (i in 1:n) { # loop through every title
    to_check = intext[(i+1):n]
    match_index <- agrep(pattern = intext[i], x = to_check,
               ignore.case = TRUE, value = FALSE,
               max.distance = distance, useBytes = TRUE) # relatively short max distance
    if(length(match_index) > 0){
      frame = data.frame(text = intext[i], match = to_check[match_index])
      matches = bind_rows(matches, frame)
    }
  }
  return(matches)
} 

# altmetric function from https://github.com/karthik/rAltmetric/blob/master/R/metrics.R
# without stop
# Oct 2017
altmetrics <-
  function(oid = NULL,
           id = NULL,
           doi = NULL,
           pmid = NULL,
           arxiv = NULL,
           isbn = NULL,
           uri = NULL,
           apikey = getOption('altmetricKey'),
           foptions = list(),
           ...) {
    if (is.null(apikey))
      apikey <- '37c9ae22b7979124ea650f3412255bf9'
    
    acceptable_identifiers <- c("doi", "arxiv", "id", "pmid", "isbn", "uri")
    # If you start hitting rate limits, email support@altmetric.com
    # to get your own key.
    
    
    if (all(sapply(list(oid, doi, pmid, arxiv, isbn, uri), is.null)))
      stop("No valid identfier found. See ?altmetrics for more help", call. =
             FALSE)
    
    # If any of the identifiers are not prefixed by that text:
    if (!is.null(id)) id <- prefix_fix(id, "id")
    if (!is.null(doi)) doi <- prefix_fix(doi, "doi")
    if (!is.null(isbn)) isbn <- prefix_fix(isbn, "isbn")
    if (!is.null(uri)) uri <- prefix_fix(uri, "uri")
    if (!is.null(arxiv)) arxiv <- prefix_fix(arxiv, "arXiv")
    if (!is.null(pmid)) pmid <- prefix_fix(pmid, "pmid")
    
    # remove the identifiers that weren't specified
    identifiers <- ee_compact(list(oid, id, doi, pmid, arxiv, isbn, uri))
    
    
    # If user specifies more than one at once, then throw an error
    # Users should use lapply(object_list, altmetrics)
    # to process multiple objects.
    if (length(identifiers) > 1)
      stop(
        "Function can only take one object at a time. Use lapply with a list to process multiple objects",
        call. = FALSE
      )
    
    if (!is.null(identifiers)) {
      ids <- identifiers[[1]]
    }
    
    
    supplied_id <-
      as.character(as.list((strsplit(ids, '/'))[[1]])[[1]])
    
    # message(sprintf("%s", supplied_id))
    if (!(supplied_id %in% acceptable_identifiers))
      stop("Unknown identifier. Please use doi, pmid, isbn, uri, arxiv or id (for altmetric id).",
           call. = F)
    base_url <- "http://api.altmetric.com/v1/"
    args <- list(key = apikey)
    request <-
      httr::GET(paste0(base_url, ids), query = args, foptions)
    if(httr::status_code(request) == 404) {
      cat("No metrics found for object")
    } else {
      httr::warn_for_status(request)
      results <-
        jsonlite::fromJSON(httr::content(request, as = "text"), flatten = TRUE)
      results <- rlist::list.flatten(results)
      class(results) <- "altmetric"
      results
      
    }
  }


#' Returns a data.frame from an S3 object of class altmetric
#' @param alt_obj An object of class altmetric
#' @export
altmetric_data <- function(alt_obj) {
  if (inherits(alt_obj, "altmetric"))  {
    res <- data.frame(t(unlist(alt_obj)), stringsAsFactors = FALSE)
  }
  res
}

#' @noRd
ee_compact <- function(l)
  Filter(Negate(is.null), l)

#' @noRd
prefix_fix <- function(x = NULL, type = "doi") {
  if(is.null(x))
    stop("Some identifier required")
  
  # Check for arXiv and arxiv
  type2 <- tolower(type)
  val <- c(grep(type, x), grep(type2, x))
  
  if(any(val == 1)) {
    # lose the prefix and grab the ID
    id <-  strsplit(x, ":")[[1]][2]
    res <- paste0(tolower(type),"/", id)
  } else {
    res <- paste0(tolower(type),"/", x)
  }
  res
}

# function to count papers used by 4_compare_consent.R
count_papers = function(in_frame, in_papers, in_number, in_year){
  count = NULL
  # check if this researcher has available data
  yes = filter(in_papers, number == in_number)
  # if none then counts are missing (data will come from other databases)
  if(nrow(yes) == 0){count = data.frame(miny = NA, n = NA)}
  # if some data then count publications
  if(nrow(yes) > 0){
    count = filter(in_papers, 
                   number == in_number, # this researchers's output ...
                   year < in_year # ... prior to the year of funding
    ) 
    if(nrow(count) > 0) {
      count = summarise(count,
                        miny = min(year), # earliest year of publication
                        n = n()) # number of publications
    }
    if(nrow(count) == 0){count = data.frame(miny = in_year, n = 0)} # if no data then count is zero and years of experience is zero (so year is current year)
  }
  return(count)  
}

## function to match researchers` bibliographic data (as counts) and randomisation
# version with censoring at re-application
add_treatment_censored = function(in_random, # data frame with randomisation (treatment) information
                         in_biblio, # data frame with citation or paper counts
                         summarise_baseline = TRUE, # summarise baseline data into a covariate
                         only_consent = FALSE, # only researchers who consented
                         monthly = FALSE, # merge data based on year (FALSE) or year-month (TRUE)
                         citations = FALSE, # model of citations (if FALSE then paper counts)
                         baseline_stat = 'sum', # statistic to use in baseline summary
                         is_altmetric = FALSE, # using Altmetric
                         check_plot = FALSE){
  
  ## restrict to just consenters
  if(only_consent == TRUE){
    in_random = filter(in_random, consent=='Yes')
  }
  
  ## select merging variable of year or yrmon (for annual or monthly counts)
  if(monthly == TRUE){
    in_random = rename(in_random, 'time' = 'yrmon')
    in_biblio = rename(in_biblio, 'time' = 'yrmon')
    time_sequence = 1 / 12 # months
    xlab = 'Year and month'
  }
  if(monthly == FALSE){
    in_random = rename(in_random, 'time' = 'year')
    in_biblio = rename(in_biblio, 'time' = 'year')
    time_sequence = 1
    xlab = 'Year'
  }

  ## drop one person from monthly analysis if they do not have scopus and researchgate data
  if(monthly == TRUE){
    those_in_biblio = unique(in_biblio$number)
    those_in_random = unique(in_random$number)
    index = those_in_random %in% those_in_biblio == FALSE
    if(any(index)){
      no_scopus_or_researchgate = those_in_random [index]
      in_random = filter(in_random, number != no_scopus_or_researchgate)
    }
  }
  
  ## slim down researcher funding outcomes (fewer columns), and count cumulative funding
  in_random = dplyr::select(in_random, number, time, result) %>%
    group_by(number) %>%
    arrange(number, time) %>%
    mutate(attempt = 1:n(), # add attempt number
           funded = as.numeric(result=='Funded'),
           cfunded = cumsum(funded)) %>% # cumulative funded
    dplyr::select(-funded) %>% # drop ...
    rename('funded' = 'result') %>% # ... and rename
    ungroup()
  # who had multiple entries in the lottery
  multiple = group_by(in_random, number) %>%
    summarise(n_entries = n()) %>%
    ungroup()
  
  ### bibliographic data: 
  ## create date ranges for each researcher; earliest and latest year/yrmon for every researcher
  time_ranges = group_by(in_biblio, number) %>%
    summarise(miny = min(time),
              maxy = max(time)) %>% # should be the same for all as we are counting up to the end of the available data collection, even for those who left research
    ungroup() 
  # make counts of numbers with true zeros (avoid structural zeros)
  empty_years = dplyr::select(in_biblio, number, database) %>%
    unique() %>%  # get each database that is complete for each researcher
    left_join(time_ranges, by='number') %>% # add year ranges per researcher
    group_by(number, database) %>%
    tidyr::expand(time = seq(from=miny, to=maxy, by=time_sequence)) %>% # expand by year/yrmon ranges (per researcher)
    ungroup() %>%
    full_join(in_biblio, by=c('database','number','time')) %>%
    mutate(n = ifelse(is.na(n)==TRUE, 0, n)) # fill in missing as true zeros
  
  # transform bibliographic count data to wide, if NA that means database is not available
  wide = dplyr::select(empty_years, -denom) %>%
    pivot_wider(values_from = 'n', names_from = 'database') %>% # results from 'n'
    left_join(time_ranges, by='number') %>% # add start year/yrmon per researcher
    mutate(career_year = time - miny) %>% # create career year/yrmon
    dplyr::select(-maxy, -miny) 
  # do the same with the denominator
  wide_denom = dplyr::select(empty_years, -n) %>%
    pivot_wider(values_from = 'denom', names_from = 'database', names_prefix ='denom_')
  wide = left_join(wide, wide_denom, by=c('number','time'))
  
  ## merge biblio and randomisation data by researcher and year
  # add rounding to time to improve merge because of year and month
  in_random = mutate(in_random, time = round(time*100)/100)
  wide = mutate(wide, time = round(time*100)/100)
  #
  merged = full_join(wide, in_random, by=c('number','time')) %>%
    group_by(number) %>%
    arrange(number, time) %>%
    fill(funded, cfunded, attempt) %>% # now fill in gaps in randomisation
    ungroup() %>%
    mutate(attempt = ifelse(is.na(attempt)==TRUE, 0, attempt),
           cfunded = ifelse(is.na(cfunded)==TRUE, 0, cfunded),
           funded = ifelse(is.na(funded)==TRUE, 'Pre-randomisation', funded) # any still empty must be pre-randomisation
           ## add some other key variables - no longer used
           #yearc = (career_year+1)/10, # career year, standardised
           #yearf = (year - (min_year - 1))/10) # # make year that is positive but scaled - calendar year
    )
  
  # now account for censoring for those with repeated entries
  merged = left_join(merged, multiple, by='number')
  singles = filter(merged, n_entries == 1) # do not need to change data
  multiples = filter(merged, n_entries > 1) # change this data, with censoring
  #
  ids_to_censor = unique(multiples$number)
  new_multiples = NULL
  for (id in ids_to_censor){
    this_researcher = filter(multiples, number == id)
    n_entries = this_researcher$n_entries[1] # number of entries for this researcher
    for (entry in 1:n_entries){
      frame = filter(this_researcher, attempt <= entry) %>% # up to end of this entry
        mutate(funded = ifelse(attempt <= entry - 1, 'Pre-randomisation', funded), # everything before this attempt is pre-randomisation
               number = number + (entry/10)) # (slightly) new ID number
      new_multiples = bind_rows(new_multiples, frame)
    }
  }
  
  # put the data back together
  censored = bind_rows(singles, new_multiples) %>%
    dplyr::select(-cfunded, -n_entries) # variables no longer needed
  
  # quick check
  if(check_plot == TRUE){
    check = group_by(censored, time, funded) %>% 
      tally()
    bplot = ggplot(data=check, aes(x=time, y=n, fill=funded))+
      geom_bar(stat = 'identity', position='stack')+
      scale_fill_manual(NULL, values=c('indianred3','dodger blue','grey80'))+
      theme_bw()+
      theme(legend.position = 'top')+
      ylab('Number of researchers')+
      xlab(xlab)
    bplot
    jpeg('figures/number_treated_by_time.jpg', width=7, height=5, units='in', res=400)
    print(bplot)
    dev.off()
  } # end of if
  
  # add years since randomisation
  censored = group_by(censored, number) %>%
    arrange(number, time) %>%
    mutate(years_since = cumsum(funded == 'Funded') + cumsum(funded == 'Not Funded')) %>%
    ungroup()
  # scale years since to years if using monthly data; also add year as this is used in modelling
  if(monthly==TRUE){
    censored = mutate(censored, 
                      year = floor(time), 
                      years_since = years_since / 12) %>%
      rename('yrmon' = 'time')
  }
  if(monthly==FALSE){
    censored = rename(censored, 'year' = 'time')
  }
  
  # summarise baseline data into one number (databases used depend on model)
  if(summarise_baseline==TRUE){
    baseline = filter(censored, funded == 'Pre-randomisation')
    follow = filter(censored, funded != 'Pre-randomisation')
    #
    if(citations == TRUE & baseline_stat == 'sum'){
      summary = group_by(baseline, number) %>%
        summarise(b_scopus = sum(scopus))
    }
    #
    if(citations == TRUE & baseline_stat == 'mean' & is_altmetric == FALSE){
      summary = group_by(baseline, number) %>%
        summarise(b_scopus = mean(scopus))
    }
    #
    if(baseline_stat == 'mean' & is_altmetric == TRUE){
      summary = group_by(baseline, number) %>%
        summarise(b_altmetric = mean(altmetric))
    }
    # last result prior to randomisation
    if(citations == TRUE & baseline_stat == 'last'){
      summary = group_by(baseline, number) %>%
        arrange(number, year) %>%
        slice(n()) %>%
        rename('b_scopus' = 'scopus') %>%
        dplyr::select(number, b_scopus)
    }
    #
    if(citations == FALSE & monthly==FALSE & is_altmetric ==FALSE){
      summary = group_by(baseline, number) %>%
        summarise(b_researchgate = sum(researchgate), # total papers
                  b_scholar = sum(scholar),
                  b_scopus = sum(scopus))
    }
    if(citations == FALSE & monthly==TRUE & is_altmetric ==FALSE){
      summary = group_by(baseline, number) %>%
        summarise(b_researchgate = sum(researchgate), # total papers
                  b_scopus = sum(scopus))
    }
    summary = ungroup(summary)
    censored = left_join(follow, summary, by='number')
    # scale baseline counts to months if using monthly data
    if(citations == FALSE & monthly==TRUE){
      censored = mutate(censored,
                        b_researchgate = b_researchgate / 12,
                        b_scopus = b_scopus / 12)
    }
  }
  
  return(censored)
  
} # end of function

## function to create analysis data for employment
add_treatment_employment = function(in_random, # data frame with randomisation (treatment) information
                                    only_consent = FALSE # only researchers who consented
                                    ){
  
  ## restrict to just consenters
  if(only_consent == TRUE){
    in_random = filter(in_random, consent=='Yes')
  }
  
  # get the employment data
  load('data/1_researchers_post_ra.RData') # from 1_read_hand_entered.R
  employed = select(researchers, number, employed) %>%
    mutate(employed = as.numeric(employed == 'Confirmed')) # just look at employed vs appears/unknown
  ra_date = as.Date(paste('01 ', ra_date), format='%d %B %Y') # make into exact date (was just month and year)
  
  # set up employment record with censoring; assume employed if they re-applied for funding
  all_numbers = unique(in_random$number)
  survival = NULL
  for (num in all_numbers){
    this_researcher = filter(in_random, number==num)
    dates = c(this_researcher$date, ra_date) # string of dates
    final_outcome = filter(employed, number == num) %>% pull(employed) # get this researcher's final outcome
    outcomes = c(NA, rep(1, nrow(this_researcher)-1), final_outcome) # string of outcomes, assume `1` (still employed) if they re-applied
    for (k in 1:nrow(this_researcher)){
      frame = data.frame(
        number = num,
        start_year = this_researcher$start_year[1], # year they started in research
        funded = this_researcher$result[k], # funded or not
        time = as.numeric(dates[k+1] - dates[k])/365.25,# difference in time (in years)
        outcome = outcomes[k+1])
      survival = bind_rows(survival, frame)
    }
  }
  #
  return(survival)
  
}

## function to match researchers` bibliographic data (counts) and randomisation
# using add_treatment_censored instead
add_treatment = function(in_random, # data frame with randomisation (treatment) information
                         in_biblio, # data frame with citation or paper counts
                         max_year = 2022, 
                         check_plot = FALSE){
  
  ## slim down researcher data, and count cumulative funding
  in_random = dplyr::select(in_random, number, year, result) %>%
    group_by(number) %>%
    arrange(number, year) %>%
    mutate(attempt = 1:n(), # add attempt number
           funded = as.numeric(result=='Funded'),
           cfunded = cumsum(funded)) %>% # cumulative funded
    dplyr::select(-funded) %>% # drop ...
    rename('funded' = 'result') # ... and rename
  
  ### bibliographic data: 
  ## create date ranges for each researcher; earliest and latest year for every researcher
  years = group_by(in_biblio, number) %>%
    summarise(miny = min(year),
              maxy = max(year)) %>%
    ungroup() 
  empty_years = dplyr::select(in_biblio, number, database) %>%
    unique() %>%  # get each database that is complete for each researcher
    left_join(years, by='number') %>% # add year ranges
    group_by(number, database) %>%
    tidyr::expand(year = seq(from=miny, to=maxy, by=1)) %>% # expand by year ranges
    ungroup() %>%
    full_join(in_biblio, by=c('database','number','year')) %>%
    mutate(n = ifelse(is.na(n)==TRUE, 0, n)) # fill in missing as true zeros
  
  # transform bibliographic data to wide, if NA that means database is not available
  wide = pivot_wider(empty_years, values_from = 'n', names_from = 'database') %>%
    left_join(years, by='number') %>% # add start year per researcher
    mutate(career_year = year - miny) %>% # create career year
    dplyr::select(-maxy, -miny) 

  # to do: check for big gaps between pubs
  
  ## merge biblio and randomisation data by researcher and year
  merged = full_join(wide, in_random, by=c('number','year')) %>%
    group_by(number) %>%
    arrange(number, year) %>%
    fill(funded, cfunded, attempt) %>% # now fill in gaps in randomisation
    ungroup() %>%
    mutate(attempt = ifelse(is.na(attempt)==TRUE, 0, attempt),
           cfunded = ifelse(is.na(cfunded)==TRUE, 0, cfunded),
           funded = ifelse(is.na(funded)==TRUE, 'Pre-randomisation', funded), # any still empty must be pre-randomisation
  ## add some other key variables
      denom = ifelse(year < 2022, 1, 2/12), # denominator to adjust for shorter year in 2022
      yearc = (career_year+1)/10, # career year
      yearf = (year - (min_year - 1))/10) # # make year that is positive but scaled - calendar year
  
  # quick check
  if(check_plot == TRUE){
  check = group_by(merged, year, funded) %>% 
    tally()
  bplot = ggplot(data=check, aes(x=year, y=n, fill=funded))+
    geom_bar(stat = 'identity', position='stack')+
    scale_fill_manual(NULL, values=c('indianred3','dodger blue','grey80'))+
    theme_bw()+
    theme(legend.position = 'top')+
    ylab('Number of researchers')+
    xlab('Year')
  bplot
  jpeg('figures/number_treated_by_time.jpg', width=7, height=5, units='in', res=400)
  print(bplot)
  dev.off()
  } # end of if

  return(merged)
  
} # end of function

## function to randomly resample funding treatment
# new version, more random, random year and option for just one per person
resample_random = function(indata, 
                           earliest_year = 2000, # earliest year that funding could be won (or lost)
                           single=TRUE){
  
  # if single then just one result per researcher, this was used as a sensitivity analysis to confirm that the strange results using the complete data were due to multiple results per researcher
  if(single==TRUE){
    indata = group_by(indata, number) %>%
      sample_n(1) %>% # just one result per researcher
      ungroup()
  }
  
  # count number of winners
  n_entries = nrow(indata)
  n_success = filter(indata, result=='Funded') %>%
    dplyr::select(number) %>%
    unique() %>%
    nrow()
  p = n_success / n_entries # probability of winning funding
  # randomly sample entries and winners (no maximum on number of entries)
  numbers = unique(indata$number)
  entries = data.frame(number = sample(numbers, replace=TRUE, size=n_entries)) %>% # replace as people can enter multiple times
    mutate(funded = rbinom(size=1, n=n(), prob=p)) %>% # randomly sample winning or not
    group_by(number) %>%
    summarise(entries = n(), wins = sum(funded)) %>%
    ungroup()

  # could set a minimum for years to mimic baseline
  
  # randomly sample years, taking account of available years per person
  years = dplyr::select(researchers, number, start_year) %>%
    mutate(end_year = 2022) %>%
    pivot_longer(cols=contains('year'), values_to = "year") %>% # create all possible years per person
    group_by(number) %>%
    complete(year = min(year):max(year), fill = list(active = FALSE)) %>% 
    ungroup() %>%
    dplyr::select(-name) %>%
    filter(year >= earliest_year) %>% # do not create entries prior to this year
    left_join(entries, by='number') %>% # add entries
    filter(!is.na(entries)) %>% # drop non-entries
    group_by(number) %>%
    mutate(runif = runif(n())) %>% # random number to order rows
    arrange(number, runif) %>%
    mutate(row = 1:n()) %>% # take top that are randomly ordered, avoids duplicate years per person
    filter(row <= entries) %>%
    mutate(result = ifelse(row <= wins, 'Funded', 'Not Funded')) %>% # now randomly create wins - matching original format
    dplyr::select(-runif, -row, -entries, -wins)

  # add person information
  small = dplyr::select(indata, number, name) %>%
    unique()
  res = left_join(years, small, by='number') %>%
    arrange(number, year)
  return(res)
} # end of function

## function to randomly resample funding treatment
# new version, perturb year as well - still not working
resample_random_old2 = function(indata){
  # resample
  res = dplyr::select(indata, result) %>%
    mutate(random = runif(n())) %>% # randomly re-order
    arrange(random) %>%
    dplyr::select(-random) %>%
    mutate(year_change = round(runif(n=n(), min=-3.5, max=3.5)))
  indata = dplyr::select(indata, -result) %>% # remove from data
    rename('year_old' = 'year')
  indata = bind_cols(res, indata) # add random ordered data
  # avoid double funding outcomes in same year for same person
  indata = group_by(indata, number, year_old) %>%
    arrange(number, year_old, result) %>%
    slice(1) %>% # just one result per person per year (take funded as per 0_read_data_randomised.R)
    ungroup() %>%
    mutate(year = year_old + year_change, # peturb year as well
           year = ifelse(year>=2024, 2021, year)) %>% # cannot be beyond last funded year
    dplyr::select(-starts_with('year_')) 
  return(indata)
} # end of function

## function to randomly resample funding treatment
# old version, abandoned as it included some signal, possibly via multiple results for the same person?
resample_random_old = function(indata){
  # resample funding to keep same marginal distribution
  res = dplyr::select(indata, result) %>%
    mutate(random = runif(n())) %>% # randomly re-order
    arrange(random) %>%
    dplyr::select(-random)
  indata = dplyr::select(indata, -result) # remove funding from data
  indata = bind_cols(res, indata) # add randomly ordered data
  # avoid double funding outcomes in same year for same person
  indata = group_by(indata, number, year) %>%
    arrange(number, year, result) %>%
    slice(1) %>% # just one result per person per year (take funded as per 0_read_data_randomised.R)
    ungroup()
  return(indata)
} # end of function

## function to make paper counts per year
# fill in empty years after last paper
make_counts = function(indata, 
                       in_dates) # for denominator
  {
  
  # maximum year
  max_year = as.numeric(format(max(in_dates$dates),'%Y'))
  
  ## make frame with all possible years for all researchers and database(s)
  # get starting year for each researcher
  start = group_by(indata, number) %>%
    summarise(start = min(year)) 
  expanded = NULL
  for (k in 1:nrow(start)){ # loop through researchers
    databases = filter(indata, number == start$number[k]) %>%
      dplyr::select(database) %>%
      unique() %>%
      pull(database)
    for (d in databases){ # have to loop through databases too
      f = data.frame(number = start$number[k], database = d, year = start$start[k]:max_year) # from their start to latest year
      expanded =  bind_rows(expanded, f)
    }
  }
  # make yearly counts
  counts = group_by(indata, number, database, year) %>%
    tally() %>%
    ungroup()
  
  # add in full potential range of data to observed counts
  outcounts = full_join(counts, expanded, by=c('number','year','database')) %>%
    mutate(n = ifelse(is.na(n), 0, n)) %>% # replace zero counts
    arrange(number, database, year)
  
  # add database-dependent denominator (fraction of year in which publication data was collected)
  in_dates = mutate(in_dates,
                    year = as.numeric(format(max(dates),'%Y')),
                    yrfrac = season::yrfraction(dates)) %>%
    dplyr::select(-dates)
  outcounts = left_join(outcounts, in_dates, by=c('database','year')) %>%
    mutate(denom = ifelse(is.na(yrfrac)==TRUE, 1, yrfrac)) %>%
    dplyr::select(-yrfrac)
  
  return(outcounts)

}

## function to make paper counts per year/month
make_counts_monthly = function(indata, in_dates){
  
  # add year/month
  indata = mutate(indata, 
                 month = as.numeric(format(date,'%m')),
                 yrmon = year + ((month-1)/12))
  
  # maximum year/month
  max_year_month = as.numeric(format(max(in_dates$dates),'%Y')) +
    (as.numeric(format(max(in_dates$dates),'%m'))-1)/12
  
  ## make frame with all possible years for all researchers and database(s)
  # get starting year for each researcher
  start = group_by(indata, number) %>%
    summarise(start = min(yrmon)) %>%
    ungroup()
  expanded = NULL
  for (k in 1:nrow(start)){ # loop through researchers
    databases = filter(indata, number == start$number[k]) %>%
      dplyr::select(database) %>%
      unique() %>%
      pull(database)
    for (d in databases){ # have to loop through databases too
      f = data.frame(number = start$number[k], database = d, yrmon = seq(start$start[k], max_year_month, 1/12)) %>% # from their start to latest year
        mutate(year = floor(yrmon),
               month = round(1+(yrmon - year)*12))
      expanded =  bind_rows(expanded, f)
    }
  }
  # make counts by year and month
  counts = group_by(indata, number, database, year, month, yrmon) %>%
    tally() %>%
    ungroup()
  
  # add in full potential range of data to observed counts
  outcounts = full_join(counts, expanded, by=c('number','year','month','yrmon','database')) %>%
    mutate(n = ifelse(is.na(n), 0, n)) # replace zero counts

  # add offset to adjust for unequal months
  denom = season::flagleap(data = dplyr::select(outcounts, -yrmon), report=FALSE)
  outcounts = left_join(outcounts, denom, by=c('year','month')) %>%
    mutate(denom = ndaysmonth/30) %>% # to standard 30 day month
    dplyr::select(-ndaysmonth) %>%
    arrange(number, database, yrmon)
  
  return(outcounts)
  
}

## make nicer bugs results
make_bugs_results = function(in_bugs){
  table = in_bugs$summary[,c(1,3,7)]
  table = data.frame(table)
  names(table) = c('mean','lower','upper')
  table$parameter = row.names(table)
  row.names(table) = NULL
  
  ## p-values
  pos = in_bugs$sims.matrix>0
  pos = colMeans(pos)
  pos.dash = 1 - pos
  pvals = pmax(2*pmin(pos, pos.dash), 1/(2*MCMC))
  table$pvalue = pvals
  
  #
  return(table)
}

## parameter rename for tables of parameter estiamtes
parameter_rename = function(x, papers=TRUE, monthly=FALSE){
  case_when(
    x == 'zeta' ~ 'January',
    x == 'beta' & papers==TRUE ~ 'Double pre-randomisation papers',
    x == 'beta' & papers==FALSE ~ 'Double pre-randomisation citations',
    x == 'eta' ~ 'Year = 2022',
  x == 'gamma' ~ 'Funding',
  x == 'years_since' ~ 'Years since randomisation',
  x == 'interaction' ~ 'Funding by year interaction',
  x == 'delta' ~ 'Change by years since funding for not funded',
  x == 'lambda' ~ 'Change by years since funding for funded',
  x == 'slope.diff' ~ 'Difference in change estimates (funded minus not funded)',
  x == 'alpha[1]' ~ 'Intercept for Scopus', 
  x == 'alpha[2]' ~ 'Intercept for Google scholar',
  x == 'alpha[3]' ~ 'Intercept for researchgate')
}
