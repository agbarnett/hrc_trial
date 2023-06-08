# 99_dumped.R

## function to match researchers` bibliographic data (counts) and randomisation
# version without censoring
add_treatment = function(in_random, # data frame with randomisation (treatment) information
                         in_biblio, # data frame with citation or paper counts
                         max_year = 2022, 
                         check_plot = FALSE){
  
  ## slim down researcher data, and count cumulative funding
  in_random = select(in_random, number, year, result) %>%
    group_by(number) %>%
    arrange(number, year) %>%
    mutate(attempt = 1:n(), # add attempt number
           funded = as.numeric(result=='Funded'),
           cfunded = cumsum(funded)) %>% # cumulative funded
    select(-funded) %>% # drop ...
    rename('funded' = 'result') # ... and rename
  
  ### bibliographic data: 
  ## create date ranges for each researcher
  # earliest and latest year for every researcher
  years = group_by(in_biblio, number) %>%
    summarise(maxy = max(year),
              miny = min(year)) %>%
    ungroup() 
  empty_years = select(in_biblio, number, database) %>%
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
    select(-maxy, -miny) 
  
  # to do: check for big gaps between pubs
  
  ## merge biblio and randomisation data by researcher and year
  merged = full_join(wide, in_random, by=c('number','year')) %>%
    group_by(number) %>%
    arrange(number, year) %>%
    fill(funded, cfunded, attempt) %>% # now fill in gaps in randomisation and attempt number
    ungroup() %>%
    mutate(attempt = ifelse(is.na(attempt)==TRUE, 0, attempt),
           cfunded = ifelse(is.na(cfunded)==TRUE, 0, cfunded),
           funded = ifelse(is.na(funded)==TRUE, 'Pre-randomisation', funded)) # any still empty must be pre-randomisation
  
  ## add some other key variables
  merged = mutate(merged,
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


### from INLA
#lc = inla.make.lincomb(treatment = 1, control = -1)

## from 4_power_simulation.Rmd


### Compare researchers who were randomly funded and not in the simulation

```{r}
randomly_funded = NULL
for (s in 1:n_sim){
  numbers = filter(simulated[[s]], funded == 'Funded') %>% 
    select(number) %>%
    mutate(sim = s)
  randomly_funded = bind_rows(randomly_funded, numbers)
}
# add real funding
any_funding = filter(researchers, result == 'Funded') %>%
  select(number) %>%
  unique() %>%
  mutate(any_funding = 'Yes')
randomly_funded = left_join(randomly_funded, any_funding, by='number') %>%
  mutate(any_funding = ifelse(is.na(any_funding)==TRUE, 'No', any_funding))
#
gplot = ggplot(data=randomly_funded, aes(x=number, fill=any_funding))+
  scale_fill_manual('Won funding', values=c('grey55','darkorange2'))+
  geom_bar()+
  theme_bw()+
  xlab('Researcher number')+
  ylab('Number of times selected in simulation')
gplot
```



# from 4_summary_data.Rmd

## Gender

```{r}
tab = tabyl(researchers, gender) %>%
  janitor::adorn_totals() %>%
  mutate(percent = roundz(percent*100))
ftab = flextable(tab) %>%
  theme_box() %>%
  autofit
ftab
```


## from 5_main_analysis.Rmd

###### page break

# Other funding

## funding data
analysis_ready_funding = add_treatment_censored(in_random = researchers, 
                                                only_consent = only_consent,
                                                is_altmetric = TRUE,
                                                monthly = FALSE, 
                                                baseline_stat = 'mean', 
                                                in_biblio = funding_total)
# rename back and fix few missing
analysis_ready_funding = rename(analysis_ready_funding, 'funding' = 'altmetric') %>%   mutate(
  funding = ifelse(is.na(funding )==TRUE, 0, funding)) %>%
  select(-b_altmetric) # do not need


```{r}
file = c('bayes', 'funding')
outfile = make_file(file, only_consent = only_consent)
if(outfile$exists == FALSE){
  # model choices:
  pilot = FALSE # run big model
  source('99_nimble_funding.R')
  save(results_funding, file = outfile$file)
}
if(outfile$exists == TRUE){
  load(outfile$file)
}
```

```{r}
to_table = filter(results_funding$table, 
                  str_detect(parameter, pattern='gamma')) %>% # do not show intercept
  mutate(term = parameter_rename(parameter, papers=FALSE),
         pvalue = format.pval(pvalue, digits=2, eps=0.0001),
         mean = 100*(exp(mean)- 1), # percent change
         lower = 100*(exp(lower)- 1),
         upper = 100*(exp(upper)- 1),
         mean = roundz(mean, 1), # rounding
         lower = roundz(lower, 1),
         upper = roundz(upper, 1),
         ci = paste(lower , ' to ' , upper, sep='')) %>%
  dplyr::select(term, mean, ci, pvalue)
ftab = flextable(to_table)%>%
  autofit() %>%
  theme_box()
ftab
```

### Residual checks (funding)

```{r, fig.width=8}
res = results_funding$residuals
hplot = ggplot(data=res, aes(x=residual))+
  geom_histogram(col='grey44', fill='skyblue')+
  g.theme+
  xlab('Residual (dollars)')+
  ylab('Count')
hplot
```

