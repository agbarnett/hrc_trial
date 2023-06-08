# 0_read_ria_emails.R
# read in the RIA email data entered by David
# December 2022
library(janitor)
library(dplyr)
library(readxl)
library(stringr)

# institutes
institutes = read_excel('data/RIA-main_22-12-2022.xlsx', sheet = 'all_institutes') %>%
  clean_names() %>%
  select(-starts_with('x')) %>%
  filter(!str_detect(website, pattern = '^Same as')) # remove six duplicates institutes


# Research Intergrity Advisors
rias = read_excel('data/RIA-main_22-12-2022.xlsx', sheet = 'contact_details') %>%
  clean_names()

## check the data
# emails without at symbol - should be no rows
filter(rias, 
       !is.na(email),
       !str_detect(email, '@')) # 
filter(institutes, 
       rias_available  == FALSE, # only need email where there's no public data
       !is.na(email),
       !str_detect(email, '@')) # 
# emails without au
filter(rias, 
       !is.na(email),
       !str_detect(email, '\\.au')) %>% # 
  select(inst, email)

# save
save(institutes, rias, file = 'data/0_emails.RData')
