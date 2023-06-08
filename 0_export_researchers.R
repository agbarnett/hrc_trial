# 0_export_researchers.R
# export the data on researchers for data collection by research assistant
# June 2021
library(openxlsx)
library(dplyr)
library(stringr)
library(tidyr)

# get the data
load('data/researchers.RData') # from 0_read_data.R

# check for researchers in both consent and non-consent groups
check = select(researchers, name, consent) %>%
  unique() %>%
  group_by(name) %>%
  tally() %>%
  filter(n > 1)
doubles = filter(indata, name %in% check$name)
doubles

# Now get one result per person
one_result = arrange(researchers, surname, desc(year)) %>%
  group_by(first_name, surname) %>%
  slice(1) %>% # take most recent
  ungroup() %>%
  select(title, first_name, surname, host) %>%
  arrange(surname)

## export to excel
hs1 <- createStyle(fgFill = "seagreen3", textDecoration = "Bold", fontColour = "white") # header style
wb <- createWorkbook("Adrian Barnett")
# Add 
addWorksheet(wb, "Researchers")
writeData(wb, "Researchers", x=one_result, rowNames = FALSE, headerStyle = hs1)
setColWidths(wb, sheet = 1, cols = 1:6, widths = "auto")
saveWorkbook(wb, file = "../data/nz_researchers.xlsx", overwrite = TRUE)
