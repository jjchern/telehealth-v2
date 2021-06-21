
library(tidyverse)
library(pander)

# Tabulate effective dates by states --------------------------------------

haven::read_dta("dta/01_policy_date.dta") %>% 
  select(state, type, date) %>% 
  mutate(date = as.character(date)) %>%
  mutate(date = if_else(is.na(date), NA_character_, substr(date, 1, 7))) %>% 
  mutate(date = if_else(is.na(date), " ", date)) %>% 
  spread(type, date) %>% 
  select(state, private_parity_law, medicaid_parity_law) %>% 
  rename("State" = state,
         "Private Parity Law\nEffective Date" = private_parity_law,
         "Medicaid Parity Law\nEffective Date" = medicaid_parity_law) %>% 
  as.data.frame() %>% 
  pandoc.table(caption = "Appendix Table 1: Timelines of State Private Parity Law and Medicaid Parity Law",
               keep.line.breaks = TRUE,
               justify = "lll")
