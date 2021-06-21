
library(tidyverse)

haven::read_dta("dta/01_policy_date.dta") %>% 
  filter(type == "private_parity_law") %>% 
  filter(year >= 1999 + 4 & year <= 2016 - 2) %>% 
  select(usps, year) %>% 
  transmute(treated_states = paste0(usps, " (", year, ")")) %>% 
  pull(1) %>% # length() # 14 Treated States: Limited pre-periods
  paste0(collapse = ", ") %>% 
  print() -> row1

haven::read_dta("dta/01_policy_date.dta") %>% 
  filter(type == "private_parity_law") %>% 
  filter(year < 1999 + 4) %>% 
  select(usps, year) %>% 
  transmute(treated_states = paste0(usps, " (", year, ")")) %>% 
  pull(1) %>% # length() # 6 Treated States: Limited pre-periods
  paste0(collapse = ", ") %>% 
  print() -> row2

haven::read_dta("dta/01_policy_date.dta") %>% 
  filter(type == "private_parity_law") %>% 
  filter(year %in% 2015:2016) %>% 
  select(usps, year) %>% 
  transmute(treated_states = paste0(usps, " (", year, ")")) %>% 
  pull(1) %>% # length() # 10 Treated States: Limited post-periods
  paste0(collapse = ", ") %>% 
  print() -> row3

haven::read_dta("dta/01_policy_date.dta") %>% 
  filter(type == "private_parity_law") %>% 
  filter(is.na(year) | year >= 2017) %>% 
  mutate(usps = if_else(!is.na(year), paste0(usps, " (", year, ")"), usps)) %>% 
  pull(usps) %>% # length() # 16 Never Treated States
  paste0(collapse = ", ") %>% 
  print() -> row4

tibble(
  `Treatment Status` = c("Selected Fourteen Treated States", 
                         "Six Treated States with Limited Pre-Period", 
                         "Ten  Treated States with Limited Post-Period", 
                         "Twenty-One Never Treated States"),
  `States` = c(row1, row2, row3, row4)
) %>% 
  as.data.frame() %>% 
  pander::pandoc.table(caption = "Table 1: Categorization of Treated States",
                       keep.line.breaks = TRUE,
                       justify = "ll")
