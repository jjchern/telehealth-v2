
# dos/02-dta-ever-any-parity-law.R

library(tidyverse)

# Prepare ever any parity law ---------------------------------------------

haven::read_dta("dta/01_policy_date.dta") %>% 
  filter(type == "any_parity_law") %>% 
  mutate(year2015 = if_else(year <= 2016, 1L, NA_integer_)) %>% 
  mutate(yes = if_else(!is.na(year2015), "Ever", "Never")) %>% 
  select(usps, yes) %>% 
  print() -> ever_any_parity_law

haven::write_dta(ever_any_parity_law, "dta/02_ever_any_parity_law.dta")
