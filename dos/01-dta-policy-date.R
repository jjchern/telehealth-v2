
# 01-dta-policy-date.R

library(tidyverse)

# Load state policy file --------------------------------------------------

readxl::read_excel("raw/TelehealthLaws.xlsx",
                   col_types = c("text", "text", "date", "text", 
                                 "text", "date", "text"),
                   skip = 1) %>% 
  select(state = State,
         private_parity_law = `...3`,
         medicaid_parity_law = `...6`) %>% 
  mutate(any_parity_law = pmin(medicaid_parity_law, private_parity_law, 
                               na.rm = TRUE)) %>% 
  gather(type, date, -state) %>% 
  arrange(state) %>% 
  mutate(state = if_else(state == "D.C.", "District of Columbia", state),
         state = if_else(state == "Winsconson", "Wisconsin", state)) %>%
  left_join(fips::state, by = "state") %>% 
  mutate(date = as.Date(date)) %>% 
  mutate(year = lubridate::year(date)) %>% 
  print() -> policy_date

haven::write_dta(policy_date, "dta/01_policy_date.dta")

# Construct state-year treatment variables --------------------------------

policy_date %>% 
  select(fips, usps, type, policy_year = year) %>%
  {right_join(., expand(., fips, year = 1999:2016))} %>% 
  # Drop years after 2016
  mutate(policy_year = if_else(policy_year >= 2016, NA_real_, policy_year)) %>% 
  mutate(post        = if_else(is.na(policy_year), FALSE, year > policy_year)) %>% 
  mutate(event_year  = if_else(is.na(policy_year), 0, year - policy_year + 1)) %>% # View
  gather(year_var, year_value, -fips, -usps, -type, -year) %>% 
  unite(type_year_var, year_var, type) %>% 
  group_by(fips, usps, year) %>% 
  arrange(fips, usps, year, type_year_var) %>% 
  spread(type_year_var, year_value) %>% 
  ungroup() %>% 
  print() -> st_yr_treatment

haven::write_dta(st_yr_treatment, "dta/01_st_yr_treatment.dta")
