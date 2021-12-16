
library(tidyverse)

# Prepare county year mortality rates -------------------------------------

haven::read_dta("dta/05-deaths-pop-county.dta") %>% 
  mutate(death_rate = deaths / pop * 100000) %>% # filter(county_code == "19103")
  select(usps, county_code, year, type, death_rate) %>% 
  mutate(type = paste0("dr_", type)) %>% 
  spread(type, death_rate) %>% 
  arrange(county_code, year) %>% # group_by(county_code) # 3147
  rename(dr_non_trans_acc = `dr_non-trans_acc`) %>% 
  print() -> cnty_yr_death_rate

haven::read_dta("dta/05-deaths-pop-county.dta") %>% 
  mutate(r_death_rate = deaths_r / pop * 100000) %>% # filter(county_code == "19103")
  select(usps, county_code, year, type, r_death_rate) %>% 
  mutate(type = paste0("r_dr_", type)) %>% 
  spread(type, r_death_rate) %>% 
  arrange(county_code, year) %>% # group_by(county_code) # 3147
  rename(r_dr_non_trans_acc = `r_dr_non-trans_acc`) %>% 
  print() -> cnty_yr_r_death_rate

full_join(cnty_yr_death_rate, cnty_yr_r_death_rate) %>% 
  print() -> cnty_yr_death_rate

haven::write_dta(cnty_yr_death_rate, "dta/07_cnty_yr_death_rate.dta")

# Prepare under-65 county year mortality rates ----------------------------

haven::read_dta("dta/05-deaths-pop-county-under-65.dta") %>% 
  mutate(death_rate = deaths / pop * 100000) %>% # filter(county_code == "19103")
  select(usps, county_code, year, type, death_rate) %>% 
  mutate(type = paste0("dr_", type, "_u65")) %>% 
  spread(type, death_rate) %>% 
  arrange(county_code, year) %>% # group_by(county_code) # 3147
  # rename(dr_non_trans_acc = `dr_non-trans_acc`) %>% 
  print() -> cnty_yr_death_rate_under_65

haven::write_dta(cnty_yr_death_rate_under_65, "dta/07_cnty_yr_death_rate_under_65.dta")

# Prepare age3564 county year mortality rates ----------------------------

haven::read_dta("dta/05-deaths-pop-county-age3564.dta") %>% 
  mutate(death_rate = deaths / pop * 100000) %>% # filter(county_code == "19103")
  select(usps, county_code, year, type, death_rate) %>% 
  mutate(type = paste0("dr_", type, "_3564")) %>% 
  spread(type, death_rate) %>% 
  arrange(county_code, year) %>% # group_by(county_code) # 3147
  # rename(dr_non_trans_acc = `dr_non-trans_acc`) %>% 
  print() -> cnty_yr_death_rate_age3564

haven::write_dta(cnty_yr_death_rate_age3564, "dta/07_cnty_yr_death_rate_age3564.dta")

# Prepare state year mortality rates --------------------------------------

haven::read_dta("dta/05-deaths-pop-state.dta") %>% 
  mutate(death_rate = deaths / pop * 100000) %>% 
  left_join(fips::state, by = c("state_code" = "fips")) %>% 
  select(usps, year, type, death_rate) %>% 
  mutate(type = paste0("dr_", type)) %>% 
  spread(type, death_rate) %>% 
  arrange(usps, year) %>% # group_by(usps) # 51
  rename(dr_non_trans_acc = `dr_non-trans_acc`) %>% 
  print() -> st_yr_death_rate

haven::write_dta(st_yr_death_rate, "dta/07_st_yr_death_rate.dta")

# Prepare under-65 state year mortality rates -----------------------------

haven::read_dta("dta/05-deaths-pop-state-under-65.dta") %>% 
  mutate(death_rate = deaths / pop * 100000) %>% 
  filter(!is.na(year)) %>% 
  left_join(fips::state, by = c("state_code" = "fips")) %>% 
  select(usps, year, type, death_rate) %>% 
  mutate(type = paste0("dr_", type, "_u65")) %>% 
  spread(type, death_rate) %>% 
  arrange(usps, year) %>% # group_by(usps) # 51
  print() -> st_yr_death_rate_under_65

haven::write_dta(st_yr_death_rate_under_65, "dta/07_st_yr_death_rate_under_65.dta")

# Prepare state 13ur year mortality rates --------------------------------------

haven::read_dta("dta/05-deaths-pop-ur13.dta") %>% # group_by(state_code, ur13_code) # 252
  mutate(death_rate = deaths / pop * 100000) %>% 
  filter(!is.na(year)) %>% 
  left_join(fips::state, by = c("state_code" = "fips")) %>% 
  select(usps, ur13, ur13_code, year, type, death_rate) %>% 
  mutate(type = paste0("dr_", type)) %>% 
  spread(type, death_rate) %>% 
  arrange(usps, ur13_code, year) %>% # group_by(usps, ur13) # 252
  print() -> ur13_yr_death_rate

haven::write_dta(ur13_yr_death_rate, "dta/07_ur13_yr_death_rate.dta")

haven::read_dta("dta/05-deaths-pop-ur13.dta") %>% # group_by(state_code, ur13_code) # 252
  mutate(death_rate = deaths / pop * 100000) %>% 
  # Remove series with any missing values ------------
  group_by(state_code, ur13_code, type) %>% 
  mutate(any_na = anyNA(deaths)) %>% 
  filter(any_na == FALSE) %>% 
  select(-any_na) %>% 
  ungroup() %>% 
  left_join(fips::state, by = c("state_code" = "fips")) %>% 
  select(usps, ur13, ur13_code, year, type, death_rate) %>% 
  mutate(type = paste0("dr_", type)) %>% 
  spread(type, death_rate) %>% 
  arrange(usps, ur13_code, year) %>% # group_by(usps, ur13) # 252
  print() -> ur13_yr_death_rate

haven::write_dta(ur13_yr_death_rate, "dta/07_ur13_yr_death_rate_na_rm.dta")
