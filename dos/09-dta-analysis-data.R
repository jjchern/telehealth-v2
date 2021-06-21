
library(tidyverse)
library(haven)

# County level analysis data ----------------------------------------------

read_dta("dta/06-cnty-yr-pop.dta") %>% 
  left_join(read_dta("dta/07_cnty_yr_death_rate.dta")) %>% 
  left_join(read_dta("dta/08_state_covariates.dta")) %>% 
  left_join(read_dta("dta/08_county_covariates.dta")) %>% 
  left_join(read_dta("dta/01_st_yr_treatment.dta")) %>% 
  left_join(read_dta("dta/02_ever_any_parity_law.dta"), by = "usps") %>% 
  select(fips, usps, county_code, year, yes, pop, everything()) %>% # View("analysis-data")
  # drop labels for `hpsa_pcp_10`, `hpsa_pcp_15`, `hpsa_psy_10`, `hpsa_psy_15`
  # zap_labels() %>% 
  print() -> analysis_data

haven::write_dta(analysis_data, "dta/09_analysis_data_county.dta")

# State level analysis data -----------------------------------------------
# Later: Construct state-level covariates directly

read_dta("dta/06-cnty-yr-pop.dta") %>% 
  left_join(read_dta("dta/07_cnty_yr_death_rate.dta")) %>% 
  select(-contains("dr_")) %>% 
  left_join(read_dta("dta/08_county_covariates.dta")) %>% 
  # So far, county-year level covariates. 
  # Next: produce state-year level covariates
  group_by(usps, year) %>% 
  # filter(usps == "AK" & year == 1999) %>% print(n = 32) %>% slice(27:28) %>% 
  # Drop county-year with missing pop, so that weighted.mean() can be used
  filter(!is.na(pop)) %>% 
  summarise(tot_pop = sum(pop, na.rm = TRUE),
            tot_active_md_p100k_appx = weighted.mean(tot_active_md_p100k_appx, pop, na.rm = TRUE),
            cty_unem_rate            = weighted.mean(cty_unem_rate, pop, na.rm = TRUE),
            median_hh_inc            = weighted.mean(median_hh_inc, pop, na.rm = TRUE),
            pov_rates                = weighted.mean(pov_rates, pop, na.rm = TRUE),
            pop_density_2010         = weighted.mean(pop_density_2010, pop, na.rm = TRUE),
            median_age_appx          = weighted.mean(median_age_appx, pop, na.rm = TRUE),
            pct_black_appx           = weighted.mean(pct_black_appx, pop, na.rm = TRUE),
            pct_white_appx           = weighted.mean(pct_white_appx, pop, na.rm = TRUE),
            pct_female_appx          = weighted.mean(pct_female_appx, pop, na.rm = TRUE),
            pct_ba_degree_appx       = weighted.mean(pct_ba_degree_appx, pop, na.rm = TRUE),
            pct_hs_or_less_appx      = weighted.mean(pct_hs_or_less_appx, pop, na.rm = TRUE)) %>% 
  left_join(read_dta("dta/08_state_covariates.dta")) %>% 
  left_join(read_dta("dta/01_st_yr_treatment.dta")) %>% 
  left_join(read_dta("dta/02_ever_any_parity_law.dta"), by = "usps") %>% 
  left_join(read_dta("dta/07_st_yr_death_rate.dta")) %>% 
  select(usps, year, yes, tot_pop, contains("dr_"), everything()) %>% # View("analysis-data")
  print() -> analysis_data

haven::write_dta(analysis_data, "dta/09_analysis_data_state.dta")

# State and 2013 ur code level analysis data ------------------------------

fips::nchs_urc %>% 
  select(usps, county_code = fips, ur13_code = code2013) %>% 
  haven::zap_labels() %>% 
  print() -> ur13_to_county

haven::read_dta("dta/07_ur13_yr_death_rate.dta") %>% 
  group_by(usps, ur13_code)

haven::read_dta("dta/06-cnty-yr-pop.dta") %>% 
  left_join(haven::read_dta("dta/07_cnty_yr_death_rate.dta")) %>% 
  select(-contains("dr_")) %>% 
  left_join(ur13_to_county) %>% 
  left_join(haven::read_dta("dta/08_county_covariates.dta")) %>% 
  haven::zap_label() %>%
  haven::zap_labels() %>% 
  mutate(ur13_code = as.numeric(ur13_code)) %>% 
  # Drop county-year with missing pop, so that weighted.mean() can be used
  filter(!is.na(pop)) %>% 
  # So far, county-year level covariates. Next: produce state-ur13-year level covariates
  group_by(usps, ur13_code, year) %>% 
  summarise(tot_pop                  = sum(pop, na.rm = TRUE),
            tot_active_md_p100k_appx = weighted.mean(tot_active_md_p100k_appx, pop, na.rm = TRUE),
            cty_unem_rate            = weighted.mean(cty_unem_rate, pop, na.rm = TRUE),
            median_hh_inc            = weighted.mean(median_hh_inc, pop, na.rm = TRUE),
            pov_rates                = weighted.mean(pov_rates, pop, na.rm = TRUE),
            pop_density_2010         = weighted.mean(pop_density_2010, pop, na.rm = TRUE),
            median_age_appx          = weighted.mean(median_age_appx, pop, na.rm = TRUE),
            pct_black_appx           = weighted.mean(pct_black_appx, pop, na.rm = TRUE),
            pct_white_appx           = weighted.mean(pct_white_appx, pop, na.rm = TRUE),
            pct_female_appx          = weighted.mean(pct_female_appx, pop, na.rm = TRUE),
            pct_ba_degree_appx       = weighted.mean(pct_ba_degree_appx, pop, na.rm = TRUE),
            pct_hs_or_less_appx      = weighted.mean(pct_hs_or_less_appx, pop, na.rm = TRUE)) %>% 
  ungroup() %>% 
  # Compare with population numbers from WONDER: In the future
  left_join(haven::read_dta("dta/01_st_yr_treatment.dta")) %>% 
  left_join(haven::read_dta("dta/02_ever_any_parity_law.dta"), by = "usps") %>% 
  # Without dropping any areas with missing values
  # left_join(haven::read_dta("dta/07_ur13_yr_death_rate.dta")) %>% 
  # Drop areas with any missing values
  left_join(haven::read_dta("dta/07_ur13_yr_death_rate_na_rm.dta")) %>% 
  select(usps, ur13, ur13_code, year, yes, tot_pop, contains("dr_"), everything()) %>% # View("analysis-data")
  print() -> analysis_data

haven::write_dta(analysis_data, "dta/09_analysis_data_ur13.dta")
