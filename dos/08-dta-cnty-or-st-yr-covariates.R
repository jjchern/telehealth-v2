
# remotes::install_github("jjchern/ahrf@v0.0.1")
# remotes::install_github("jjchern/laus@v0.0.3")
# remotes::install_github("jjchern/saipe@v0.0.2")
# remotes::install_github("jjchern/census.stat@v0.0.2")
# remotes::install_github("jjchern/kffdata@v0.0.1")
# remotes::install_github("jjchern/aca.medicaid.expansion@v0.0.1")

library(tidyverse)

# 1. Total active mds -----------------------------------------------------
# Next: combind outcome + covariates
# Next: remake sum tab

# ahrf::ahrf_county_layout %>% View() # active

ahrf::ahrf_county %>% 
  select(F04437, F00011, F00012, contains("F08857")) %>% 
  select(-`F08857-90`, -`F08857-80`, -`F08857-70`, -`F08857-60`, -F04437) %>% # 00, 05-08, 10-14
  unite(county_code, F00011, F00012, sep = "") %>% 
  gather(year, tot_active_md, -county_code) %>% 
  mutate(year = paste0(20, substring(year, 8)) %>% as.numeric()) %>% 
  mutate_at(c("tot_active_md"), as.integer) %>% 
  arrange(county_code, year) %>% 
  {right_join(., expand(., county_code, year = 1999:2016))} %>% 
  right_join(haven::read_dta("dta/06-cnty-yr-pop.dta")) %>% 
  mutate(tot_active_md_p100k = tot_active_md / pop * 100000) %>% 
  group_by(county_code) %>% 
  arrange(county_code, year) %>% 
  filter(sum(!is.na(tot_active_md_p100k)) >= 2) %>% 
  mutate(tot_active_md_p100k_appx = approx(year, tot_active_md_p100k, year, rule = 2)$y) %>% 
  ungroup() %>% 
  mutate(tot_active_md_p100k_appx = if_else(is.na(pop), NA_real_, tot_active_md_p100k_appx)) %>% 
  select(county_code, year, tot_active_md_p100k, tot_active_md_p100k_appx) %>% 
  print(n = 20) -> tot_active_md_p100k

# 2. HPSA -----------------------------------------------------------------

# ahrf::ahrf_county_layout %>% View("ahrf") # HPSA

ahrf::ahrf_county %>%
  select(F00011, F00012, contains("F09787"), contains("F12492")) %>%
  unite(county_code, F00011, F00012, sep = "") %>% # 10, 15, 16
  gather(var, hpsa, -county_code) %>%
  mutate(var = gsub("F09787-", "hpsa_pcp_", var)) %>%
  mutate(var = gsub("F12492-", "hpsa_psy_", var)) %>%
  labelled::add_value_labels(hpsa = c("part county" = 2, "whole" = 1, "none" = 0)) %>%
  mutate(hpsa = labelled::to_factor(hpsa)) %>%
  spread(var, hpsa) %>%
  select(county_code, hpsa_pcp_10, hpsa_pcp_15, hpsa_psy_10, hpsa_psy_15) %>%
  print() -> hspa

# 3. unem rate ------------------------------------------------------------

laus::county_year %>% 
  select(state_fips, county_fips, year, cty_unem_rate = unemployment_rate) %>% 
  unite(county_code, state_fips, county_fips, sep = "") %>% 
  arrange(county_code, year) %>% 
  mutate(year = year %>% as.numeric()) %>% 
  filter(year %in% 1999:2016) %>% 
  print(n = 20) -> unem

# 4. Median hh income and poverty rate ------------------------------------

saipe::saipe_county %>% 
  select(county_code = GEOID, year = time, median_hh_inc = SAEMHI_PT, pov_rates = SAEPOVRTALL_PT) %>% 
  arrange(county_code, year) %>% 
  mutate(year = year %>% as.numeric()) %>% 
  filter(year %in% 1999:2016) %>% 
  print(n = 20) -> inc_pov

# 5. Density --------------------------------------------------------------

ahrf::ahrf_county %>% 
  select(F00011, F00012, "F13876-10") %>% 
  unite(county_code, F00011, F00012, sep = "") %>% 
  rename(pop_density_2010 = `F13876-10`) %>% 
  mutate(pop_density_2010 = as.numeric(pop_density_2010) / 10) %>% 
  print() -> pop_density

# 6. Other county-covariates from Census ----------------------------------

census.stat::county_stat %>% 
  print(n = 20) -> county_stat

# Combine all city-yr covariates ------------------------------------------

tot_active_md_p100k %>% 
  left_join(hspa) %>% 
  left_join(unem) %>% 
  left_join(inc_pov) %>% 
  left_join(pop_density) %>% 
  left_join(county_stat) %>% # View("county_covariates")
  print() -> county_covariates

haven::write_dta(county_covariates, "dta/08_county_covariates.dta")

# 7. Other state-year level covariates/outcomes from KFF ------------------

kffdata::hosp_admissions %>% 
  select(usps, year, hosp_ad_p1k = total) %>% 
  mutate(year = as.integer(year)) %>% 
  print() -> hosp_ad_p1k
hosp_ad_p1k %>% count(year)

kffdata::hosp_em_visits %>% 
  select(usps, year, hosp_em_vis_p1k = total) %>% 
  mutate(year = as.integer(year)) %>% 
  print() -> hosp_em_vis_p1k
hosp_em_vis_p1k %>% count(year)

kffdata::hosp_ip_days %>% 
  select(usps, year, hosp_ip_days_p1k = total) %>% 
  mutate(year = as.integer(year)) %>% 
  mutate(hosp_ip_days_p1k = as.numeric(hosp_ip_days_p1k)) %>% 
  print() -> hosp_ip_days_p1k
hosp_ip_days_p1k %>% count(year)

kffdata::hosp_op_visits %>% 
  select(usps, year, hosp_op_vis_p1k = total) %>% 
  mutate(year = as.integer(year)) %>% 
  print(n = 20) -> hosp_op_vis_p1k
hosp_op_vis_p1k %>% count(year)

kffdata::hc_eee_pc %>% 
  filter(year %in% 1999:2016) %>% 
  select(usps, year, hc_eee_pc = health_spending_per_capita) %>% 
  mutate(year = as.integer(year)) %>% 
  print(n = 20) -> hc_eee_pc # 1999-2014
hc_eee_pc %>% count(year)

aca.medicaid.expansion::status %>% 
  mutate(year = list(1999:2016)) %>%
  unnest(year) %>%
  mutate(round_exp_year = lubridate::round_date(expansion_date, "year")) %>%
  mutate(round_exp_year = lubridate::year(round_exp_year)) %>%
  mutate(aca_mdcd_exp = if_else(year < round_exp_year |
                                  is.na(round_exp_year), FALSE, TRUE)) %>%
  mutate(aca_mdcd_exp = as.integer(aca_mdcd_exp)) %>% 
  select(usps, year, aca_mdcd_exp) %>% 
  print() -> aca_mdcd_exp
aca_mdcd_exp %>% count(year)

# download.file(url = "https://www.statepolicyindex.com/wp-content/uploads/2019/07/h_health_17.csv",
#               destfile = "raw/h_health_17.csv")

read_csv(
  "raw/h_health_17.csv",
  skip = 1, 
  na = c("", ".", "..", "#VALUE!", "#DIV/0!"), 
  guess_max = 10000
) %>% 
  docxtractr::mcga() %>% 
  select(state, year, hmbindex, hmpindex) %>% 
  filter(!is.na(hmpindex)) %>% 
  filter(year == 2006) %>% 
  left_join(fips::fips) %>% 
  transmute(usps = usps, hmbindex_06 = hmbindex, hmpindex_06 = hmpindex) %>% 
  add_row(usps = "DC", hmbindex_06 = 0, hmpindex_06 = 0) %>% 
  print() -> hm_index

# 8. State pct mdcd and mcar ----------------------------------------------

# NOTE: To load data, you must download both the extract's data and the DDI
# and also set the working directory to the folder with these files (or change the path below).

if (!require("ipumsr")) stop("Reading IPUMS data into R requires the ipumsr package. It can be installed using the following command: install.packages('ipumsr')")

ipumsr::read_ipums_ddi("raw/cps_00001.xml") %>% 
  ipumsr::ipums_conditions()

ipumsr::read_ipums_ddi("raw/cps_00001.xml") %>% 
  ipumsr::read_ipums_micro() %>% 
  docxtractr::mcga() %>% 
  print() -> cps

cps %>% 
  select(year, asecwt, statefip, himcaidly, himcarely) %>% 
  mutate(hi_mcaid_mcare_ly = (himcaidly == 2) | (himcarely == 2)) %>% 
  group_by(statefip, year) %>% 
  summarise(himcaidly = weighted.mean(x = (himcaidly == 2), w = asecwt), 
            himcarely = weighted.mean(x = (himcarely == 2), w = asecwt),
            hi_mcaid_mcare_ly = weighted.mean(hi_mcaid_mcare_ly, asecwt)) %>% 
  ungroup() %>% 
  mutate(himcaid = himcaidly * 100,
         himcare = himcarely * 100,
         hi_mcaid_mcare = hi_mcaid_mcare_ly * 100) %>% 
  mutate(fips = as.character(statefip) %>% stringr::str_pad(width = 2, pad = "0")) %>% 
  right_join(fips::state) %>% 
  mutate(year = year - 1) %>% # Last year
  filter(year %in% 1999:2016) %>% 
  select(usps, year, himcaid, himcare, hi_mcaid_mcare) %>% 
  print() -> state_pct_mdcd_mcar

# 9. Join state-level covariates ------------------------------------------

aca_mdcd_exp %>% 
  full_join(hm_index) %>% 
  full_join(hosp_ad_p1k) %>% 
  full_join(hosp_em_vis_p1k) %>% 
  full_join(hosp_ip_days_p1k) %>% 
  full_join(hosp_op_vis_p1k) %>% 
  full_join(hc_eee_pc) %>% 
  full_join(state_pct_mdcd_mcar) %>% 
  print(n = 20) -> state_covariates

haven::write_dta(state_covariates, "dta/08_state_covariates.dta")
