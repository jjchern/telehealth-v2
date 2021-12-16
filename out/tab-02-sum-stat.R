
library(tidyverse)

# States ------------------------------------------------------------------

haven::read_dta("dta/01_policy_date.dta") %>% 
  filter(type == "private_parity_law") %>% 
  filter(year >= 1999 + 4 & year <= 2016 - 2) %>% 
  select(usps, yr = year) %>% 
  print() -> selected_treated_st

selected_treated_st %>% 
  expand(usps, year = 1999:2016) %>% 
  left_join(selected_treated_st) %>% 
  filter(year < yr) %>% 
  print() -> selected_treated_st_pre

selected_treated_st %>% 
  expand(usps, year = 1999:2016) %>% 
  left_join(selected_treated_st) %>% 
  filter(year >= yr) %>% 
  print() -> selected_treated_st_post

haven::read_dta("dta/01_policy_date.dta") %>% 
  filter(type == "private_parity_law") %>% 
  filter(year %>% is.na | year %in% 2017:2018) %>% 
  select(usps) %>% 
  print() -> never_treated_st

# Col 1: Never treated states ---------------------------------------------

haven::read_dta("dta/09_analysis_data_county.dta") %>% 
  right_join(never_treated_st) %>% 
  select(-dr_disease_of_heart, -dr_skin_cancer,
         -dr_accidents, -dr_assualt,
         -dr_non_trans_acc, -dr_transport_acc) %>% 
  select(county_code, year, pop, starts_with("dr"),
         cty_unem_rate, median_hh_inc, pov_rates,
         median_age_appx, pct_black_appx, pct_white_appx,
         pct_female_appx, pct_ba_degree_appx, pct_hs_or_less_appx,
         tot_active_md_p100k_appx, pop_density_2010,
         himcaid, himcare) %>% 
  gather(type, death_rate, -county_code, -year, -pop) %>% 
  # Drop county-year with missing pop, so that weighted.mean() can be used
  filter(!is.na(pop)) %>% 
  group_by(type) %>% 
  summarise(value2 = weighted.mean(death_rate, w = pop, na.rm = TRUE)) %>% 
  mutate(value2 = formatC(value2, digits = 1, format = "f", big.mark = ",")) %>% 
  ungroup() %>% 
  mutate(type = case_when(
    type == "dr_all_causes"               ~ "01 - All Causes",
    type == "dr_cerebrovascular_diseases" ~ "02 - Cerebrovascular Diseases",
    type == "dr_diabetes_mellitus"        ~ "03 - Diabetes Mellitus",
    type == "dr_influenza_and_pneumonia"  ~ "04 - Influenza and Pneumonia",
    type == "dr_ischemic_heart_disease"   ~ "05 - Ischemic Heart Disease",
    type == "dr_suicide"                  ~ "06 - Suicide",
    type == "cty_unem_rate"               ~ "07 - Unemployment Rate",
    type == "median_hh_inc"               ~ "08 - Median Household Income",
    type == "pov_rates"                   ~ "09 - Poverty Rate",
    type == "median_age_appx"             ~ "10 - Median Age",
    type == "pct_black_appx"              ~ "11 - Percent Black",
    type == "pct_white_appx"              ~ "12 - Percent White",
    type == "pct_female_appx"             ~ "13 - Percent Female",
    type == "pct_ba_degree_appx"          ~ "14 - Percent BA Degree",
    type == "pct_hs_or_less_appx"         ~ "15 - Percent HS or Below",
    type == "tot_active_md_p100k_appx"    ~ "16 - Total Active MDs (per 100,000)",
    type == "pop_density_2010"            ~ "17 - Population Density (per sq. mile) in 2010",
    type == "himcaid"                     ~ "18 - Percent Medicaid",
    type == "himcare"                     ~ "19 - Percent Medicare"
  )) %>% 
  arrange(type) %>% 
  separate(type, c("row_order", "type"), sep = " - ") %>% 
  print(n = 23) -> top

haven::read_dta("dta/09_analysis_data_county.dta") %>% 
  right_join(never_treated_st) %>%
  filter(year == 2015) %>% 
  select(usps, county_code, hpsa_pcp_10) %>% 
  haven::as_factor() %>% 
  mutate(short = hpsa_pcp_10 %in% c("whole", "part county")) %>% 
  summarise("Number of Counties Short on PC Providers in 2010" = sum(as.integer(short), na.rm = TRUE),
            "Total Number of Counties" = n_distinct(county_code),
            "Total Number of States" = n_distinct(usps)) %>% 
  mutate("Total Number of Observations" = `Total Number of Counties` * 18) %>% 
  gather(type, value2) %>% 
  mutate(value2 = formatC(value2, digits = 0, format = "f", big.mark = ",")) %>% 
  print() -> bottom

top %>% 
  bind_rows(bottom) %>% 
  print(n = 25) -> col1

# Col 2 and 3 -------------------------------------------------------------

haven::read_dta("dta/09_analysis_data_county.dta") %>% 
  right_join(selected_treated_st_pre) %>% 
  select(-dr_disease_of_heart, -dr_skin_cancer,
         -dr_accidents, -dr_assualt,
         -dr_non_trans_acc, -dr_transport_acc) %>% 
  select(county_code, year, pop, starts_with("dr"),
         cty_unem_rate, median_hh_inc, pov_rates,
         median_age_appx, pct_black_appx, pct_white_appx,
         pct_female_appx, pct_ba_degree_appx, pct_hs_or_less_appx,
         tot_active_md_p100k_appx,
         himcaid, himcare) %>% 
  gather(type, death_rate, -county_code, -year, -pop) %>% 
  # Drop county-year with missing pop, so that weighted.mean() can be used
  filter(!is.na(pop)) %>% 
  group_by(type) %>% 
  summarise(pre = weighted.mean(death_rate, w = pop, na.rm = TRUE)) %>% 
  mutate(pre = formatC(pre, digits = 1, format = "f", big.mark = ",")) %>% 
  ungroup() %>% 
  mutate(type = case_when(
    type == "dr_all_causes"               ~ "01 - All Causes",
    type == "dr_cerebrovascular_diseases" ~ "02 - Cerebrovascular Diseases",
    type == "dr_diabetes_mellitus"        ~ "03 - Diabetes Mellitus",
    type == "dr_influenza_and_pneumonia"  ~ "04 - Influenza and Pneumonia",
    type == "dr_ischemic_heart_disease"   ~ "05 - Ischemic Heart Disease",
    type == "dr_suicide"                  ~ "06 - Suicide",
    type == "cty_unem_rate"               ~ "07 - Unemployment Rate",
    type == "median_hh_inc"               ~ "08 - Median Household Income",
    type == "pov_rates"                   ~ "09 - Poverty Rate",
    type == "median_age_appx"             ~ "10 - Median Age",
    type == "pct_black_appx"              ~ "11 - Percent Black",
    type == "pct_white_appx"              ~ "12 - Percent White",
    type == "pct_female_appx"             ~ "13 - Percent Female",
    type == "pct_ba_degree_appx"          ~ "14 - Percent BA Degree",
    type == "pct_hs_or_less_appx"         ~ "15 - Percent HS or Below",
    type == "tot_active_md_p100k_appx"    ~ "16 - Total Active MDs (per 100,000)",
    type == "himcaid"                     ~ "18 - Percent Medicaid",
    type == "himcare"                     ~ "19 - Percent Medicare")) %>% 
  arrange(type) %>% 
  separate(type, c("row_order", "type"), sep = " - ") %>%   
  filter(!is.na(type)) %>% 
  print() -> col2

haven::read_dta("dta/09_analysis_data_county.dta") %>% 
  right_join(selected_treated_st_post) %>% 
  select(-dr_disease_of_heart, -dr_skin_cancer,
         -dr_accidents, -dr_assualt,
         -dr_non_trans_acc, -dr_transport_acc) %>% 
  select(county_code, year, pop, starts_with("dr"),
         cty_unem_rate, median_hh_inc, pov_rates,
         median_age_appx, pct_black_appx, pct_white_appx,
         pct_female_appx, pct_ba_degree_appx, pct_hs_or_less_appx,
         tot_active_md_p100k_appx,
         himcaid, himcare) %>% 
  gather(type, death_rate, -county_code, -year, -pop) %>% 
  # Drop county-year with missing pop, so that weighted.mean() can be used
  filter(!is.na(pop)) %>% 
  group_by(type) %>% 
  summarise(post = weighted.mean(death_rate, w = pop, na.rm = TRUE)) %>% 
  mutate(post = formatC(post, digits = 1, format = "f", big.mark = ",")) %>% 
  ungroup() %>% 
  mutate(type = case_when(
    type == "dr_all_causes"               ~ "01 - All Causes",
    type == "dr_cerebrovascular_diseases" ~ "02 - Cerebrovascular Diseases",
    type == "dr_diabetes_mellitus"        ~ "03 - Diabetes Mellitus",
    type == "dr_influenza_and_pneumonia"  ~ "04 - Influenza and Pneumonia",
    type == "dr_ischemic_heart_disease"   ~ "05 - Ischemic Heart Disease",
    type == "dr_suicide"                  ~ "06 - Suicide",
    type == "cty_unem_rate"               ~ "07 - Unemployment Rate",
    type == "median_hh_inc"               ~ "08 - Median Household Income",
    type == "pov_rates"                   ~ "09 - Poverty Rate",
    type == "median_age_appx"             ~ "10 - Median Age",
    type == "pct_black_appx"              ~ "11 - Percent Black",
    type == "pct_white_appx"              ~ "12 - Percent White",
    type == "pct_female_appx"             ~ "13 - Percent Female",
    type == "pct_ba_degree_appx"          ~ "14 - Percent BA Degree",
    type == "pct_hs_or_less_appx"         ~ "15 - Percent HS or Below",
    type == "tot_active_md_p100k_appx"    ~ "16 - Total Active MDs (per 100,000)",
    type == "himcaid"                     ~ "18 - Percent Medicaid",
    type == "himcare"                     ~ "19 - Percent Medicare")) %>% 
  arrange(type) %>% 
  separate(type, c("row_order", "type"), sep = " - ") %>%   
  filter(!is.na(type)) %>% 
  print() -> col3

# Col 4: Selected treated states, all years 1999-2016 ---------------------

haven::read_dta("dta/09_analysis_data_county.dta") %>% 
  right_join(selected_treated_st %>% select(usps)) %>% 
  select(-dr_disease_of_heart, -dr_skin_cancer,
         -dr_accidents, -dr_assualt,
         -dr_non_trans_acc, -dr_transport_acc) %>% 
  select(county_code, year, pop, starts_with("dr"),
         cty_unem_rate, median_hh_inc, pov_rates,
         median_age_appx, pct_black_appx, pct_white_appx,
         pct_female_appx, pct_ba_degree_appx, pct_hs_or_less_appx,
         tot_active_md_p100k_appx, pop_density_2010,
         himcaid, himcare) %>% 
  gather(type, death_rate, -county_code, -year, -pop) %>% 
  # Drop county-year with missing pop, so that weighted.mean() can be used
  filter(!is.na(pop)) %>% 
  group_by(type) %>% 
  summarise(value = weighted.mean(death_rate, w = pop, na.rm = TRUE)) %>% 
  ungroup() %>%  
  mutate(value = formatC(value, digits = 1, format = "f", big.mark = ",")) %>% 
  ungroup() %>% 
  mutate(type = case_when(
    type == "dr_all_causes"               ~ "01 - All Causes",
    type == "dr_cerebrovascular_diseases" ~ "02 - Cerebrovascular Diseases",
    type == "dr_diabetes_mellitus"        ~ "03 - Diabetes Mellitus",
    type == "dr_influenza_and_pneumonia"  ~ "04 - Influenza and Pneumonia",
    type == "dr_ischemic_heart_disease"   ~ "05 - Ischemic Heart Disease",
    type == "dr_suicide"                  ~ "06 - Suicide",
    type == "cty_unem_rate"               ~ "07 - Unemployment Rate",
    type == "median_hh_inc"               ~ "08 - Median Household Income",
    type == "pov_rates"                   ~ "09 - Poverty Rate",
    type == "median_age_appx"             ~ "10 - Median Age",
    type == "pct_black_appx"              ~ "11 - Percent Black",
    type == "pct_white_appx"              ~ "12 - Percent White",
    type == "pct_female_appx"             ~ "13 - Percent Female",
    type == "pct_ba_degree_appx"          ~ "14 - Percent BA Degree",
    type == "pct_hs_or_less_appx"         ~ "15 - Percent HS or Below",
    type == "tot_active_md_p100k_appx"    ~ "16 - Total Active MDs (per 100,000)",
    type == "pop_density_2010"            ~ "17 - Population Density (per sq. mile) in 2010",
    type == "himcaid"                     ~ "18 - Percent Medicaid",
    type == "himcare"                     ~ "19 - Percent Medicare"
  )) %>% 
  arrange(type) %>% 
  separate(type, c("row_order", "type"), sep = " - ") %>% 
  print(n = 19) -> top

haven::read_dta("dta/09_analysis_data_county.dta") %>% 
  right_join(selected_treated_st %>% select(usps)) %>%
  filter(year == 2015) %>% 
  select(usps, county_code, hpsa_pcp_10) %>% 
  haven::as_factor() %>% 
  mutate(short = hpsa_pcp_10 %in% c("whole", "part county")) %>% 
  summarise("Number of Counties Short on PC Providers in 2010" = sum(as.integer(short), na.rm = TRUE),
            "Total Number of Counties" = n_distinct(county_code),
            "Total Number of States" = n_distinct(usps)) %>% 
  mutate("Total Number of Observations" = `Total Number of Counties` * 18) %>% 
  gather(type, value) %>% 
  mutate(value = formatC(value, digits = 0, format = "f", big.mark = ",")) %>% 
  print() -> bottom

top %>% 
  bind_rows(bottom) %>% 
  print() -> col4

# Combine col1, col2, and col3 --------------------------------------------

col1 %>% 
  left_join(col2) %>% 
  left_join(col3) %>% 
  left_join(col4) %>% 
  select(-row_order) %>% 
  mutate(pre = if_else(is.na(pre), "", pre)) %>% 
  mutate(post = if_else(is.na(post), "", post)) %>% 
  add_row(type = "Mortality Rate (per 100,000): ",
          value = " ", pre = " ", post = " ", value2 = " ",
          .before = 1) %>%   
  add_row(type = "County Statistics: ",
          value = " ", pre = " ", post = " ", value2 = " ",
          .before = 8) %>% 
  print(n = 25) -> df

haven::write_dta(df, "out/tab-02-sum-stat.dta")

haven::read_dta("out/tab-02-sum-stat.dta") %>% 
  rename(` ` = type,
         "Selected Treated\nStates (All Years)" = value,
         "Selected Treated\nStates (Pre)" = pre,
         "Selected Treated\nStates (Post)" = post,
         "Never Treated\nStates (All Years)" = value2) %>%   
  as.data.frame() %>% 
  pander::pandoc.table(caption = "Table 2: Descriptive Statistics (Weighted Means)",
                       keep.line.breaks = TRUE,
                       justify = "lcccc")
