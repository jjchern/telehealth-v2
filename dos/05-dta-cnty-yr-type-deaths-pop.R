
library(tidyverse)

# Load county-level outcome variables -------------------------------------

map2_dfr(
  .x = read_csv("dta/03-icd10-113-cause-list.csv")$V1,
  .y = read_csv("dta/03-icd10-113-cause-list.csv")$V3,
  .f = ~ list.files("raw/county_part1") %>% 
    grep(.x, ., value = TRUE) %>% 
    `[`(1) %>% 
    paste0("raw/county_part1/", .) %>% 
    read_tsv() %>% 
    filter(!is.na(County)) %>% 
    mutate(Type = .y) %>% 
    mutate(Deaths = as.numeric(Deaths)) %>% 
    mutate(Population = as.numeric(Population)) %>% 
    mutate(`Crude Rate` = as.character(`Crude Rate`)) %>% 
    mutate(`Age Adjusted Rate` = as.character(`Age Adjusted Rate`))
)  %>% 
  print() -> raw_part1

map2_dfr(
  .x = read_csv("dta/03-icd10-113-cause-list.csv")$V1,
  .y = read_csv("dta/03-icd10-113-cause-list.csv")$V3,
  .f = ~ list.files("raw/county_part2") %>% 
    grep(.x, ., value = TRUE) %>% 
    `[`(1) %>% 
    paste0("raw/county_part2/", .) %>% 
    read_tsv() %>% 
    filter(!is.na(County)) %>% 
    mutate(Type = .y) %>% 
    mutate(Deaths = as.numeric(Deaths)) %>% 
    mutate(Population = as.numeric(Population)) %>% 
    mutate(`Crude Rate` = as.character(`Crude Rate`)) %>% 
    mutate(`Age Adjusted Rate` = as.character(`Age Adjusted Rate`))
)  %>% 
  print() -> raw_part2

raw_part1 %>% 
  bind_rows(raw_part2) %>% 
  select(county = County,
         county_code = `County Code`, 
         year = Year,
         type = Type,
         deaths = Deaths,
         pop = Population) %>% 
  separate(county, c("county", "usps"), ", ") %>% 
  arrange(county_code, year, type) %>% 
  print(n = 13) -> deaths_pop

# Add deaths_r with random numbers for missing values ---------------------

set.seed(222)

deaths_pop %>% 
  filter(is.na(deaths)) %>% 
  nrow() %>% print() -> n_missing

as.numeric(sample(0L:9L, n_missing, replace = TRUE)) %>% 
  print() -> missing_vec

deaths_pop %>% 
  filter(is.na(deaths)) %>% 
  print() -> missing_df

bind_cols(missing_df, tibble(deaths_r = missing_vec)) %>% 
  print() -> missing_df2

full_join(deaths_pop, missing_df2) %>% 
  mutate(deaths_r = if_else(is.na(deaths), deaths_r, deaths)) %>% 
  print() -> deaths_pop2

haven::write_dta(deaths_pop2, "dta/05-deaths-pop-county.dta")

# Add under-65 deaths -----------------------------------------------------

# Walk before run
# read_tsv("raw/county_part1/Under-65 GR113-000. Mortality rates of all causes by county, 1999-2008.txt") %>%
#   filter(!is.na(County)) %>%
#   mutate(Type = "all_causes") %>%
#   mutate(Deaths = as.numeric(Deaths)) %>%
#   mutate(Population = as.numeric(Population)) %>%
#   mutate(`Crude Rate` = as.character(`Crude Rate`)) %>%
#   print() -> under_65_raw_part1
# 
# read_tsv("raw/county_part2/Under-65 GR113-000. Mortality rates of all causes by county, 2009-2016.txt") %>%
#   filter(!is.na(County)) %>%
#   mutate(Type = "all_causes") %>%
#   mutate(Deaths = as.numeric(Deaths)) %>%
#   mutate(Population = as.numeric(Population)) %>%
#   mutate(`Crude Rate` = as.character(`Crude Rate`)) %>%
#   print() -> under_65_raw_part2

#  "GR113-000" "GR113-046"         "GR113-058"              "GR113-070"                "GR113-076"               "GR113-124" 
# "all_causes" "diabetes_mellitus" "ischemic_heart_disease" "cerebrovascular_diseases" "influenza_and_pneumonia"  "suicide" 

map2_dfr(
  .x = c("Under-65 GR113-000", "Under-65 GR113-046", "Under-65 GR113-058", "Under-65 GR113-070", "Under-65 GR113-076", "Under-65 GR113-124", "Under-65 Mortality"),
  .y = c("all_causes", "diabetes_mellitus", "ischemic_heart_disease", "cerebrovascular_diseases", "influenza_and_pneumonia", "suicide", "combined"),
  .f = ~ list.files("raw/county_part1") %>% 
    grep(.x, ., value = TRUE) %>% 
    paste0("raw/county_part1/", .) %>% 
    read_tsv() %>% 
    filter(!is.na(County)) %>% 
    mutate(Type = .y) %>% 
    mutate(Deaths = as.numeric(Deaths)) %>% 
    mutate(Population = as.numeric(Population)) %>% 
    mutate(`Crude Rate` = as.character(`Crude Rate`))
)  %>% 
  print() -> under_65_raw_part1

under_65_raw_part1 %>% count(Type)

map2_dfr(
  .x = c("Under-65 GR113-000", "Under-65 GR113-046", "Under-65 GR113-058", "Under-65 GR113-070", "Under-65 GR113-076", "Under-65 GR113-124", "Under-65 Mortality"),
  .y = c("all_causes", "diabetes_mellitus", "ischemic_heart_disease", "cerebrovascular_diseases", "influenza_and_pneumonia", "suicide", "combined"),
  .f = ~ list.files("raw/county_part2") %>% 
    grep(.x, ., value = TRUE) %>% 
    paste0("raw/county_part2/", .) %>% 
    read_tsv() %>% 
    filter(!is.na(County)) %>% 
    mutate(Type = .y) %>% 
    mutate(Deaths = as.numeric(Deaths)) %>% 
    mutate(Population = as.numeric(Population)) %>% 
    mutate(`Crude Rate` = as.character(`Crude Rate`))
)  %>% 
  print() -> under_65_raw_part2

under_65_raw_part2 %>% count(Type)

under_65_raw_part1 %>% 
  bind_rows(under_65_raw_part2) %>% 
  select(county = County,
         county_code = `County Code`, 
         year = Year,
         type = Type,
         deaths = Deaths,
         pop = Population) %>% 
  separate(county, c("county", "usps"), ", ") %>% 
  arrange(county_code, year, type) %>% 
  print(n = 13) -> under_65_deaths_pop

haven::write_dta(under_65_deaths_pop, "dta/05-deaths-pop-county-under-65.dta")

# Add 35-64 deaths -----------------------------------------------------

# Walk before run
# read_tsv("raw/county_part1/Age3564 GR113-000. Mortality rates of all causes by county, 1999-2008.txt") %>%
#   filter(!is.na(County)) %>%
#   mutate(Type = "all_causes") %>%
#   mutate(Deaths = as.numeric(Deaths)) %>%
#   mutate(Population = as.numeric(Population)) %>%
#   mutate(`Crude Rate` = as.character(`Crude Rate`)) %>%
#   print() -> age3564_raw_part1
# 
# read_tsv("raw/county_part2/Age3564 GR113-000. Mortality rates of all causes by county, 2009-2016.txt") %>%
#   filter(!is.na(County)) %>%
#   mutate(Type = "all_causes") %>%
#   mutate(Deaths = as.numeric(Deaths)) %>%
#   mutate(Population = as.numeric(Population)) %>%
#   mutate(`Crude Rate` = as.character(`Crude Rate`)) %>%
#   print() -> age3564_raw_part1

#  "GR113-000" "GR113-046"         "GR113-058"              "GR113-070"                "GR113-076"               "GR113-124" 
# "all_causes" "diabetes_mellitus" "ischemic_heart_disease" "cerebrovascular_diseases" "influenza_and_pneumonia"  "suicide" 

map2_dfr(
  .x = c("Age3564 GR113-000", "Age3564 GR113-046", "Age3564 GR113-058", "Age3564 GR113-070", "Age3564 GR113-076", "Age3564 GR113-124", "Age3564 Mortality"),
  .y = c("all_causes", "diabetes_mellitus", "ischemic_heart_disease", "cerebrovascular_diseases", "influenza_and_pneumonia", "suicide", "combined"),
  .f = ~ list.files("raw/county_part1") %>% 
    grep(.x, ., value = TRUE) %>% 
    paste0("raw/county_part1/", .) %>% 
    read_tsv() %>% 
    filter(!is.na(County)) %>% 
    mutate(Type = .y) %>% 
    mutate(Deaths = as.numeric(Deaths)) %>% 
    mutate(Population = as.numeric(Population)) %>% 
    mutate(`Crude Rate` = as.character(`Crude Rate`))
)  %>% 
  print() -> age3564_raw_part1

age3564_raw_part1 %>% count(Type)

map2_dfr(
  .x = c("Age3564 GR113-000", "Age3564 GR113-046", "Age3564 GR113-058", "Age3564 GR113-070", "Age3564 GR113-076", "Age3564 GR113-124", "Age3564 Mortality"),
  .y = c("all_causes", "diabetes_mellitus", "ischemic_heart_disease", "cerebrovascular_diseases", "influenza_and_pneumonia", "suicide", "combined"),
  .f = ~ list.files("raw/county_part2") %>% 
    grep(.x, ., value = TRUE) %>% 
    paste0("raw/county_part2/", .) %>% 
    read_tsv() %>% 
    filter(!is.na(County)) %>% 
    mutate(Type = .y) %>% 
    mutate(Deaths = as.numeric(Deaths)) %>% 
    mutate(Population = as.numeric(Population)) %>% 
    mutate(`Crude Rate` = as.character(`Crude Rate`))
)  %>% 
  print() -> age3564_raw_part2

age3564_raw_part2 %>% count(Type)

age3564_raw_part1 %>% 
  bind_rows(age3564_raw_part2) %>% 
  select(county = County,
         county_code = `County Code`, 
         year = Year,
         type = Type,
         deaths = Deaths,
         pop = Population) %>% 
  separate(county, c("county", "usps"), ", ") %>% 
  arrange(county_code, year, type) %>% 
  print(n = 13) -> age3564_deaths_pop

haven::write_dta(age3564_deaths_pop, "dta/05-deaths-pop-county-age3564.dta")

# Load 2013 Urban-Rural level outcome variables ---------------------------

map2_dfr(
  .x = read_csv("dta/03-icd10-113-cause-list.csv")$V1[c(1:7, 11)],
  .y = read_csv("dta/03-icd10-113-cause-list.csv")$V3[c(1:7, 11)],
  .f = ~ list.files("raw/ur_codes_2013") %>% 
    grep(.x, ., value = TRUE) %>% 
    paste0("raw/ur_codes_2013/", .) %>% 
    read_tsv() %>% 
    filter(is.na(Notes)) %>% 
    mutate(Type = .y) %>% 
    mutate(Deaths = as.numeric(Deaths)) %>% 
    mutate(Population = as.numeric(Population)) %>% 
    mutate(`Crude Rate` = as.character(`Crude Rate`)) %>% 
    mutate(`Age Adjusted Rate` = as.character(`Age Adjusted Rate`))
)  %>% 
  print() -> raw_13ur

raw_13ur %>% 
  select(state = State,
         state_code = `State Code`,
         ur13 = `2013 Urbanization`,
         ur13_code = `2013 Urbanization Code`,
         year = Year,
         type = Type,
         deaths = Deaths,
         pop = Population) %>%
  arrange(state_code, ur13_code, year, type) %>% 
  print() -> deaths_pop

haven::write_dta(deaths_pop, "dta/05-deaths-pop-ur13.dta")

# Load state level outcome variables ---------------------------

map2_dfr(
  .x = read_csv("dta/03-icd10-113-cause-list.csv")$V1,
  .y = read_csv("dta/03-icd10-113-cause-list.csv")$V3,
  .f = ~ list.files("raw/state") %>% 
    grep(.x, ., value = TRUE) %>% 
    paste0("raw/state/", .) %>% 
    read_tsv() %>% 
    filter(is.na(Notes)) %>% 
    mutate(Type = .y) %>% 
    mutate(Deaths = as.numeric(Deaths)) %>% 
    mutate(Population = as.numeric(Population)) %>% 
    mutate(`Crude Rate` = as.character(`Crude Rate`)) %>% 
    mutate(`Age Adjusted Rate` = as.character(`Age Adjusted Rate`))
)  %>% 
  print() -> raw_state

raw_state %>% 
  select(state = State,
         state_code = `State Code`,
         year = Year,
         type = Type,
         deaths = Deaths,
         pop = Population) %>%
  arrange(state_code, year, type) %>% 
  print() -> deaths_pop

haven::write_dta(deaths_pop, "dta/05-deaths-pop-state.dta")

# Under 65, state year mortality rates ------------------------------------

read_tsv("raw/state/Under-65 Mortality rates by selected five causes by state, 1999-2016.txt") %>% 
  filter(is.na(Notes)) %>% 
  rename(cl = `ICD-10 113 Cause List`) %>% 
  bind_rows(
    read_tsv("raw/state/Under-65 Mortality rates for all causes by state, 1999-2016.txt") %>% 
      filter(is.na(Notes)) %>% 
      mutate(cl = "all_causes") %>% 
      mutate(Deaths = as.character(Deaths)) %>% 
      mutate(`Crude Rate` = as.character(`Crude Rate`))
  ) %>% 
  mutate(Type = case_when(
    cl == "#Cerebrovascular diseases (I60-I69)"                   ~ "cerebrovascular_diseases",
    cl == "#Diabetes mellitus (E10-E14)"                          ~ "diabetes_mellitus",
    cl == "#Influenza and pneumonia (J09-J18)"                    ~ "influenza_and_pneumonia",
    cl == "#Intentional self-harm (suicide) (*U03,X60-X84,Y87.0)" ~ "suicide",
    cl == "Ischemic heart diseases (I20-I25)"                     ~ "ischemic_heart_disease",
    TRUE ~ cl
  )) %>% 
  mutate(Deaths = as.numeric(Deaths)) %>% 
  mutate(Population = as.numeric(Population)) %>% 
  mutate(`Crude Rate` = as.character(`Crude Rate`)) %>% 
  select(state = State,
         state_code = `State Code`,
         year = Year,
         type = Type,
         deaths = Deaths,
         pop = Population) %>%
  arrange(state_code, year, type) %>% 
  print() -> deaths_pop_under_65

haven::write_dta(deaths_pop_under_65, "dta/05-deaths-pop-state-under-65.dta")
