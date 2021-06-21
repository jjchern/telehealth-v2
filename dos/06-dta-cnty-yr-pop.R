
library(tidyverse)

# Prepare county-level population for all selected counties ---------------

haven::read_dta("dta/05-deaths-pop-county.dta") %>% 
  print(n = 13) %>% # pop is the same regardless of causes of death
  distinct(county_code, year, pop) %>% # group_by(county_code) # 3147 # 
  # filter(is.na(pop)) %>% group_by(county_code) %>% print(n = 111) %>% distinct(county_code) # 111 (12)
  print() -> cnty_yr_pop

haven::write_dta(cnty_yr_pop, "dta/06-cnty-yr-pop.dta")

# Counties with missing years ---------------------------------------------

# Denali Borough, AK, 02068: No data in 2002 and before
haven::read_dta("dta/05-deaths-pop-county.dta") %>% filter(county_code == "02068") %>% print(n = 144)

# Hoonah-Angoon Census Area, AK, 02105: No data in 2013 and before
haven::read_dta("dta/05-deaths-pop-county.dta") %>% filter(county_code == "02105") %>% print(n = 144)

# Petersburg Borough/Census Area, AK, 02195: No data in 2013 and before
haven::read_dta("dta/05-deaths-pop-county.dta") %>% filter(county_code == "02195") %>% print(n = 144)

# Prince of Wales-Hyder Census Area, AK, 02198: No data in 2013 and before
haven::read_dta("dta/05-deaths-pop-county.dta") %>% filter(county_code == "02198") %>% print(n = 144)

# Prince of Wales-Outer Ketchikan Census Area, AK, 02201: No data in 2014 and after
haven::read_dta("dta/05-deaths-pop-county.dta") %>% filter(county_code == "02201") %>% print(n = 144)

# Skagway Municipality, AK, 02230: No data in 2013 and before
haven::read_dta("dta/05-deaths-pop-county.dta") %>% filter(county_code == "02230") %>% print(n = 144)

# Skagway-Hoonah-Angoon Census Area, AK, 02232: No data in 2014 and after
haven::read_dta("dta/05-deaths-pop-county.dta") %>% filter(county_code == "02232") %>% print(n = 144)

# Wrangell City and Borough, AK, 02275: No data in 2013 and before
haven::read_dta("dta/05-deaths-pop-county.dta") %>% filter(county_code == "02275") %>% print(n = 144)

# Wrangell-Petersburg Census Area, AK, 02280: No data in 2014 and after
haven::read_dta("dta/05-deaths-pop-county.dta") %>% filter(county_code == "02280") %>% print(n = 144)

# Broomfield County, CO, 08014: No data in 2002 and before
haven::read_dta("dta/05-deaths-pop-county.dta") %>% filter(county_code == "08014") %>% print(n = 144)

# Bedford city, VA, 51515: No data in 2014 and after
haven::read_dta("dta/05-deaths-pop-county.dta") %>% filter(county_code == "51515") %>% print(n = 144)

# Clifton Forge city, VA, 51560: No data in 2001 and after
haven::read_dta("dta/05-deaths-pop-county.dta") %>% filter(county_code == "51560") %>% print(n = 144)
