
library(tidyverse)
library(tidylog)

# How many treated states during 1999-2016? -------------------------------

haven::read_dta("dta/01_policy_date.dta") %>% 
  filter(type == "private_parity_law") %>% 
  filter(!is.na(year)) %>% # 35 
  filter(year %in% 1999:2016) %>% # 26
  filter(year > 2010) # 20

# Plot number of states with parity law by year ---------------------------

haven::read_dta("dta/01_policy_date.dta") %>% 
  select(state, type, year) %>% 
  group_by(type, year) %>% 
  count() %>% 
  arrange(type, year) %>% 
  group_by(type) %>% 
  mutate(number = cumsum(n)) %>% 
  na.omit(year) %>% 
  ungroup() %>% 
  filter(type != "any_parity_law") %>% 
  mutate(type = if_else(type == "private_parity_law", "1", "2")) %>% 
  mutate(type = factor(type, labels = c("Private Parity Law", 
                                        "Medicaid Parity Law"))) %>% 
  ggplot() +
  geom_step(aes(x = year, y = number, colour = type, linetype = type)) + 
  annotate("rect", xmin = 1999, xmax = 2016, ymin = 0, ymax = Inf,
           alpha = .1) +
  labs(y = "Number of States with Telemedicine Parity Law",
       x = NULL) +
  hrbrthemes::theme_ipsum(grid = "XY",
                             axis_title_size = 11,
                             base_size = 12) +
  theme(legend.position = "bottom",
        legend.title = element_blank()) +
  scale_y_continuous(breaks = seq(0, 35, 5)) -> p
p

# Save a version for pdf --------------------------------------------------

p
ggsave("fig/fig1.png", dpi = 1000, width = 5.306, height = 5.453)

# Extra -------------------------------------------------------------------

haven::read_dta("dta/12_reg_data_state.dta") %>% 
  filter(s3 == 1) %>% 
  select(year, tot_pop,
         ln_dr_all_causes,
         ln_dr_cerebrovascular_diseases,
         ln_dr_diabetes_mellitus,
         ln_dr_influenza_and_pneumonia,
         ln_dr_ischemic_heart_disease,
         ln_dr_suicide) %>% 
  gather(var, val, -year, -tot_pop) %>% 
  group_by(year, var) %>% 
  summarise(val = weighted.mean(val, tot_pop)) %>% 
  ggplot(aes(x = year, y = val, colour = var)) +
  geom_line()

haven::read_dta("dta/01_policy_date.dta") %>% 
  select(state, type, year) %>% 
  group_by(type, year) %>% 
  count() %>% 
  arrange(type, year) %>% 
  group_by(type) %>% 
  mutate(number = cumsum(n)) %>% 
  na.omit(year) %>% 
  ungroup() %>% 
  filter(type != "any_parity_law") %>% 
  mutate(type = if_else(type == "private_parity_law", "1", "2")) %>% 
  mutate(type = factor(type, labels = c("Private Parity Law", 
                                        "Medicaid Parity Law"))) %>% 
  ggplot() +
  geom_step(aes(x = year, y = number, colour = type, linetype = type)) + 
  annotate("rect", xmin = 1999, xmax = 2016, ymin = 0, ymax = Inf,
           alpha = .1) +
  labs(y = "Number of States with Telemedicine Parity Law",
       x = NULL) +
  hrbrthemes::theme_ipsum(grid = "XY",
                          axis_title_size = 11,
                          base_size = 12) +
  theme(legend.position = "bottom",
        legend.title = element_blank()) +
  geom_line(data = haven::read_dta("dta/12_reg_data_state.dta") %>% 
              filter(s3 == 1) %>% 
              select(year, tot_pop,
                     ln_dr_all_causes,
                     ln_dr_cerebrovascular_diseases,
                     ln_dr_diabetes_mellitus,
                     ln_dr_influenza_and_pneumonia,
                     ln_dr_ischemic_heart_disease,
                     ln_dr_suicide) %>% 
              gather(var, val, -year, -tot_pop) %>% 
              group_by(year, var) %>% 
              summarise(val = weighted.mean(val, tot_pop)) %>% 
              mutate(val = val * 4.5),
            aes(x = year, y = val, colour = var)) +
  scale_y_continuous(sec.axis = sec_axis(~ . / 4.5, 
                                         name = "Log(Death Rate per 100,000)"))

                     