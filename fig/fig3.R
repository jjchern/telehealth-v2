
library(tidyverse)

haven::read_dta("dta/app-tab-02-es-wo-nt-est.dta") %>% 
  # filter(out == "dr_all_causes") %>% 
  filter(grepl("es_year", parm)) %>% 
  select(out, parm, estimate, min95, max95) %>% 
  mutate(out = case_when(
    out == "dr_all_causes" ~ "All Causes",
    out == "dr_cerebrovascular_diseases" ~ "CVD",
    out == "dr_diabetes_mellitus" ~ "DM",
    out == "dr_influenza_and_pneumonia" ~ "I&P",
    out == "dr_ischemic_heart_disease" ~ "IHD",
    out == "dr_suicide" ~ "Suicide"
  )) %>% 
  {full_join(., expand(., out, parm = "s3_es_year_f0", estimate = 0))} %>% 
  arrange(out, parm) %>% 
  separate(parm, c("s", "es", "year", "lf")) %>% 
  mutate(es_year = parse_number(lf)) %>% 
  mutate(lf = substr(lf, 1, 1)) %>% 
  mutate(es_year = if_else(grepl("l", lf), -es_year, es_year)) %>%
  print(n = 20) %>% 
  filter(!is.na(es_year)) %>% 
  filter(!is.na(out)) %>% 
  ggplot(aes(x = es_year, y = estimate)) +
  geom_line(colour = "red") +
  geom_point(colour = "red") +
  geom_errorbar(aes(ymin = min95, ymax = max95, width = 0.5), colour = "blue") +
  geom_vline(aes(xintercept = 0), colour = "black") +
  geom_hline(aes(yintercept = 0), colour = "black") +
  facet_wrap(~out) +
  hrbrthemes::theme_ipsum(strip_text_size = 11) -> p

p +
  labs(y = "Event Study Estimates", x = "Years Since Parity Law",
       title = "Effects of Parity Laws on Log Mortality Rates")

p +
  labs(y = "Event Study Estimates", x = "Years Since Parity Law") +
  theme(plot.margin = unit(c(0.1, 1.0, 0.1, 1.0), units = "cm"))

ggsave("fig/fig3.png", dpi = 1000, width = 5.306, height = 6.047)
