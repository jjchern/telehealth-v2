
# remotes::install_github("jjchern/usmapdata")

library(tidyverse)

# Plot maps of states with parity law (2005 and 2016) ---------------------

haven::read_dta("dta/01_policy_date.dta") %>% 
  filter(type == "any_parity_law") %>% 
  mutate(law = case_when(year <= 2005 ~ 1, 
                         year > 2005 & year <= 2016 ~ 2, 
                         TRUE ~ 0)) %>% 
  select(id = fips, law) %>% 
  mutate(law = factor(law, labels = 
                        c("No parity law during\n1999-2016",
                          "Any parity law\nin 2005",
                          "Enacted any parity law\nduring 2005-2016"))) %>% 
  left_join(usmapdata::state, by = "id") %>% {
    ggplot() +
      geom_map(data = ., map = .,
               aes(x = long, y = lat, map_id = id, fill = law),
               colour = alpha("white", 0.5), size = 0.5) +
      coord_map("albers", lat0 = 30, lat1 = 40) +
      hrbrthemes::theme_ipsum() +
      ggthemes::theme_map() +
      scale_fill_brewer(palette = 14) +
      theme(legend.position = c(.5, .05),
            legend.text = element_text(size = 12, family = "Arial Narrow"),
            legend.title = element_blank(),
            legend.justification = "center",
            legend.direction = "horizontal")} -> p
p
ggsave("fig/fig2.png", dpi = 1000, width = 5.306, height = 5.453)
