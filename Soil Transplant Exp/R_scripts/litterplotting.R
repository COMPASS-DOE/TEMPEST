
files <- list.files("../litter_data/", pattern = '*.csv', full.names = TRUE)

library(readr)
x <- lapply(files, read_csv)

library(dplyr)
ld <- bind_rows(x)

library(tidyr)
ld %>% 
  gather(Category, Mass_g, starts_with("M_")) %>% 
  # compute per square meter
  mutate(Mass_g = Mass_g * 2) ->
  ld_long

library(lubridate)

ld_long %>% 
  mutate(Site = substr(Plot, 1, 2)) %>% 
  filter(Site == "HS") %>% 
  mutate(Date_collected = mdy(Date_collected)) %>% 
  # average cross litter traps and plots
  group_by(Date_collected, Category) %>% 
  summarise(Mass_g = mean(Mass_g)) %>% 
  ungroup() ->
  ld_hs

ld_hs %>%
  arrange(Category, Date_collected) %>% 
  group_by(Category) %>% 
  mutate(Mass_g_cumulative = cumsum(Mass_g)) %>% 
  # compute our HS avg and sd
  # group_by(Category, Date_collected) %>% 
  # summarise(Mass_g_cumulative_sd = sd(Mass_g_cumulative),
  #           Mass_g_cumulative = mean(Mass_g_cumulative)) %>% 
  ggplot(aes(Date_collected, Mass_g_cumulative, fill = Category)) + geom_area(position = "stack") +
  labs(x = "Date Collected", y = "Cumulative Mass (g)")

  


