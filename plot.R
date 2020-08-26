# Boom!
# Read and plot 7810 data from Anya Hopple's TEMPEST exclusion plots
# BBL May 2020

library(readr)
library(dplyr)
library(ggplot2)
theme_set(theme_bw())
library(lubridate)
library(tidyr)
library(scales)

files <- list.files("7810_data/", pattern = "*.txt", full.names = TRUE)

dat <- bind_rows(lapply(files, read_tsv, 
                        col_names = c("Timestamp", "Obs", "Port",
                                      "CO2_Flux", "Collar", "CH4_Flux"), 
                        col_types = "_Tdcddd_",
                        skip = 1))

collars <- read_csv("design/collar_map.csv", col_types = "dcdci")

if(any(is.na(dat$Collar))) {
  warning("We have empty collars!")
}

dat %>%
  filter(CO2_Flux != 0.0, CH4_Flux != 0.0, !is.na(Collar)) %>%
  mutate(Year = year(Timestamp), DOY = yday(Timestamp)) %>% 
  left_join(collars, by = "Collar") ->
  dat_plot

dat_plot %>%
  bind_rows(mutate(dat_plot, Plot = "Combined")) ->
  dat_plot_all

dat_plot_all %>% 
  group_by(Plot, Treatment, yday(Timestamp)) %>% 
  summarise(Timestamp = mean(Timestamp),
            CO2_Flux_sd = sd(CO2_Flux), 
            CO2_Flux = mean(CO2_Flux),
            CH4_Flux_sd = sd(CH4_Flux),
            CH4_Flux = mean(CH4_Flux)) ->
  dat_plot_all_means

dat_plot_all_means %>% 
  ggplot(aes(Timestamp, CO2_Flux, color = Treatment, size = Treatment == "Control")) +
  geom_errorbar(aes(ymin = CO2_Flux - CO2_Flux_sd, ymax = CO2_Flux + CO2_Flux_sd)) +
  geom_line() +
  scale_size_manual(guide = FALSE, values = c(0.5, 2)) +
  coord_cartesian(ylim = c(0, 40)) +
  facet_wrap(~Plot) ->
  p

print(p)
ggsave("over_time_co2.png", width = 9, height = 6)

dat_plot_all_means %>% 
  ggplot(aes(Timestamp, CH4_Flux, color = Treatment, size = Treatment == "Control")) +
  geom_errorbar(aes(ymin = CH4_Flux - CH4_Flux_sd, ymax = CH4_Flux + CH4_Flux_sd)) +
  geom_line() +
  scale_size_manual(guide = FALSE, values = c(0.5, 2)) +
  coord_cartesian(ylim = c(-6, 2)) +
  facet_wrap(~Plot) ->
  p

print(p)
ggsave("over_time_ch4.png", width = 9, height = 6)


dat_plot %>% 
  gather(Gas, Flux, CO2_Flux, CH4_Flux) %>% 
  ggplot(aes(Timestamp, Flux, group = Collar, color = Plot)) +
  geom_line() + 
  geom_point() +
  facet_wrap(~ Treatment + Gas, scales = "free_y", ncol = 2 ) ->
  p

print(p)
ggsave("over_time_collars.png", width = 9, height = 6)

dat_plot %>% 
  filter(Treatment == "Control") %>% 
  group_by(Year, DOY, Plot) %>% 
  summarise(CO2_Flux_CONTROL = mean(CO2_Flux), CH4_Flux_CONTROL = mean(CH4_Flux)) ->
  control_data

dat_plot %>% 
  left_join(control_data, by = c("Year", "DOY", "Plot")) %>% 
  mutate(CO2_treatment_diff = (CO2_Flux - CO2_Flux_CONTROL) / CO2_Flux_CONTROL,
         CH4_treatment_diff = (CH4_Flux - CH4_Flux_CONTROL) / CH4_Flux_CONTROL) %>% 
  group_by(Year, DOY, Plot, Treatment) %>% 
  summarise(Timestamp = mean(Timestamp),
            CO2_treatment_diff = mean(CO2_treatment_diff),
            CH4_treatment_diff = mean(CH4_treatment_diff)) ->
  diffs

p <- ggplot(diffs, aes(Timestamp, CO2_treatment_diff, color = Treatment)) + 
  geom_line() + 
  facet_wrap(~Plot) +
  scale_y_continuous(labels = scales::percent)
print(p)
ggsave("treatment_diff_co2.png", width = 9, height = 6)
p <- ggplot(diffs, aes(Timestamp, CH4_treatment_diff, color = Treatment)) + 
  geom_line() + 
  facet_wrap(~Plot) +
  scale_y_continuous(labels = scales::percent) +
  coord_cartesian(ylim = c(-5, 5))
print(p)
ggsave("treatment_diff_ch4.png", width = 9, height = 6)

