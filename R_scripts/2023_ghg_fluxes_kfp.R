## TEMPEST 2023
## preliminary processing and graphs for Tree GHG (chambers) and Soil GHG during TEMPEST 2023
## fluxes were measured using LICORS and manually recorded in the field
## Data were scanned and transcribed into Google Docs.
## kfp 2023-06-21


# setup -------------------------------------------------------------------

# packages
library(tidyverse)
library(googlesheets4)
theme_set(theme_bw())

# set a dataframe for labelling the flood events
flood_label <-
    tribble(
        ~plot, ~timepoint, ~label,
        "freshwater", "T2", "flood 1 (Jun 6)",
        "seawater", "T2", "flood 1 (Jun 6)",
        "freshwater", "T3B", "flood 2 (Jun 7)",
        "seawater", "T3B", "flood 2 (Jun 7)",
    ) %>%
    recode_days(.)

recode_days <- function(dat){
    dat %>%
        mutate(timepoint = str_replace(timepoint, "T", "day "),
               timepoint = recode(timepoint, "day 3A" = "day 3 (am)", "day 3B" = "day 3 (pm)"))
}


#
# Tree GHG fluxes ---------------------------------------------------------
tree_flux <- read_sheet("1IsnDv34SwgniJIkGiV7zRldr6zyIoZXjRXI7mmVFWwI")
tree_flux2 <-
    tree_flux %>%
    mutate(plot = tolower(plot),
           start_time = format(as.POSIXct(start_time), format = "%H:%M"),
           end_time = format(as.POSIXct(end_time), format = "%H:%M")) %>%
    filter(!is.na(timepoint)) %>%
    recode_days(.)

    # tree_flux2 %>%
    #     ggplot(aes(x = timepoint, y = flux_CO2_ppms, color = plot, fill = plot))+
    #     geom_boxplot(width = 0.5,
    #                  color = "black",
    #                  position = position_dodge(width = 0.7),
    #                  outlier.colour = NA,
    #                  alpha = 0.1)+
    #     geom_point(size = 2,
    #                position = position_dodge(width = 0.7))
    #

tree_flux2 %>%
    ggplot(aes(x = timepoint, y = flux_CO2_ppms,
               color = ID, group = ID))+
    geom_point(size = 2)+
    geom_line()+
    geom_segment(data = flood_label, aes(x = timepoint, xend = timepoint, y = 1.8, yend = 1.3),
                 color = "black", group = 1,
                 arrow = arrow(length = unit(2, "mm")))+
    geom_text(data = flood_label, aes(x = timepoint, y = 1.6, label = label),
              color = "black", group = 1, angle = 90, vjust = -0.5)+
    facet_wrap(~plot)+
    labs(title = "Tree GHG fluxes - CO2",
         subtitle = "TEMPEST 2023",
         x = "",
         y = "CO2 flux, ppm/s")+
    theme(legend.position = "none")

tree_flux2 %>%
    ggplot(aes(x = timepoint, y = flux_CH4_ppbs,
               color = ID, group = ID))+
    geom_point(size = 2)+
    geom_line()+
    geom_segment(data = flood_label, aes(x = timepoint, xend = timepoint, y = 1.0, yend = 0.65),
                 color = "black", group = 1,
                 arrow = arrow(length = unit(2, "mm")))+
    geom_text(data = flood_label, aes(x = timepoint, y = 0.8, label = label),
              color = "black", group = 1, angle = 90, vjust = -0.5)+
    facet_wrap(~plot)+
    labs(title = "Tree GHG fluxes - CH4",
         subtitle = "TEMPEST 2023",
         x = "",
         y = "CH4 flux, ppb/s")+
    theme(legend.position = "none")

#

# Soil GHG fluxes ---------------------------------------------------------
soil_flux <- read_sheet("1VdefK8Vrg4QtwAuxxHmUrcrBFpCVDPlvHA6oE523VLI")
soil_flux2 <-
    soil_flux %>%
    mutate(time = format(as.POSIXct(time), format = "%H:%M"),
           plot = tolower(plot),
           collar = as.character(collar),
           plot = dplyr::recode(plot, "fresh" = "freshwater", "salt" = "seawater")) %>%
    filter(!is.na(timepoint)) %>%
    recode_days(.)

# graphs
soil_flux2 %>%
    ggplot(aes(x = timepoint, y = CO2,
               color = collar, group = collar))+
    geom_point(size = 2)+
    geom_line()+
    geom_segment(data = flood_label, aes(x = timepoint, xend = timepoint, y = 40, yend = 25),
                 color = "black", group = 1,
                 arrow = arrow(length = unit(2, "mm")))+
    geom_text(data = flood_label, aes(x = timepoint, y = 35, label = label),
              color = "black", group = 1, angle = 90, vjust = -0.5)+
    facet_wrap(~plot)+
    labs(title = "Soil GHG fluxes - CO2",
         subtitle = "TEMPEST 2023",
         x = "",
         y = "CO2 flux, μmol/m2/s")+
    theme(legend.position = "none")


soil_flux2 %>%
    ggplot(aes(x = timepoint, y = CH4,
               color = collar, group = collar))+
    geom_point(size = 2)+
    geom_line()+
    geom_segment(data = flood_label, aes(x = timepoint, xend = timepoint, y = 10, yend = 3.5),
                 color = "black", group = 1,
                 arrow = arrow(length = unit(2, "mm")))+
    geom_text(data = flood_label, aes(x = timepoint, y = 7.5, label = label),
              color = "black", group = 1, angle = 90, vjust = -0.5)+
    facet_wrap(~plot)+
    labs(title = "Soil GHG fluxes - CH4",
         subtitle = "TEMPEST 2023",
         x = "",
         y = "CH4 flux, nmol/m2/s")+
    theme(legend.position = "none")
