# TODO: this is preliminary, as I don't have Dropbox access

library(readr)
library(lubridate)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(kableExtra)
set.seed(7)

read_aquatroll <- function(filename, token, total_files) {

    # If we're running in a Shiny session, update progress bar
    if(!is.null(getDefaultReactiveDomain())) {
        incProgress(1 / total_files)
    }

    # download file to temp file
    drop_read_csv(filename, dtoken = token)
}

process_aquatroll <- function(token, datadir) {
    # Generate list of 'current' Aquatroll files
    at_dir <- drop_dir(datadir, dtoken = token)
    at_files <- grep(at_dir$path_display, pattern = "TMP_TROLL", value = TRUE)

    at_inventory <- read_csv("TMP_AquaTroll_InstallationMethods.csv")

    lapply(at_files, fileread, token, length(at_files)) %>% bind_rows() -> aquatroll_raw

    if(nrow(aquatroll_raw)) {
        aquatroll_raw %>%
            separate(Probe_Name, into = c("junk", "probenum"), sep = "_GW", remove = FALSE) %>%
            mutate(Probe_ShortName = paste0("GW", probenum)) %>%
            select(-junk, -probenum) %>%
            left_join(select(at_inventory, Probe_ShortName = InstallationMethod_ID, Plot_long = Site_ID), by = "Probe_ShortName")
    } else { # no data
        tibble(Timestamp = NA_Date_, Probe_ShortName = NA_character_,
               Plot_long = NA_character_, Temp = NA_real_, Plot = NA_character_)
    }
}
