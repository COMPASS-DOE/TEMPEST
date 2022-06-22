# TODO: this is preliminary, as I don't have Dropbox access

library(readr)
library(lubridate)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(kableExtra)
set.seed(7)

# read_aquatroll <- function(filename, token, total_files) {
#
#     # If we're running in a Shiny session, update progress bar
#     if(!is.null(getDefaultReactiveDomain())) {
#         incProgress(1 / total_files)
#     }
#
#     # download file to temp file
#     drop_read_csv(filename, dtoken = token)
# }

process_aquatroll <- function(token, datadir) {
    # Generate list of 'current' AquaTroll files
    at_dir <- drop_dir(datadir, dtoken = token)
    at_files_600 <- grep(at_dir$path_display, pattern = "WaterLevel600", value = TRUE)
    at_files_200 <- grep(at_dir$path_display, pattern = "WaterLevel200", value = TRUE)

    change_date <- "2021-03-10 00:00:00"

    change_IDs <- c("PNNL_13", "PNNL_23", "PNNL_32")
    change_instrument <- "TROLL600"

    troll_inventory <- read_csv("aquatroll_inventory copy.csv")

    lapply(at_files_600, read_600, token, length(at_files_600)) %>%
        bind_rows() %>%
        mutate(Install = if_else(Timestamp >= change_date &
                                     Logger_ID %in% change_IDs &
                                     Instrument == change_instrument, 2, 1)) %>%
        left_join(troll_inventory, by = c("Logger_ID", "Instrument", "Install")) -> aquatroll_raw_600

    lapply(at_files_200, read_200, token, length(at_files_200)) %>%
        bind_rows() %>%
        mutate(Install = if_else(Timestamp >= change_date &
                                     Logger_ID %in% change_IDs &
                                     Instrument == change_instrument, 2, 1)) %>%
        left_join(troll_inventory, by = c("Logger_ID", "Instrument", "Install")) -> aquatroll_raw_200

    list(aquatroll_600 = aquatroll_raw_600,
         aquatroll_200 = aquatroll_raw_200)
# aquatroll_raw <- FALSE
#
#
#     if(nrow(aquatroll_raw)) {
#         aquatroll_raw %>%
#             separate(Probe_Name, into = c("junk", "probenum"), sep = "_GW", remove = FALSE) %>%
#             mutate(Probe_ShortName = paste0("GW", probenum)) %>%
#             select(-junk, -probenum) %>%
#             left_join(select(at_inventory, Probe_ShortName = InstallationMethod_ID, Plot_long = Site_ID), by = "Probe_ShortName")
#     } else { # no data
        # tibble(Timestamp = NA_Date_, Probe_ShortName = NA_character_,
        #        Plot_long = NA_character_, Temp = NA_real_, Plot = NA_character_)
#    }
}

# read in AquaTroll 600 data the manual way
read_600 <- function(filename, token, total_files) {

    # If we're running in a Shiny session, update progress bar
    if(!is.null(getDefaultReactiveDomain())) {
        incProgress(1 / total_files)
    }

    # download file to temp file
    drop_download(filename, local_path = "tempfile.dat",
                  dtoken = token,
                  overwrite = TRUE)

    read_delim("tempfile.dat",
               skip=1, delim=",", col_names=T) %>%
        dplyr::slice(3:n())%>% #remove junk header
        dplyr::mutate(Timestamp = force_tz(parsedate::parse_date(TIMESTAMP), tzone ="EST"),
                      Depth = as.numeric(Depth600),
                      Temp = as.numeric(Temperature600),
                      #Specific_Conductivity = as.numeric(Specific_Conductivity600),
                      Salinity = as.numeric(Salinity600),
                      #DO_sat = as.numeric(RDO_perc_sat600),
                      DO_mgl = as.numeric(RDO_concen600),
                      #pH = as.numeric(pH600),
                      #ORP = as.numeric(pH_ORP600),
                      #eH = ORP + V0 + dV.dT * Temp,
                      #Density = as.numeric(Water_Density600),
                      Pressure_psi = as.numeric(Pressure600),
                      #Pressure_mbar = Pressure_psi * 68.948,
                      #Resistivity = as.numeric(Resistivity600),
                      Instrument = "TROLL600") %>%
        rename(Logger_ID = Statname) %>%
        dplyr::select(Timestamp, Temp, Salinity, DO_mgl,
                      Pressure_psi, Instrument, Logger_ID)
}


# read in AquaTroll 200 data the manual way
read_200 <- function(filename, token, total_files) {

    # If we're running in a Shiny session, update progress bar
    if(!is.null(getDefaultReactiveDomain())) {
        incProgress(1 / total_files)
    }

    # download file to temp file
    drop_download(filename, local_path = "tempfile.dat",
                  dtoken = token,
                  overwrite = TRUE)

    read_delim("tempfile.dat",
               skip=1, delim=",", col_names=T) %>%
        slice(3:n()) %>% #remove junk header
        dplyr::mutate(Timestamp = force_tz(parsedate::parse_date(TIMESTAMP), tzone ="EST"),
                      #Depth = as.numeric(Depth),
                      Temp = as.numeric(Temperature),
                      #Specific_Conductivity = as.numeric(Specific_Conductivity),
                      Salinity = as.numeric(Salinity),
                      #Density = as.numeric(Water_Density),
                      Pressure_psi = as.numeric(Pressure),
                      #Pressure_mbar = Pressure_psi * 68.948,
                      #Resistivity = as.numeric(Resistivity),
                      Instrument = "TROLL200") %>%
        rename(Logger_ID = Statname) %>%
        dplyr::select(Timestamp, Logger_ID, Temp, Pressure_psi, Salinity, Instrument)
}

