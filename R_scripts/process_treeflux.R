
# install.packages("remotes")
# remotes::install_github("COMPASS-DOE/fluxfinder")

library(fluxfinder)
library(dplyr)
library(lubridate)

#set data path
setwd("C:/Users/morr497/OneDrive - PNNL/Documents/TEMPEST/TEMPEST/Data/")

# Get names of data files
files <- list.files("tree_flux_licor/", pattern = "\\.data$", full.names = TRUE)

# Helper function
read_file <- function(f) {
  message("Reading ", f)
  ffi_read_LI7810(f) %>%
    mutate(File = basename(f))
}

#bind all files into raw dataframe
lapply(files, read_file) %>%
  bind_rows() -> tree_data_raw

tree_data_one <- tree_data_raw[1:650,]

#read in metadata
meta22 <- read.csv("tree_flux_licor/metadata & excel files/tree_flux_metadata22.csv ")
meta23 <- read.csv("tree_flux_licor/metadata & excel files/tree_flux_metadata23.csv ")

meta_dat <- bind_rows(meta22, meta23)

meta_dat %>%
    mutate(start_string = paste(collection_date, start_time),
           start_timestamp = as.POSIXct(start_string, format = "%m/%d/%Y %H:%M", tz = "EST"),
           end_string = paste(collection_date, end_time),
           end_timestamp = as.POSIXct(end_string, format = "%m/%d/%Y %H:%M", tz = "EST"),
           collection_date = date(start_timestamp),
           obs_lengths = 600) -> meta_dat2#%>%
    #select(-c(start_string, end_string)) -> meta_dat

meta_dat2 %>%
    filter(collection_date == "2022-06-20") %>%
    arrange(start_timestamp) -> one_day_meta_dat

#tree_w_meta <-
ffi_metadata_match(
    data_timestamps = tree_data_raw$TIMESTAMP,
    start_dates = meta_dat$start_timestamp,
    start_times = hm(meta_dat$start_timestamp),
    obs_lengths = meta_dat$obs_lengths
)

#tree_one_w_meta <-
ffi_metadata_match(
    data_timestamps = tree_data_one$TIMESTAMP,
    start_dates = one_day_meta_dat$start_timestamp,
    start_times = hm(one_day_meta_dat$start_timestamp),
    obs_lengths = one_day_meta_dat$obs_lengths
)

#at some point will need to pull air temperature data
