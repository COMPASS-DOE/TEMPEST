
# install.packages("remotes")
# remotes::install_github("COMPASS-DOE/fluxfinder")

library(fluxfinder)
library(dplyr)
library(lubridate)

# Get names of data files
files <- list.files("Data/tree_flux_licor/", pattern = "\\.data$", full.names = TRUE)

# Helper function
read_file <- function(f) {
  message("Reading ", f)
  ffi_read_LI7810(f) %>%
    mutate(File = basename(f))
}

#bind all files into raw dataframe
lapply(files, read_file) %>%
  bind_rows() -> tree_data_raw

#read in metadata
meta22 <- read.csv("Data/tree_flux_licor/metadata & excel files/tree_flux_metadata22.csv ")
meta23 <- read.csv("Data/tree_flux_licor/metadata & excel files/tree_flux_metadata23.csv ")

library(stringr)

meta22 %>%
    mutate(start_string = paste(collection_date, start_time),
           start_timestamp = as.POSIXct(start_string, format = "%m/%d/%Y %H:%M", tz = "EST"),
           start_clock = paste0(start_time, ":00"),
           start_clock = str_pad(start_clock, width=8, side="left", pad="0"),
           end_string = paste(collection_date, end_time),
           end_timestamp = as.POSIXct(end_string, format = "%m/%d/%Y %H:%M", tz = "EST"),
           collection_date = date(start_timestamp),
           obs_lengths = 240) -> meta_dat22

meta23 %>%
    mutate(start_string = paste(collection_date, start_time),
           start_timestamp = as.POSIXct(start_string, format = "%m/%d/%Y %H:%M", tz = "EST"),
           start_timestamp = start_timestamp - hours(1), #2023 metadata recorded in EDT
           start_clock = paste0(start_time, ":00"),
           start_clock = str_pad(start_clock, width=8, side="left", pad="0"),
           #start_clock = hms(start_clock),
           #start_clock = start_clock - hours(1),
           end_string = paste(collection_date, end_time),
           end_timestamp = as.POSIXct(end_string, format = "%m/%d/%Y %H:%M", tz = "EST"),
           end_timestamp = end_timestamp - hours(1),
           collection_date = date(start_timestamp),
           obs_lengths = 240) -> meta_dat23

#this would be a good place for some defensive programming
#script should stop is start/end_timestamp and collection_date aren't correctly converted

meta_dat <- bind_rows(meta_dat22, meta_dat23)
meta_dat %>%
    arrange(start_timestamp) -> meta_dat


#plot one slope
library(ggplot2)

#create single licor file for testing
tree_data_raw %>%
    filter(File == "TG10-01028-2022-06-19T000000_Nick.data") -> Nick

head(Nick$TIMESTAMP)

# filter to one day
Nick %>%
    filter(date(TIMESTAMP) == "2022-06-20"
           #TIMESTAMP < "2022-06-20 13:10:00 EDT",
           #TIMESTAMP > "2022-06-20 12:45:00 EDT"
           ) -> Nick_pre_treatment

ggplot(Nick_pre_treatment, aes(x = TIMESTAMP, y = CH4)) +
    geom_point() +
    ylim(1950, 2050) +
    ggtitle("Seawater PreTreatment")


#filter metadata
meta_dat %>%
    filter(collection_date == "2022-06-20",
           plot == "Seawater") -> first_day22_seawater



nick_first_day_w_meta <-
ffi_metadata_match(
    data_timestamps = Nick_pre_treatment$TIMESTAMP,
    start_dates = first_day22_seawater$start_timestamp,
    start_times = first_day22_seawater$start_clock,
    obs_lengths = first_day22_seawater$obs_lengths
)
#returns
#Error in ffi_metadata_match(data_timestamps = mike$TIMESTAMP, start_dates = meta_dat[meta_dat$collection_date ==  :
#start_timestamps overlaps: 2, 4, 6, 7, 8, 9, 10, 11, 12, 13, 15, 16, 17, 18, 19, 20, 21, 22, 24, 25, 26, 27, 28, 29, 31, 32, 34, 35, 36, 37, 38, 40, 41, 42, 43, 45, 46, 47, 48, 49

meta_dat %>%
    filter(plot == "Freshwater") -> f_dat

#mike_w_meta <-
ffi_metadata_match(
    data_timestamps = mike$TIMESTAMP,
    start_dates = f_dat[f_dat$collection_date == "2023-06-04",]$start_timestamp,
    start_times = f_dat[f_dat$collection_date == "2023-06-04",]$start_clock,
    obs_lengths = f_dat[f_dat$collection_date == "2023-06-04",]$obs_lengths
) -> wtf
#returns a large numeric element and a warning that there are 18 entries with no matches
#element converted into a dataframe is a single column of NA the length of "mike"

meta_dat %>%
    filter(plot == "Control") -> c_dat

#mike_w_meta <-
ffi_metadata_match(
    data_timestamps = mike$TIMESTAMP,
    start_dates = c_dat[c_dat$collection_date == "2023-06-04",]$start_timestamp,
    start_times = c_dat[c_dat$collection_date == "2023-06-04",]$start_clock,
    obs_lengths = c_dat[c_dat$collection_date == "2023-06-04",]$obs_lengths
) -> ugh
#returns a large numeric element and a warning that there are 18 entries with no matches
#element converted into a dataframe is a single column of NA the length of "mike"


meta_dat %>%
    filter(plot == "Seawater") -> s_dat

#mike_w_meta <-
ffi_metadata_match(
    data_timestamps = mike$TIMESTAMP,
    start_dates = s_dat[s_dat$collection_date == "2023-06-04",]$start_timestamp,
    start_times = s_dat[s_dat$collection_date == "2023-06-04",]$start_clock,
    obs_lengths = s_dat[s_dat$collection_date == "2023-06-04",]$obs_lengths
) -> fingers_crossed

#Error in ffi_metadata_match(data_timestamps = mike$TIMESTAMP, start_dates = s_dat[s_dat$collection_date ==  :
#  start_timestamps overlaps: 5






split(tree_data_raw, f = tree_data_raw$File) -> grouped_tree_data

# Helper function
match_data <- function(m) {
    message("Matching ", m)
    ffi_metadata_match(data_timestamps = m$TIMESTAMP,
                       start_dates = meta_dat$start_timestamp,
                       start_times = hms(meta_dat$start_timestamp),
                       obs_lengths = meta_dat$obs_lengths)
}

lapply(grouped_tree_data, match_data) -> tree_data_matched

#at some point will need to pull air temperature data
