
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
           obs_lengths = 100) -> meta_dat22

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
           obs_lengths = 100) -> meta_dat23

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
    filter(#date(TIMESTAMP) == "2022-06-20"
           #TIMESTAMP < "2022-06-20 13:10:00 EDT",
           #TIMESTAMP > "2022-06-20 12:45:00 EDT"
           ) -> Nick_event1 #pre_treatment

ggplot(Nick_pre_treatment, aes(x = TIMESTAMP, y = CH4)) +
    geom_point() +
    ylim(1950, 2050) +
    ggtitle("Seawater PreTreatment")

#filter metadata
meta_dat %>%
    filter(year(collection_date) == "2022",
           plot == "Seawater") -> Event1_seawater
Event1_seawater <- Event1_seawater[-80,]
Event1_seawater <- Event1_seawater[-1,]

Nick_pre_treatment$match <-
ffi_metadata_match(
    data_timestamps = Nick_pre_treatment$TIMESTAMP,
    start_dates = as.character(first_day22_seawater$collection_date),
    start_times = first_day22_seawater$start_clock,
    obs_lengths = first_day22_seawater$obs_lengths
)

Nick_event1$match <-
    ffi_metadata_match(
        data_timestamps = Nick_event1$TIMESTAMP,
        start_dates = as.character(Event1_seawater$collection_date),
        start_times = Event1_seawater$start_clock,
        obs_lengths = Event1_seawater$obs_lengths
    )

Nick_event1 %>%
    filter(! is.na(match)) -> Seawater_Event1_matched

#need to add tree ids to each flux chunk
Event1_seawater %>%
    select(ID) %>%
    mutate(match = 1:142) %>%
    right_join(Seawater_Event1_matched, by = "match") -> Seawater_Event1_matched

Nick_pre_treatment %>%
    filter(! is.na(match)) -> matched_example


ggplot(matched_example, aes(x = TIMESTAMP, y = CH4, color = as.factor(match))) +
    geom_point() +
    ylim(1950, 2050) +
    ggtitle("Seawater PreTreatment") +
    facet_wrap(~as.factor(match), scales = "free_x")


ggplot(Seawater_Event1_matched[Seawater_Event1_matched$ID == "S1",],
       aes(x = TIMESTAMP, y = CH4)) +
    geom_point() +
    ylim(1950, 2050) +
    ggtitle("Seawater TEMPEST II") +
    facet_wrap(~date(TIMESTAMP), scales = "free")


split(tree_data_raw, f = tree_data_raw$File) -> grouped_tree_data

# Helper function
match_data <- function(m) {
    message("Matching ", m)
    ffi_metadata_match(data_timestamps = m$TIMESTAMP,
                       start_dates = as.character(meta_dat$collection_date),
                       start_times = meta_dat$start_clock,
                       obs_lengths = meta_dat$obs_lengths)
}

lapply(grouped_tree_data, match_data) -> tree_data_raw$matched

#at some point will need to pull air temperature data
