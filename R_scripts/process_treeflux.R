
# install.packages("remotes")
# remotes::install_github("COMPASS-DOE/fluxfinder")

library(fluxfinder)
library(dplyr)
library(stringr)
library(ggplot2)
library(lubridate)

# Get names of data files
# from TEMPEST I and II (2022 and 2023)
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

# OK, we've got the input files, now what?

# create single licor file for testing
tree_data_raw %>%
    filter(File == "TG10-01028-2022-06-19T000000_Nick.data") -> Nick
# check timestamps
head(Nick$TIMESTAMP)

# filter to one day
Nick %>%
    filter(date(TIMESTAMP) == "2022-06-20") -> Nick_pre_treatment

# we have data, yes?
ggplot(Nick_pre_treatment, aes(x = TIMESTAMP, y = CH4)) +
         geom_point() +
         ylim(1950, 2050) +
         ggtitle("Seawater PreTreatment 2022")

# filter metadata for the same day
meta_dat %>%
    filter(collection_date == "2022-06-20",
           plot == "Seawater") -> pretreatment_seawater
# we know it's seawater because we used our human eyes
# should think of a way to automate that matching

# matchy match?
Nick_pre_treatment$match <-
ffi_metadata_match(
    data_timestamps = Nick_pre_treatment$TIMESTAMP,
    start_dates = as.character(pretreatment_seawater$collection_date),
    start_times = pretreatment_seawater$start_clock,
    obs_lengths = pretreatment_seawater$obs_lengths
)

# we have pretty good match, only missing one!
ggplot(data = Nick_pre_treatment,
       aes(x = TIMESTAMP, y = CH4)) +
    geom_point() +
    facet_wrap(.~match, scales = "free") +
    ylim(1950, 2050) +
    ggtitle("Seawater PreTreatment One Day 2022")

# Summary thus far...
# we have pretty good matching using the default settings
# and one to one inputs


# let's test another day or two

# create single licor file for testing
tree_data_raw %>%
    filter(File == "TG10-01286-2023-06-04T050000_Louise.data") -> Louise
head(Louise$TIMESTAMP)

# filter to one day
Louise %>%
    filter(date(TIMESTAMP) == "2023-06-04") -> Louise_oneday

# do we have data?
ggplot(Louise_oneday, aes(x = TIMESTAMP, y = CH4)) +
    geom_point() +
    ylim(1950, 2050) +
    ggtitle("Louise Pretreatment 2023")

# filter metadata
meta_dat %>%
    filter(collection_date == "2023-06-04",
           plot == "Control") -> pretreatment_control
# matching!
Louise_oneday$match <-
    ffi_metadata_match(
        data_timestamps = Louise_oneday$TIMESTAMP,
        start_dates = as.character(pretreatment_control$collection_date),
        start_times = pretreatment_control$start_clock,
        obs_lengths = pretreatment_control$obs_lengths
    )

# did we match?
ggplot(data = Louise_oneday,
       aes(x = TIMESTAMP, y = CH4)) +
    geom_point() +
    facet_wrap(.~match, scales = "free") +
    ylim(1950, 2050) +
    ggtitle("Control PreTreatment One Day 2023")

# second test, passed!


# create single licor file for testing
tree_data_raw %>%
    filter(File == "TG10-01448-2023-06-04T050000_Mike.data") -> Mike
tail(Mike$TIMESTAMP)

# filter to one day
Mike %>%
    filter(date(TIMESTAMP) == "2023-06-09") -> Mike_oneday

# do we have data?
ggplot(Mike_oneday, aes(x = TIMESTAMP, y = CH4)) +
    geom_point() +
    ylim(1950, 2050) +
    ggtitle("Mike Posttreatment 2023")

# filter metadata
meta_dat %>%
    filter(collection_date == "2023-06-09",
           plot == "Freshwater") -> posttreatment_fw
# matching!
Mike_oneday$match <-
    ffi_metadata_match(
        data_timestamps = Mike_oneday$TIMESTAMP,
        start_dates = as.character(posttreatment_fw$collection_date),
        start_times = posttreatment_fw$start_clock,
        obs_lengths = posttreatment_fw$obs_lengths
    )

# did we match?
ggplot(data = Mike_oneday,
       aes(x = TIMESTAMP, y = CH4)) +
    geom_point() +
    facet_wrap(.~match, scales = "free") +
    ylim(1950, 2050) +
    ggtitle("Mike One Day 2023")
