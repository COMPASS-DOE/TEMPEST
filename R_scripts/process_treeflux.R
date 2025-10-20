# process_treeflux.R
# Script to process raw Licor files into
# ready-for-analysis data matched with metadata
# KAM 2025

# install.packages("remotes")
# remotes::install_github("COMPASS-DOE/fluxfinder")

library(fluxfinder)
library(dplyr)
library(stringr)
library(ggplot2)
theme_set(theme_bw())
library(lubridate)

# Get names of data files
# from TEMPEST I and II (2022 and 2023)
files <- list.files("Data/tree_flux_licor/", pattern = "\\.data$", full.names = TRUE)

# Helper function
read_file <- function(f) {
    message("\nReading ", basename(f))
    ffi_read_LI7810(f) %>%
        mutate(File = basename(f)) %>%
        select(TIMESTAMP, TZ, CO2, CH4, SN, File)
}

# Bind all data files into raw dataframe
lapply(files, read_file) %>%
    bind_rows() ->
    tree_data_raw

# Read in metadata and construct start/end timestamps
message("Reading metadata...")
meta22 <- read_csv("Data/tree_flux_licor/metadata_excel_files/tree_flux_metadata22.csv",
                   col_types = "ccccccddcc")
meta23 <- read_csv("Data/tree_flux_licor/metadata_excel_files/tree_flux_metadata23.csv",
                   col_types = "ccccccddcc")

meta22 %>%
    bind_rows(meta23) %>%
    mutate(start_timestamp = mdy_hm(paste(collection_date, start_time), tz = "EST"),
           end_timestamp = mdy_hm(paste(collection_date, end_time), tz = "EST")) %>%
    select(-start_time, -end_time, -collection_date, -notes) %>%
    arrange(start_timestamp) ->
    md

# Check for missing/bad entries
if(any(is.na(md$start_timestamp))) {
    stop("Some start times not valid; missing info or not mm/dd/yyyy?")
}
if(any(is.na(md$end_timestamp))) {
    stop("Some end times not valid; missing info or not mm/dd/yyyy?")
}

# OK, we've got the input files, now what?

# We use a "treeflux-processing-info" file to step through the data. This
# simplifies things and provides a documentary record of decisions, etc.
message("Reading processing info file...")
tfpi <- read_csv("Data/tree_flux_licor/treeflux-processing-info.csv", col_types = "cDcc")

i <- 1

FILE <- tfpi$File[i]
DATE <- tfpi$Date[i]
PLOT <- tfpi$Plot[i]
I_STR <- sprintf("%02s", i)

message(paste("Processing", i, FILE, DATE, PLOT))

# Filter to one Licor file and one day for testing
tree_data_raw %>%
    filter(File == FILE) %>%
    filter(date(TIMESTAMP) == DATE) ->
    tree_data_filtered
message("\t", nrow(tree_data_filtered), " rows of data")

# Here and below, we use CO2 for plotting because we *know* it has
# to be emitted, not taken up, by tree stems, which makes it easier
# to diagnose matching problems
ggplot(tree_data_filtered, aes(x = TIMESTAMP, y = CO2)) +
    geom_point(na.rm = TRUE) +
    ylim(300, 1000) +
    ggtitle("Seawater PreTreatment 2022")

# Filter metadata for the same day
md %>%
    filter(date(start_timestamp) == DATE,
           plot == PLOT) %>%
    mutate(start_times = paste(hour(start_timestamp),
                               minute(start_timestamp),
                               second(start_timestamp), sep = ":")) ->
    md_filtered
message("\t", nrow(md_filtered), " rows of metadata")

TIMEPOINT <- unique(md_filtered$timepoint)
if(length(TIMEPOINT) > 1) {
    stop("Hmm, this should not happen!")
}

# matchy match?
message("Matching...")
tree_data_filtered$match <-
    ffi_metadata_match(
        data_timestamps = tree_data_filtered$TIMESTAMP,
        start_dates = as.character(date(md_filtered$start_timestamp)),
        start_times = md_filtered$start_times,
        obs_lengths = md_filtered$obs_length
    )
tree_data_filtered$ID <- md_filtered$ID[tree_data_filtered$match]

test <- tree_data_filtered %>% left_join(md_filtered, by = "ID")

# Diagnostic plot 1: color data by match
ggplot(tree_data_filtered, aes(x = TIMESTAMP, y = CO2, color = factor(match))) +
    geom_point(na.rm = TRUE) +
    ylim(300, 1000) +
    ggtitle(paste(I_STR, PLOT, TIMEPOINT, DATE, "matched"), subtitle = tfpi$Notes[i])

DIR_ROOT <- "Data/tree_flux_licor/processing_outputs/"
FN_ROOT <- paste(I_STR, FILE, DATE, PLOT, sep = "_")
ggsave(file.path(DIR_ROOT, paste0(FN_ROOT, "_match.png")), width = 10, height = 6)

# Diagnostic plot 2: individual tree data with
ggplot(tree_data_filtered, aes(x = TIMESTAMP, y = CO2)) +
    geom_point(na.rm = TRUE) +
    facet_wrap(. ~ ID, scales = "free_x") +
    ylim(300, 1000) +
    geom_vline(data = md_filtered,
               aes(xintercept = start_timestamp + start_adjust),
               linetype = 2, color = "darkgreen") +
    geom_vline(data = md_filtered,
               aes(xintercept = start_timestamp + start_adjust + obs_length),
               linetype = 2, color = "darkred") +
    ggtitle(paste(I_STR, PLOT, TIMEPOINT, DATE, "fluxwindows"))
ggsave(file.path(DIR_ROOT, paste0(FN_ROOT, "_fluxwindows.png")), width = 10, height = 6)


stop("All done")


# let's test another day or two

# create single licor file for testing
tree_data_raw %>%
    filter(File == "TG10-01286-2023-06-04T050000_Louise.data") ->
    Louise

# filter to one day
Louise %>%
    filter(date(TIMESTAMP) == "2023-06-04") ->
    Louise_oneday

# do we have data?
ggplot(Louise_oneday, aes(x = TIMESTAMP, y = CH4)) +
    geom_point() +
    ylim(1950, 2050) +
    ggtitle("Louise Pretreatment 2023")

# filter metadata
md %>%
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
md %>%
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
