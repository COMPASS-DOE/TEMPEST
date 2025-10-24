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
library(readr)

INPUT_DIR_ROOT <- "Data/tree_flux_licor/"
OUTPUT_DIR_ROOT <- "Data/tree_flux_licor/processing_outputs/"

# Get names of data files from TEMPEST I and II (2022 and 2023)
files <- list.files(INPUT_DIR_ROOT, pattern = "\\.data$", full.names = TRUE)

# Helper function
read_file <- function(f) {
    message("\nReading ", basename(f))
    ffi_read_LI7810(f) %>%
        mutate(File = basename(f)) %>%
        select(TIMESTAMP, TZ, CO2, CH4, SN, File)
}

# Bind all data files into raw dataframe
# Reading the files is a a bit slow so skip if possible
if(!exists("tree_data_raw")) {
    lapply(files, read_file) %>%
        bind_rows() %>%
        as_tibble() %>%
        # although the Licor's timezone settings are "America/New_York" this
        # is incorrect -- they *should be* maintained at EST. So change this
        mutate(TIMESTAMP = force_tz(TIMESTAMP, tzone = "EST")) ->
        tree_data_raw
}

# Read in metadata and construct start/end timestamps
message("Reading metadata...")
meta22 <- read_csv(file.path(INPUT_DIR_ROOT, "metadata_excel_files/tree_flux_metadata22.csv"),
                   col_types = "ccccccddcc")
meta23 <- read_csv(file.path(INPUT_DIR_ROOT, "metadata_excel_files/tree_flux_metadata23.csv"),
                   col_types = "ccccccddcc")

meta22 %>%
    bind_rows(meta23) %>%
    mutate(start_timestamp = mdy_hm(paste(collection_date, start_time), tz = "EST"),
           end_timestamp = mdy_hm(paste(collection_date, end_time), tz = "EST")) %>%
    # we will get time zone information from treeflux-processing-info.csv
    select(-start_time, -end_time, -collection_date, -timezone, -notes) %>%
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
tfpi <- read_csv(file.path(INPUT_DIR_ROOT, "treeflux-processing-info.csv"),
                 col_types = "cDccccc")

#for(i in seq_len(nrow(tfpi))) {
i <- 25

I_STR <- sprintf("%02s", i)
FILE <- tfpi$File[i]
DATE <- tfpi$Date[i]
TIMEPOINT <- tfpi$Timepoint[i]
PLOT <- tfpi$Plot[i]
MD_TZ <- tfpi$Metadata_tz[i]
INS_TZ <- tfpi$Instrument_tz[i]
NOTES <- tfpi$Notes[i]

message(paste("Processing", I_STR, FILE, DATE, TIMEPOINT, PLOT))

# Filter to one Licor file and one day for testing
tree_data_raw %>%
    filter(File == FILE) %>%
    filter(date(TIMESTAMP) == DATE) ->
    tree_data_filtered
message("\t", nrow(tree_data_filtered), " rows of data")

# ---- Licor data time zone conversion, if needed ----
if(INS_TZ != "EST") {
    message("Converting instrument times from ", INS_TZ, " to EST")
    tree_data_filtered$TIMESTAMP <- force_tz(tree_data_filtered$TIMESTAMP, tzone = INS_TZ)
    tree_data_filtered$TIMESTAMP <- with_tz(tree_data_filtered$TIMESTAMP, tzone = "EST")
}

# Here and below, we use CO2 for plotting because we *know* it has
# to be emitted, not taken up, by tree stems, which makes it easier
# to diagnose matching problems
ggplot(tree_data_filtered, aes(x = TIMESTAMP, y = CO2)) +
    geom_point(na.rm = TRUE) +
    ylim(300, 1000) +
    ggtitle(paste(I_STR, PLOT, DATE, TIMEPOINT),
            subtitle = NOTES)

# ---- Filter metadata for the same day ----
md %>%
    filter(date(start_timestamp) == DATE,
           timepoint == TIMEPOINT,
           plot == PLOT) ->
    md_filtered
message("\t", nrow(md_filtered), " rows of metadata")

# ---- Metadata time zone conversion, if needed ----
if(MD_TZ != "EST") {
    message("Converting metadata times from ", MD_TZ, " to EST")
    md_filtered$start_timestamp <- force_tz(md_filtered$start_timestamp, tzone = MD_TZ)
    md_filtered$start_timestamp <- with_tz(md_filtered$start_timestamp, tzone = "EST")
    md_filtered$end_timestamp <- force_tz(md_filtered$end_timestamp, tzone = MD_TZ)
    md_filtered$end_timestamp <- with_tz(md_filtered$end_timestamp, tzone = "EST")
}

# Construct start timestamps needed by ffi_metadata_match
md_filtered %>%
    mutate(start_times = paste(hour(start_timestamp),
                           minute(start_timestamp),
                           second(start_timestamp), sep = ":")) ->
    md_filtered
# ---- matchy match? ----
message("Matching...")
tree_data_filtered$match <-
    ffi_metadata_match(
        data_timestamps = tree_data_filtered$TIMESTAMP,
        start_dates = as.character(date(md_filtered$start_timestamp)),
        start_times = md_filtered$start_times,
        # to start, match 100 seconds of data
        # this will be refined using the dead_band and obs_length
        # entries in the metadata files
        obs_lengths = rep(100, nrow(md_filtered))
    )
tree_data_filtered$ID <- md_filtered$ID[tree_data_filtered$match]

# ---- Diagnostic plot 1: color data by match ----
p1 <- ggplot(tree_data_filtered, aes(x = TIMESTAMP, y = CO2, color = factor(match))) +
    geom_point(na.rm = TRUE) +
    ylim(300, 1000) +
    ggtitle(paste(I_STR, PLOT, DATE, TIMEPOINT, "matched"),
            subtitle = NOTES)
print(p1)

FN_ROOT <- paste(FILE, DATE, TIMEPOINT, PLOT, sep = "_")
fn <- file.path(OUTPUT_DIR_ROOT, paste0(FN_ROOT, "_match.pdf"))
message("Saving ", basename(fn), "...")
ggsave(fn, width = 10, height = 6)

# ---- Diagnostic plot 2: individual tree data ----
p2 <- ggplot(tree_data_filtered, aes(x = TIMESTAMP, y = CO2)) +
    geom_point(na.rm = TRUE) +
    facet_wrap(. ~ ID, scales = "free_x") +
    ylim(300, 1000) +
    geom_vline(data = md_filtered,
               aes(xintercept = start_timestamp + dead_band),
               linetype = 2, color = "darkgreen") +
    geom_vline(data = md_filtered,
               aes(xintercept = start_timestamp + dead_band + obs_length),
               linetype = 2, color = "darkred") +
    ggtitle(paste(I_STR, PLOT, TIMEPOINT, DATE, "fluxwindows"),
            subtitle = NOTES)
print(p2)

fn <- file.path(OUTPUT_DIR_ROOT, paste0(FN_ROOT, "_fluxwindows.pdf"))
message("Saving ", basename(fn), "...")
ggsave(fn, width = 10, height = 6)

#} # for

stop("All done")
