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
library(broom)

INPUT_DIR_ROOT <- "Data/tree_flux_licor/"
OUTPUT_DIR_ROOT <- "Data/tree_flux_licor/processing_outputs/"

# Get names of data files from TEMPEST I and II (2022, 2023, 2024)
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
        mutate(TIMESTAMP = force_tz(TIMESTAMP, tzone = "EST"), TZ = "EST") ->
        tree_data_raw
}

# Read in metadata and construct start/end timestamps
message("Reading metadata...")
meta22 <- read_csv(file.path(INPUT_DIR_ROOT, "metadata_excel_files/tree_flux_metadata22.csv"),
                   col_types = "ccccccddcc")
meta23 <- read_csv(file.path(INPUT_DIR_ROOT, "metadata_excel_files/tree_flux_metadata23.csv"),
                   col_types = "ccccccddcc")
meta24 <- read_csv(file.path(INPUT_DIR_ROOT, "metadata_excel_files/tree_flux_metadata24.csv"),
                   col_types = "ccccccccddddccc")
# meta24 has a different format; rework it to match others
meta24 %>%
    select(-grid_cell, ID = Sapflux_ID, timepoint = Timepoint,
           collection_date = collection_date_YYYYMMDD,
           start_time = start_time_24hr_EDT, end_time = end_time_24hr_EDT,
           -licor_timezone, -flux_CO2_ppms, -flux_CH4_ppbs,
           -instrument, -personnel) %>%
    # make the date string into mm/dd/yyyy
    mutate(collection_date = paste(substr(collection_date, 5, 6),
                                   substr(collection_date, 7, 8),
                                   substr(collection_date, 1, 4), sep = "/")) ->
    meta24

meta22 %>%
    bind_rows(meta23, meta24) %>%
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

results <- list()
for(i in seq_len(nrow(tfpi))) {
#i <- 1

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
    message("\tConverting instrument times from ", INS_TZ, " to EST")
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
    message("\tConverting metadata times from ", MD_TZ, " to EST")
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
message("\tMatching...")
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

# ---- Duplication check ----
message("\tChecking for multiple observations per timestamp")
matched_data <- tree_data_filtered %>% filter(!is.na(match))
if(any(duplicated(matched_data$TIMESTAMP))) {
    # If the 7810's time zone settings gets changed during use,
    # the instrument writes multiple observations per timestamp
    # It *seems* that the last observation is the one we want to keep
    tree_data_filtered %>%
        group_by(TIMESTAMP) %>%
        mutate(obsrep = 1:n()) -> x
    message("\t", sum(x$obsrep > 1), " of ", nrow(tree_data_filtered), " timestamps with duplicate entries")
    # Keep only the last observation
    x %>%
        group_by(TIMESTAMP) %>%
        filter(obsrep == n()) %>%
        select(-obsrep) ->
        tree_data_filtered
    warning("De-duplicated data in ", i, ": multiple obs per timestamp!")
}

# ---- Diagnostic plot 1: color data by match ----
p1 <- ggplot(tree_data_filtered, aes(x = TIMESTAMP, y = CO2, color = factor(match))) +
    geom_point(na.rm = TRUE) +
    ylim(300, 1000) +
    ggtitle(paste(I_STR, PLOT, DATE, TIMEPOINT, "matched"),
            subtitle = NOTES)
print(p1)

FN_ROOT <- paste(DATE, TIMEPOINT, PLOT, sep = "_")
fn <- file.path(OUTPUT_DIR_ROOT, paste0(FN_ROOT, "_match.pdf"))
message("\tSaving ", basename(fn), "...")
ggsave(fn, width = 10, height = 6)

# Detail plot
tree_data_filtered %>% filter(!is.na(match)) -> matches
print(p1 + xlim(c(min(matches$TIMESTAMP), max(matches$TIMESTAMP))))
fn <- file.path(OUTPUT_DIR_ROOT, paste0(FN_ROOT, "_match_detail.pdf"))
message("\tSaving ", basename(fn), "...")
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
message("\tSaving ", basename(fn), "...")
ggsave(fn, width = 10, height = 6)

# Merge data with metadata
tree_data_filtered %>%
    filter(!is.na(match)) %>%
    left_join(md_filtered, by = "ID") %>%
    # Filter for dead_band and obs_length
    group_by(ID) %>%
    filter(TIMESTAMP - min(TIMESTAMP) > dead_band) %>%
    group_by(ID) %>%
    filter(TIMESTAMP - min(TIMESTAMP) <= obs_length) %>%
    ungroup() %>%
    select(-dead_band, -obs_length, -start_timestamp,
           -end_timestamp, -start_times, -match) ->
    results[[i]]

} # for


# ---- Wrap up ----
message("Done with processing")
results <- bind_rows(results)
fn <- file.path(OUTPUT_DIR_ROOT, "tempest_tree_ghg_concentrations.csv")
message("Writing ", basename(fn))
write_csv(results, fn)

# CO2 slopes
results %>%
    mutate(Year = year(TIMESTAMP), Date = date(TIMESTAMP)) %>%
    group_by(Year, Date, plot, timepoint, ID) %>%
    mutate(secs = TIMESTAMP - min(TIMESTAMP)) %>%
    group_modify(~ broom::tidy(lm(CO2 ~ secs, data = .x))) %>%
    filter(term == "secs") %>%
    select(-term, -statistic,
           slope_CO2 = estimate,
           p.value_CO2 = p.value,
           std.error_CO2 = std.error) ->
    slopes_CO2

results %>%
    mutate(Year = year(TIMESTAMP), Date = date(TIMESTAMP)) %>%
    group_by(Year, Date, plot, timepoint, ID) %>%
    mutate(secs = TIMESTAMP - min(TIMESTAMP)) %>%
    group_modify(~ broom::tidy(lm(CH4 ~ secs, data = .x))) %>%
    filter(term == "secs") %>%
    select(-term, -statistic,
           slope_CH4 = estimate,
           p.value_CH4 = p.value,
           std.error_CH4 = std.error) ->
    slopes_CH4

slopes_CO2 %>%
    left_join(slopes_CH4, by = c("Year", "Date", "plot", "timepoint", "ID")) %>%
    group_by(Year, Date, plot, timepoint) %>%
    mutate(z_CO2 = (slope_CO2 - mean(slope_CO2)) / sd(slope_CO2),
           z_CH4 = (slope_CH4 - mean(slope_CH4)) / sd(slope_CH4),
           lab_CO2 = if_else(abs(z_CO2) > 1.96, ID, ""),
           lab_CH4 = if_else(abs(z_CH4) > 1.95, ID, "")) ->
    slopes

fn <- file.path(OUTPUT_DIR_ROOT, "tempest_tree_ghg_slopes.csv")
message("Writing ", basename(fn))
write_csv(slopes, fn)

for(yr in 2022:2024) {
    ggplot(filter(slopes, Year == yr), aes(1, slope_CO2, color = z_CO2)) +
        geom_jitter() +
        scale_color_distiller(type = "div") +
        geom_text(aes(label = lab_CO2), size = 2) +
        facet_grid(timepoint ~ plot) +
        coord_flip() +
        ggtitle(paste(yr, "slope_CO2"))
    fn <- file.path(OUTPUT_DIR_ROOT, paste0("tempest_CO2_slopes_", yr, ".pdf"))
    message("\tSaving ", basename(fn), "...")
    ggsave(fn, width = 10, height = 6)

    ggplot(filter(slopes, Year == yr), aes(1, slope_CH4, color = z_CH4)) +
        geom_jitter() +
        scale_color_distiller(type = "div") +
        geom_text(aes(label = lab_CH4), size = 2) +
        facet_grid(timepoint ~ plot) +
        coord_flip() +
        ggtitle(paste(yr, "slope_CH4"))
    fn <- file.path(OUTPUT_DIR_ROOT, paste0("tempest_CH4_slopes_", yr, ".pdf"))
    message("\tSaving ", basename(fn), "...")
    ggsave(fn, width = 10, height = 6)
}

stop("All done")
