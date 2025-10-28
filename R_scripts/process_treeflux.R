# process_treeflux.R
# Script to process raw Licor files into
# ready-for-analysis data matched with metadata
# KAM/BBL 2025

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
library(arrow)

now <- function() format(Sys.time(), "%a %b %d %X %Y")

message(now(), " Welcome to process_treeflux.R")

INPUT_DIR_ROOT <- "Data/tree_flux_licor/"
OUTPUT_DIR_ROOT <- "Data/tree_flux_licor/processing_outputs/"

# Get names of data files from TEMPEST I and II (2022, 2023, 2024)
files <- list.files(INPUT_DIR_ROOT, pattern = "\\.data$", full.names = TRUE, recursive = TRUE)
message("I see ", length(files), " data files")

# Helper function
# We use a cache (a list) to only read from disk when needed
if(!exists("cache")) cache <- list()
basefiles <- basename(files)
read_data_file <- function(base_f) {
    if(!base_f %in% basefiles) {
        stop("Hmm, I don't see a file named ", base_f, " in ", INPUT_DIR_ROOT)
    }

    f <- files[which(base_f == basefiles)]
    if(f %in% names(cache)) {
        message("\tGetting from cache: ", base_f)
    } else {
        message("\tReading: ", f)
        ffi_read_LI7810(f) %>%
            as_tibble() %>%
            mutate(File = basename(f)) %>%
            select(TIMESTAMP, TZ, CO2, CH4, SN, File) %>%
            # although the Licor's timezone settings vary, assume that
            # they are maintained EST. This is just a temporary
            # standardization; below we re-set the time zone based on
            # the entry in the treeflux-processing-info.csv file
            mutate(TIMESTAMP = force_tz(TIMESTAMP, tzone = "EST"),
                   TZ = "EST") ->>
            cache[[f]]
    }

    return(cache[[f]])
}

# Read in metadata and construct start/end timestamps
message("Reading metadata...")
meta22 <- read_csv(file.path(INPUT_DIR_ROOT, "metadata_excel_files/tree_flux_metadata22.csv"),
                   col_types = "ccccccddcc")
meta23 <- read_csv(file.path(INPUT_DIR_ROOT, "metadata_excel_files/tree_flux_metadata23.csv"),
                   col_types = "ccccccddcc")
meta24 <- read_csv(file.path(INPUT_DIR_ROOT, "metadata_excel_files/tree_flux_metadata24.csv"),
                   col_types = "ccccccccddddccc")
meta2125 <- read_csv(file.path(INPUT_DIR_ROOT, "metadata_excel_files/tree_flux_metadata21-25.csv"),
                     col_types = "ccccccdddd___dc", na = c("N/A", "n/a"))

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

# meta2125 has a different format; rework it to match others
meta2125 %>%
    select(plot = Plot, ID, collection_date = Date,
           start_time = `Start Time`, end_time = `End Time`,
           -`Tubing length (cm)`, dead_band, obs_length) %>%
    filter(!is.na(start_time)) %>%
    # change period to colons in the time columns and remove seconds
    mutate(timepoint = "(none)",
           start_time = gsub(".", ":", start_time, fixed = TRUE),
           start_time = gsub(":[0-9]{2}$", "", start_time),
           end_time = gsub(".", ":", end_time, fixed = TRUE),
           end_time = gsub(":[0-9]{2}$", "", end_time)) ->
    meta2125

meta22 %>%
    bind_rows(meta23, meta24, meta2125) %>%
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
                 col_types = "cDcccdcc")

results <- list()
#for(i in seq_len(nrow(tfpi))) {
        i <- 72

    I_STR <- sprintf("%02s", i)
    FILE <- tfpi$File[i]
    DATE <- tfpi$Date[i]
    TIMEPOINT <- tfpi$Timepoint[i]
    PLOT <- tfpi$Plot[i]
    MD_TZ <- tfpi$Metadata_tz[i]
    MD_TIME_ADD <- tfpi$Metadata_time_add[i]
    INS_TZ <- tfpi$Instrument_tz[i]
    NOTES <- tfpi$Notes[i]

    if(is.na(INS_TZ) || is.na(FILE)) {
        message("No entry for row ", i, "; skipping")
        next
    }
    message(now(), paste(" processing", I_STR, FILE, DATE, TIMEPOINT, PLOT))

    # Filter to one Licor file and one day for testing
    read_data_file(FILE) %>%
        filter(date(TIMESTAMP) == DATE) ->
        tree_data_filtered
    message("\t", nrow(tree_data_filtered), " rows of data")

    # ---- Change Licor data time zone, if needed ----
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

    stopifnot(nrow(md_filtered) > 0)

    # ---- Metadata time zone conversion, if needed ----
    if(MD_TZ != "EST") {
        message("\tConverting metadata times from ", MD_TZ, " to EST")
        md_filtered$start_timestamp <- force_tz(md_filtered$start_timestamp, tzone = MD_TZ)
        md_filtered$start_timestamp <- with_tz(md_filtered$start_timestamp, tzone = "EST")
        md_filtered$end_timestamp <- force_tz(md_filtered$end_timestamp, tzone = MD_TZ)
        md_filtered$end_timestamp <- with_tz(md_filtered$end_timestamp, tzone = "EST")
    }

    if(!is.na(MD_TIME_ADD) && MD_TIME_ADD != 0) {
        message("Adding ", MD_TIME_ADD, " to metadata start times")
        md_filtered$start_timestamp <- md_filtered$start_timestamp + MD_TIME_ADD * 60
        NOTES <- paste0(NOTES, " (+", MD_TIME_ADD, " min added to m.d. starts)")
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

    # Add ID information to the Licor data
    tree_data_filtered$ID <- md_filtered$ID[tree_data_filtered$match]
    tree_data_filtered$num_ID <- paste0(tree_data_filtered$match, " (", tree_data_filtered$ID, ")")
    tree_data_filtered$num_ID[is.na(tree_data_filtered$match)] <- NA

    # ---- Duplication check ----
    message("\tChecking for multiple observations per timestamp")
    matched_data <- tree_data_filtered %>% filter(!is.na(match))
    if(any(duplicated(matched_data$TIMESTAMP))) {
        # If the 7810's time zone settings gets changed during use,
        # the instrument writes multiple observations per timestamp
        # After investigation, it *seems* that the last observation
        # is the one we want to keep
        tree_data_filtered %>%
            group_by(TIMESTAMP) %>%
            mutate(obsrep = 1:n()) -> x
        message("\t", sum(x$obsrep > 1), " of ",
                nrow(tree_data_filtered), " timestamps with duplicate entries")
        # Keep only the last observation
        x %>%
            group_by(TIMESTAMP) %>%
            filter(obsrep == n()) %>%
            select(-obsrep) ->
            tree_data_filtered
        warning("De-duplicated data in ", i, ": multiple obs per timestamp!")
    }

    # ---- Diagnostic plot 1: color data by match ----
    p1 <- ggplot(tree_data_filtered, aes(x = TIMESTAMP, y = CO2, color = num_ID)) +
        geom_point(na.rm = TRUE) +
        ylim(300, 1000) +
        ggtitle(paste(I_STR, PLOT, DATE, TIMEPOINT, "matched"),
                subtitle = NOTES)
    print(p1)

    # Organize QAQC outputs by year and plot; change as you like
    SUBFOLDER <- file.path(year(DATE), PLOT)
    if(!dir.exists(file.path(OUTPUT_DIR_ROOT, SUBFOLDER))) {
        dir.create(file.path(OUTPUT_DIR_ROOT, SUBFOLDER), recursive = TRUE)
    }
    FN_ROOT <- paste(DATE, TIMEPOINT, PLOT, sep = "_")
    fn <- file.path(OUTPUT_DIR_ROOT, SUBFOLDER, paste0(FN_ROOT, "_match.pdf"))
    message("\tSaving ", basename(fn), "...")
    ggsave(fn, width = 10, height = 6)

    # Detail plot
    tree_data_filtered %>% filter(!is.na(match)) -> matches
    print(p1 + xlim(c(min(matches$TIMESTAMP), max(matches$TIMESTAMP))))
    fn <- file.path(OUTPUT_DIR_ROOT, SUBFOLDER, paste0(FN_ROOT, "_match_detail.pdf"))
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

    fn <- file.path(OUTPUT_DIR_ROOT, SUBFOLDER, paste0(FN_ROOT, "_fluxwindows.pdf"))
    message("\tSaving ", basename(fn), "...")
    ggsave(fn, width = 10, height = 6)

    # Merge data with metadata and save
    tree_data_filtered %>%
        filter(!is.na(match)) %>%
        left_join(md_filtered, by = "ID") %>%
        # Filter for dead_band and obs_length settings
        group_by(ID) %>%
        filter(TIMESTAMP - min(TIMESTAMP) > dead_band) %>%
        group_by(ID) %>%
        filter(TIMESTAMP - min(TIMESTAMP) <= obs_length) %>%
        ungroup() %>%
        select(-dead_band, -obs_length,
               -start_timestamp, -end_timestamp, -start_times,
               -match, -num_ID) ->
        results[[i]]

#} # for
stop("OK")

# ---- Wrap up ----
message("Done with processing")

message("Writing concentration data")
results <- bind_rows(results)
conc_fn <- file.path(OUTPUT_DIR_ROOT, "tempest_tree_ghg_concentrations.csv")
message("\tWriting ", basename(conc_fn))
write_csv(results, conc_fn)
conc_fn_pqt <- gsub("csv", "parquet", conc_fn)
message("\tWriting ", basename(conc_fn_pqt))
arrow::write_parquet(results, conc_fn_pqt)

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

# CH4 slopes
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
    arrange(Year, Date, plot, timepoint, ID) ->
    slopes

message("Writing slope data")
slopes_fn <- file.path(OUTPUT_DIR_ROOT, "tempest_tree_ghg_slopes.csv")
message("\tWriting ", basename(slopes_fn))
write_csv(slopes, slopes_fn)
slopes_fn_pqt <- gsub("csv", "parquet", slopes_fn)
message("\tWriting ", basename(slopes_fn_pqt))
arrow::write_parquet(results, slopes_fn_pqt)

message("Writing summary plots")
slopes %>%
    group_by(Year, Date, plot, timepoint) %>%
    # compute z-scores for
    mutate(z_CO2 = (slope_CO2 - mean(slope_CO2, na.rm = TRUE)) / sd(slope_CO2, na.rm = TRUE),
           z_CH4 = (slope_CH4 - mean(slope_CH4, na.rm = TRUE)) / sd(slope_CH4, na.rm = TRUE),
           lab_CO2 = if_else(abs(z_CO2) > 1.96, ID, ""),
           lab_CH4 = if_else(abs(z_CH4) > 1.95, ID, "")) ->
    slopes_plot

for(yr in unique(slopes_plot$Year)) {
    slopes_plot_yr <- filter(slopes_plot, Year == yr)
    ggplot(slopes_plot_yr, aes(1, slope_CO2, color = z_CO2)) +
        geom_jitter() +
        scale_color_distiller(type = "div") +
        geom_text(aes(label = lab_CO2), size = 2) +
        facet_grid(timepoint ~ plot) +
        coord_flip() +
        theme(axis.text.y = element_blank(), axis.title = element_blank()) +
        ggtitle(paste(yr, "slope_CO2"))
    fn <- file.path(OUTPUT_DIR_ROOT, paste0("tempest_CO2_slopes_", yr, ".pdf"))
    message("\tSaving ", basename(fn), "...")
    ggsave(fn, width = 10, height = 6)

    ggplot(slopes_plot_yr, aes(1, slope_CH4, color = z_CH4)) +
        geom_jitter() +
        scale_color_distiller(type = "div") +
        geom_text(aes(label = lab_CH4), size = 2) +
        facet_grid(timepoint ~ plot) +
        coord_flip() +
        theme(axis.text.y = element_blank(), axis.title = element_blank()) +
        ggtitle(paste(yr, "slope_CH4"))
    fn <- file.path(OUTPUT_DIR_ROOT, paste0("tempest_CH4_slopes_", yr, ".pdf"))
    message("\tSaving ", basename(fn), "...")
    ggsave(fn, width = 10, height = 6)
}

message(now(), " All done")

