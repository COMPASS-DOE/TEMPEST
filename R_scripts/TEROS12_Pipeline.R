# TEMPEST TEROS NETWORK DATA PROCESSING PIPELINE
# Anya Hopple - 2021-03-04

# Parse loggernet data streams into data tables for researcher use on
# the TEMPEST Project. Functions are ordered and annotated for routine implementation.

library(lubridate)
library(tidyr)
library(dplyr)
library(ggplot2)
theme_set(theme_bw())

# Helper function: read string vector into a data frame, reshape, and drop NA
fileread <- function(fn) {
    message("Reading ", basename(fn), "...")
    rawdata <- readLines(fn)[-c(1, 3, 4)]
    textConnection(rawdata) %>%
        read.csv(check.names = FALSE, na.strings = "NAN", stringsAsFactors = FALSE) %>%
        gather(channel, value, -TIMESTAMP, -RECORD, -Statname) %>%
        filter(!is.na(value))
}

# Create a list of all TEROS Network data files; recursive = TRUE includes sub-folders
rawdata_dir <- "../Data/TEROS12/teros12_data/Raw_Data/"
message("Raw data dir: ", rawdata_dir)
files <- list.files(path = rawdata_dir,
                    pattern = "^TEROS[0-9]{2}_[0-9]{8}.txt$", all.files = FALSE,
                    full.names = TRUE, recursive = TRUE, ignore.case = FALSE)
message(length(files), " files to parse")
stopifnot(length(files) > 0)  # error if no files found

# Lines 1, 3, and 4 of the TEROS data files contain sensor metadata that we want to remove
# Read the data files into a string vector, remove those lines, and then pass to read.csv()
# Finally we set the TIMESTAMP field and reshape the combined data frame to one observation per row
files %>%
    lapply(fileread) %>%
    bind_rows() %>%
    distinct() %>%
    mutate(TIMESTAMP = ymd_hms(TIMESTAMP)) %>%
    as_tibble() ->
    teros_data

# Parse the data logger number, channel number, and variable number out of the
# Statname and Channel columns
message("Parsing data...")
teros_data %>%
    # Pull data logger ID out of statname
    separate(Statname, into = c("Inst", "Data_Logger_ID"), sep = "_" ) %>%
    mutate(Data_Logger_ID = as.integer(Data_Logger_ID, fixed = TRUE)) %>%
    # Next, parse channel into the data logger channel and variable number
    separate(channel, into = c("Data_Table_ID", "variable"), sep = ",") %>%
    mutate(Data_Table_ID = as.integer(gsub("Teros(", "", Data_Table_ID, fixed = TRUE)),
           variable = as.integer(gsub(")", "", variable, fixed = TRUE))) %>%
    # Give them sensible names
    mutate(variable = case_when(variable == 1 ~ "VWC",
                                variable == 2 ~ "TSOIL",
                                variable == 3 ~ "EC")) ->
    teros_data

# Read mapping file that includes location and sensor ID info
message("Reading map file and merging...")
read.csv(file.path(rawdata_dir, "TEMPEST_TEROS_Network_Location&ID.csv"),
         stringsAsFactors = FALSE) %>%
    select(Plot, Grid_Square, ID, Depth, Data_Logger_ID, Data_Table_ID) ->
    map

# Defensive programming: should be exactly three variables
stopifnot(length(unique(teros_data$variable)) == 3)

# Merge the two data frames, pulling plot, grid square, ID, and depth info into teros_data
# Reshape to put each variable into its own column
teros_data %>%
    left_join(map, by = c("Data_Logger_ID", "Data_Table_ID")) %>%
    spread(variable, value) ->
    teros_data

# Initial inspection of each environmental variable over time, data set will need some cleaning
message("Plotting...")
p_tsoil <- ggplot(teros_data, aes(TIMESTAMP, TSOIL, color = Plot, group = ID)) + geom_line()
print(p_tsoil)

p_vwc <- ggplot(teros_data, aes(TIMESTAMP, VWC, color = Plot, group = ID)) + geom_line() +
    # There are some crazy values
    coord_cartesian(ylim = c(0, 3000)) +
    facet_grid(Plot~.)
print(p_vwc)

p_ec <- ggplot(teros_data, aes(TIMESTAMP, VWC, color = Plot)) + geom_line()
print(p_ec)

message("All done.")
