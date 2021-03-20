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
    filter(!is.na(value)) %>%
    # Pull data logger ID out of statname
    separate(Statname, into = c("Inst", "Data_Logger_ID"), sep = "_" ) %>%
    # Parse the data logger number, channel number, and variable number out of the
    # Statname and Channel columns
    mutate(Data_Logger_ID = as.integer(Data_Logger_ID, fixed = TRUE),
           TIMESTAMP = ymd_hms(TIMESTAMP)) %>%
    select(-Inst) %>%  # unneeded
    # Next, parse channel into the data logger channel and variable number
    separate(channel, into = c("Data_Table_ID", "variable"), sep = ",") %>%
    mutate(Data_Table_ID = as.integer(gsub("Teros(", "", Data_Table_ID, fixed = TRUE)),
           variable = as.integer(gsub(")", "", variable, fixed = TRUE)),
           # Give them sensible names
           variable = case_when(variable == 1 ~ "VWC",
                                variable == 2 ~ "TSOIL",
                                variable == 3 ~ "EC"))
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
  spread(variable, value) %>%
  as_tibble() ->
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
  left_join(map, by = c("Data_Logger_ID", "Data_Table_ID")) ->
  teros_data

# Applying calibration equation for mineral soil VWC
teros_data$VWC <- 3.879E-4 * (teros_data$VWC) - 0.6956

# Initial inspection of each environmental variable over time, data set will need some cleaning
message("Plotting...")
p_tsoil <- ggplot(teros_data, aes(TIMESTAMP, TSOIL, color = Plot)) +
  geom_line() +
  facet_grid(.~Plot)
print(p_tsoil)

p_vwc <- ggplot(teros_data, aes(TIMESTAMP, VWC, color = Plot)) +
  geom_line() +
  facet_grid(.~Plot)
print(p_vwc)

p_ec <- ggplot(teros_data, aes(TIMESTAMP, EC, color = Plot)) +
  geom_line() +
  facet_grid(.~Plot)
print(p_ec)

# Data QA/QC issues:
# Why are there 71 NAs for Plot?
# Handful of Control Plot sensors faulty in late summer 2020
# Several Freshwater Plot sensors faulty in late fall 2020

# BBL any idea why there are NAs for Plot? I could not figure it out. All the raw data files, mapping
# document, and code look good to me. For now, I am removing them from the data set.

teros_data %>%
  filter(!is.na(Plot)) ->
  teros_data2 # It looks like 71 rows with no Plot???

# Cleaning data set

# Outlier detection based on mean absolute deviation
# https://www.sciencedirect.com/science/article/abs/pii/S0022103113000668
# "By default, we suggest a threshold of 2.5 as a reasonable choice."
mad_outlier <- function(x, ndev = 2.5) {
  xmed <- median(x, na.rm = TRUE)
  xmad <- mad(x, na.rm = TRUE)
  xout <- abs(x - xmed) / xmad > ndev
  nexc <- sum(xout, na.rm = TRUE)
  message("Median = ", xmed, ", mad = ", xmad, ", ndev = ", ndev, ", exclude = ",
          nexc, " of ", length(xout), " (", round(nexc / length(xout) * 100), "%)")
  xout
}

message("TSOIL: ", appendLF = FALSE)
teros_data2$TSOIL[mad_outlier(teros_data2$TSOIL)] <- NA_real_
message("VWC: ", appendLF = FALSE)
teros_data2$VWC[mad_outlier(teros_data2$VWC)] <- NA_real_
message("EC: ", appendLF = FALSE)
teros_data2$EC[mad_outlier(teros_data2$EC)] <- NA_real_

# Calculating daily averages - or do we want to keep the 15-minute data, BBL?

teros_data2 %>%
  mutate(Date = as.Date(TIMESTAMP)) %>%
  group_by(Date, Plot, Data_Logger_ID, Data_Table_ID, Grid_Square, ID, Depth) %>%
  summarise(n = n(),
            meanTSOIL = mean(TSOIL),
            meanVWC = mean(VWC),
            meanEC = mean(EC)) ->
  daily_dat

p_tsoil <- ggplot(daily_dat, aes(Date, meanTSOIL, color = Plot, group=ID)) +
  geom_point() +
  ylab("Average Daily Soil Temperature (?C)") +
  xlab("Date") +
  scale_color_manual(values=c("green", "blue", "red")) +
  facet_wrap(.~Plot)
print(p_tsoil)

p_vwc <- ggplot(daily_dat, aes(Date, meanVWC, color = Plot, group=ID)) +
  geom_point() +
  ylab("Average Daily Volumetric Water Content") +
  xlab("Date") +
  scale_color_manual(values=c("green", "blue", "red")) +
  facet_wrap(.~Plot)
print(p_vwc)

p_ec <- ggplot(daily_dat, aes(Date, meanEC, color = Plot, group=ID)) +
  geom_point() +
  ylab("Average Daily Electrical Conductivity (?S/cm)") +
  xlab("Date") +
  scale_color_manual(values=c("green", "blue", "red")) +
  facet_wrap(.~Plot)
print(p_ec)

# Looking at data post-February 2021 network maintenance
daily_dat %>%
  filter(Date >= as.Date("2021-02-26 11:52:30")) %>%
  mutate(Depth = factor(Depth, levels=c("5", "15", "30"), labels=c("5 cm", "15 cm", "30 cm"))) ->
  daily_2021

p_tsoil <- ggplot(daily_2021, aes(Date, meanTSOIL, color = Plot, group=ID)) +
  geom_line(size=1.5) +
  ylab("Average Daily Soil Temperature (?C)") +
  xlab("Date") +
  scale_color_manual(values=c("green", "blue", "red")) +
  facet_wrap(.~Plot)
print(p_tsoil)

p_vwc <- ggplot(daily_2021, aes(Date, meanVWC, color = Plot, group=ID)) +
  geom_line(size=1.5) +
  ylab("Average Daily Volumetric Water Content") +
  xlab("Date") +
  scale_color_manual(values=c("green", "blue", "red")) +
  facet_wrap(.~Plot)
print(p_vwc)

p_ec <- ggplot(daily_2021, aes(Date, meanEC, color = Plot, group=ID)) +
  geom_line(size=1.5) +
  ylab("Average Daily Electrical Conductivity (?S/cm)") +
  xlab("Date") +
  scale_color_manual(values=c("green", "blue", "red")) +
  facet_wrap(.~Plot)
print(p_ec)
