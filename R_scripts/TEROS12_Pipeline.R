# TEMPEST TEROS NETWORK DATA PROCESSING PIPELINE
# Anya Hopple - 2021-03-04

# Parse loggernet data streams into data tables for researcher use on
# the TEMPEST Project. Functions are ordered and annotated for routine implementation.

library(lubridate)
library(tidyr)
library(dplyr)
library(ggplot2)
theme_set(theme_bw())

# Source TEROS fileread() function to read in data
source("teros_fileread.R")

# Create a list of all TEROS Network data files; recursive = TRUE includes sub-folders
rawdata_dir <- "../Data/TEROS12/teros12_data/Raw_Data/"
message("Raw data dir: ", rawdata_dir)
files <- list.files(path = rawdata_dir,
                    pattern = "^TEROS[0-9]{2}_[0-9]{8}.txt$", all.files = FALSE,
                    full.names = TRUE, recursive = TRUE, ignore.case = FALSE)
message(length(files), " files to parse")
stopifnot(length(files) > 0)  # error if no files found

# Process all files through fileread() (above), combine, remove duplicates...
files %>%
  lapply(fileread) %>%
  bind_rows() %>%
  distinct() ->
  teros_data

# Defensive programming: should be exactly three variables
stopifnot(length(unique(teros_data$variable)) == 3)

# Read mapping file that includes location and sensor ID info
message("Reading map file and merging...")
read.csv(file.path(rawdata_dir, "TEMPEST_TEROS_Network_Location&ID.csv"),
         stringsAsFactors = FALSE) %>%
  select(Plot, Grid_Square, ID, Depth, Data_Logger_ID, Data_Table_ID) ->
  map

# Reshape (reducing data set size), and then merge the two
# data frames, pulling plot, grid square, ID, and depth info into teros_data
# Reshape to put each variable into its own column
teros_data %>%
  spread(variable, value) %>%
  left_join(map, by = c("Data_Logger_ID", "Data_Table_ID")) ->
  teros_data

# Applying calibration equation for mineral soil VWC
teros_data$VWC <- 3.879E-4 * (teros_data$VWC) - 0.6956

# Initial inspection of each environmental variable over time, data set will need some cleaning
message("Plotting...")
p_tsoil <- ggplot(teros_data, aes(TIMESTAMP, TSOIL, group = Data_Table_ID, color = Plot)) +
  geom_line() +
  facet_wrap(~Data_Logger_ID)
print(p_tsoil)

p_vwc <- ggplot(teros_data, aes(TIMESTAMP, VWC, group = Data_Table_ID, color = Plot)) +
  geom_line() +
  facet_wrap(~Data_Logger_ID)
print(p_vwc)

p_ec <- ggplot(teros_data, aes(TIMESTAMP, EC, group = Data_Table_ID, color = Plot)) +
  geom_line() +
  facet_wrap(~Data_Logger_ID)
print(p_ec)

# Data QA/QC issues:
# Why are there 71 NAs for Plot?
# Handful of Control Plot sensors faulty in late summer 2020
# Several Freshwater Plot sensors faulty in late fall 2020

# BBL any idea why there are NAs for Plot? I could not figure it out. All the raw data files, mapping
# document, and code look good to me. For now, I am removing them from the data set.

if(any(is.na(teros_data$Plot))) {
  warning("There are NA plot values in TEROS data. This should not happen!")
  teros_data %>%
    filter(!is.na(Plot)) ->
    teros_data # It looks like 71 rows with no Plot???
}

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
tsoil_outliers <- mad_outlier(teros_data$TSOIL)
teros_data$TSOIL[tsoil_outliers] <- NA_real_
message("VWC: ", appendLF = FALSE)
vwc_outliers <- mad_outlier(teros_data$VWC)
teros_data$VWC[vwc_outliers] <- NA_real_
message("EC: ", appendLF = FALSE)
ec_outliers <- mad_outlier(teros_data$EC)
teros_data$EC[ec_outliers] <- NA_real_

# Outlier report
teros_data %>%
  mutate(Date = round_date(TIMESTAMP, unit = "month"),
         TSOIL = tsoil_outliers,
         VWC = vwc_outliers,
         EC = ec_outliers) %>%
  select(Date, Data_Logger_ID, Data_Table_ID, TSOIL, VWC, EC) %>%
  group_by(Date, Data_Logger_ID, Data_Table_ID) %>%
  summarise(TSOIL = sum(TSOIL) / n(),
            VWC = sum(VWC) / n(),
            EC = sum(EC) / n()) ->
  outlier_summary

outlier_summary %>%
  pivot_longer(c(TSOIL, VWC, EC), names_to = "Group", values_to = "Outliers") %>%
  ggplot(aes(Group, Date, fill = Outliers)) + geom_tile() +
  scale_fill_continuous(labels = scales::percent_format()) ->
  p_outliers
print(p_outliers)

# Calculating daily averages - or do we want to keep the 15-minute data, BBL?

teros_data %>%
  mutate(Date = as.Date(TIMESTAMP)) %>%
  group_by(Date, Plot, Data_Logger_ID, Data_Table_ID, Grid_Square, ID, Depth) %>%
  summarise(n = n(),
            meanTSOIL = mean(TSOIL, na.rm = TRUE),
            meanVWC = mean(VWC, na.rm = TRUE),
            meanEC = mean(EC, na.rm = TRUE)) ->
  daily_dat

p_tsoil <- ggplot(daily_dat, aes(Date, meanTSOIL, color = Plot, group=ID)) +
  geom_point(na.rm = TRUE) +
  ylab("Average Daily Soil Temperature (?C)") +
  xlab("Date") +
  scale_color_manual(values = c("green", "blue", "red")) +
  facet_wrap(.~Plot)
print(p_tsoil)

p_vwc <- ggplot(daily_dat, aes(Date, meanVWC, color = Plot, group=ID)) +
  geom_point(na.rm = TRUE) +
  ylab("Average Daily Volumetric Water Content") +
  xlab("Date") +
  scale_color_manual(values = c("green", "blue", "red")) +
  facet_wrap(.~Plot)
print(p_vwc)

p_ec <- ggplot(daily_dat, aes(Date, meanEC, color = Plot, group=ID)) +
  geom_point(na.rm = TRUE) +
  ylab("Average Daily Electrical Conductivity (?S/cm)") +
  xlab("Date") +
  scale_color_manual(values = c("green", "blue", "red")) +
  facet_wrap(.~Plot)
print(p_ec)

# Looking at data post-February 2021 network maintenance
daily_dat %>%
  filter(Date >= as.Date("2021-02-26 11:52:30")) %>%
  mutate(Depth = factor(Depth, levels = c("5", "15", "30"),
                        labels = c("5 cm", "15 cm", "30 cm"))) ->
  daily_2021

p_tsoil <- ggplot(daily_2021, aes(Date, meanTSOIL, color = Plot, group = ID)) +
  geom_line(size = 1.5) +
  ylab("Average Daily Soil Temperature (?C)") +
  scale_color_manual(values = c("green", "blue", "red")) +
  facet_wrap(.~Plot)
print(p_tsoil)

p_vwc <- ggplot(daily_2021, aes(Date, meanVWC, color = Plot, group = ID)) +
  geom_line(size = 1.5) +
  ylab("Average Daily Volumetric Water Content") +
  scale_color_manual(values = c("green", "blue", "red")) +
  facet_wrap(.~Plot)
print(p_vwc)

p_ec <- ggplot(daily_2021, aes(Date, meanEC, color = Plot, group = ID)) +
  geom_line(size = 1.5) +
  ylab("Average Daily Electrical Conductivity (?S/cm)") +
  scale_color_manual(values = c("green", "blue", "red")) +
  facet_wrap(.~Plot)
print(p_ec)
