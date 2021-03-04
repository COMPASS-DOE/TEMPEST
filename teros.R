
fn <- "./Data/Soil VWC, T, and EC (TEROS12)/teros12_data/Raw_Data/PNNL11/TEROS11_11192020.txt"
teros_raw <- readLines(fn)
teros <- teros_raw[-c(1, 3, 4)]
teros_dat <- read.csv(textConnection(teros), check.names = FALSE, na.strings = "NAN")

library(lubridate)
teros_dat$TIMESTAMP <- ymd_hms(teros_dat$TIMESTAMP)
library(tidyr)
teros_long <- gather(teros_dat, channel, value, -TIMESTAMP, -RECORD, -Statname)

# The filename encodes critical information, so add it
teros_long$filename <- basename(fn)
teros_long <- as_tibble(teros_long)

library(dplyr)

# At this point we want to parse the datalogger number, channel number, and variable number
# out of the filename and channel columns
teros_long %>%
    # First, pull the data logger ID out of the filename
    separate(filename, into = "Data_Logger_ID", extra = "drop", sep = "_") %>%
    mutate(Data_Logger_ID = as.integer(gsub("TEROS", "", Data_Logger_ID))) %>%
    # Next, parse channel into the datalogger channel and variable number
    separate(channel, into = c("Data_Table_ID", "variable"), sep = ",") %>%
    mutate(Data_Table_ID = as.integer(gsub("Teros(", "", Data_Table_ID, fixed = TRUE)),
           variable = as.integer(gsub(")", "", variable, fixed = TRUE))) ->
    teros_long

# Now let's go get that mapping file!
map <- read.csv("Data/Soil VWC, T, and EC (TEROS12)/teros12_data/TEMPEST_TEROS_Network_Location&ID.csv")
map <- select(map, Plot, Grid_Square, ID, Depth, Data_Logger_ID, Data_Table_ID)

# Defensive programming
stopifnot(length(unique(teros_long$variable)) == 3)

# Merge the two
teros_long %>%
    left_join(map, by = c("Data_Logger_ID", "Data_Table_ID")) %>%
    mutate(variable = case_when(variable == 1 ~ "VWC",
                                variable == 2 ~ "TSOIL",
                                variable == 3 ~ "EC")) %>%
    # Reshape to put each variable into its own column
    spread(variable, value) ->
    teros_long
