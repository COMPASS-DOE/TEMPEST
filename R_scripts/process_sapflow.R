# Script to read raw sapflow data and process
# Produces a sapflow data frame produce and writes to CSV

library(readr)
library(lubridate)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(kableExtra)
set.seed(7)

process_sapflow <- function(token) {

    # Generate list of 'current' sapflow files
    s_dir <- drop_dir("TEMPEST_PNNL_Data/Current_Data/", dtoken = token)
    s_files <- grep(s_dir$path_display, pattern = "sapflow\\.dat$", value = TRUE)

    sf_inventory <- read_csv("sapflow_inventory copy.csv", col_types = "cdcdddcD")

    lapply(s_files, read_sapflow, token, length(s_files)) %>%
        bind_rows()  -> sf_primitive

    sf_primitive %>%
        distinct() %>%
        # extract number form former col name "DiffVolt_Avg(1)" would become "1"
        # join with ports dataframe and bring in tree codes
        pivot_longer(cols = starts_with("DiffVolt_Avg"),
                     names_to = "Port", values_to = "Value") %>%
        rename(Timestamp = TIMESTAMP,
               Record = RECORD) %>%
        mutate(Timestamp = ymd_hms(Timestamp, tz = "EST"),
               Port = parse_number(Port),
               Logger = parse_number(Logger)) -> sf_raw

    sf_raw %>%
        left_join(sf_inventory, by = c("Logger", "Port")) %>%
        filter(!is.na(Tree_Code)) %>% #remove ports that dont have any sensors
        select(Timestamp, Record, BattV_Avg, Port, Value, Logger, Tree_Code, Grid_Square, Species, Installation_Date) %>%
        mutate(Deep_Sensor = grepl("D", Tree_Code),
               Grid_Letter = substring(Grid_Square, 1, 1),
               Grid_Number = substring(Grid_Square, 2, 2)) -> sapflow

    nomatch_ports <- anti_join(sf_raw, sf_inventory, by = c("Logger", "Port"))

    if(nrow(nomatch_ports) > 0) {
        warning("There were logger/port combinations that I couldn't find in sapflow_inventory.csv:")
        nomatch_ports %>%
            distinct(Logger, Port) %>%
            kable()
    }

    # Add some extra time information
    sapflow %>%
        mutate(Date = as.Date(Timestamp),
               Hour = hour(Timestamp) + minute(Timestamp) / 60,
               Plot = substr(Tree_Code, 1, 1))

    #write_csv(sapflow, "~/Documents/GitHub/COMPASS-DOE/TEMPEST/Data/sapflow/sapflow.csv")

}
