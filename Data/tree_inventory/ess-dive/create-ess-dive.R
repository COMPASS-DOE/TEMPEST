# create-ess-dive.R
# Create tree inventory data and metadata files for ESS-DIVE
# BBL 2025-09-04

if(basename(getwd()) != "ess-dive") {
    stop("Set working directory to ess-dive directory before running")
}


message("Copying data and species codes")
file.copy("../species-genera.csv", ".")
library(readr)
x <- read_csv("../inventory.csv")
# Change the plot codes to match those used by sensor data, etc.
codes <- c("Control" = "C", "Freshwater" = "F", "Saltwater" = "S")
if(!all(x$Plot %in% names(codes))) {
    stop("Wait, there are unknown plot names!")
}
x$Plot <- codes[x$Plot]
x$JS_codes <- NULL
write_csv(x, "inventory-wide.csv", na = "")

message("Checking inventory-wide_dd.csv")
dd <- read_csv("inventory-wide_dd.csv")
if(!identical(sort(dd$Column_or_Row_Name), sort(names(x)))) {
    stop("Column names and data dictionary entries are not identical!")
}

message("Creating inventory-long.csv")
library(dplyr)
library(tidyr)

pivot_and_split <- function(df, col_prefix) {
    stopifnot(any(grepl(col_prefix, colnames(df)))) # not present
    newcol <- gsub("_", "", col_prefix)
    df %>%
        select(Plot, Section, Tag, Grid, Species_code, In_Plot, Notes, starts_with(col_prefix)) %>%
        pivot_longer(cols = starts_with(col_prefix), values_to = newcol) %>%
        separate(name, into = c("x", "Year")) %>%
        select(-x)
}

inv_long_dbh <- pivot_and_split(x, "DBH_")
inv_long_date <- pivot_and_split(x, "Date_")
inv_long_status <- pivot_and_split(x, "Status_")
joincols <- c("Plot", "Section", "Tag", "Grid", "Species_code", "In_Plot", "Notes", "Year")
inv_long_dbh %>%
    left_join(inv_long_date, by = joincols) %>%
    left_join(inv_long_status, by = joincols) %>%
    # clean up
    mutate(Year = as.integer(Year)) %>%
    select(Plot, Section, Tag, Grid, Species_code, In_Plot, Year, Date, DBH, Status, Notes) %>%
    arrange(Plot, Tag, Year) ->
    inv_long

write_csv(inv_long, "inventory-long.csv", na = "")

message("Checking inventory-long_dd.csv")
dd <- read_csv("inventory-long_dd.csv")
if(!identical(sort(dd$Column_or_Row_Name), sort(names(inv_long)))) {
    stop("Column names and data dictionary entries are not identical!")
}

message("Checking flmd.csv")
flmd <- read_csv("flmd.csv")
if(!all(file.exists(flmd$File_Name))) {
    stop("One or more of the flmd files doesn't exist!")
}

message("All done!")
