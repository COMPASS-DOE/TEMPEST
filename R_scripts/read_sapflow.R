# Function to read logger network sapflow data
# TEMPEST
# Stephanie Pennington | Created July 2021

# `filename` is a character path to a raw sapflow dataset from a Campbell data logger
# returns a dataframe

library(readr)
library(dplyr)

# Function setup
read_sapflow <- function(filename, token, total_files) {

    incProgress(1 / total_files)

    # download file to temp file
    drop_download(filename, local_path = "tempfile.dat",
                  dtoken = token,
                  overwrite = TRUE)

    sdat <- readLines("tempfile.dat") #temp file goes here
    sdat <- sdat[-3:-4] # remove lines 3 and 4 with unneeded information

    # parse line one to extract logger name
    pnnl_x <- gregexpr("PNNL_", sdat[1])[[1]][1]
    logger_name <- substr(sdat[1], start = pnnl_x, stop = pnnl_x + 6)

    read_csv(sdat, skip = 1) %>%
        mutate(Logger = logger_name)
}
