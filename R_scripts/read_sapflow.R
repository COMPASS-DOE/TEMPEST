# Function to read logger network sapflow data
# TEMPEST
# Stephanie Pennington | Created July 2021

# `filename` is a character path to a raw sapflow dataset from a Campbell data logger
# returns a dataframe

library(readr)
library(dplyr)
library(compasstools)

# Function setup
read_sapflow <- function(filename, token, total_files) {

    # If we're running in a Shiny session, update progress bar
    if(!is.null(getDefaultReactiveDomain())) {
        incProgress(1 / total_files)
    }

    # download file to temp file
    drop_download(filename, local_path = "tempfile.dat",
                  dtoken = token,
                  overwrite = TRUE)

    sdat <- compasstools::read_sapflow_file("tempfile.dat")
    unlink("tempfile.dat")
    sdat
}
