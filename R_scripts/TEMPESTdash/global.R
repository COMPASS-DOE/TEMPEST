#
# These are global settings for the TEMPEST data dashboard
# June 2022

library(ggplot2)
theme_set(theme_minimal())
library(dplyr)
library(shiny)
library(DT)
library(readr)
library(lubridate)
library(rdrop2)
library(dygraphs)
library(xts)
library(shinybusy)

source("process_sapflow.R")
source("process_teros.R")
source("process_aquatroll.R")

TESTING <- FALSE


# The server normally accesses the SERC Dropbox to download data
# If we are TESTING, however, skip this and use local test data only
if(!TESTING) {
    datadir <- "TEMPEST_PNNL_Data/Current_Data"
    token <- readRDS("droptoken.rds")
    cursor <- drop_dir(datadir, cursor = TRUE, dtoken = token)
}
last_update <- NA

GRAPH_TIME_WINDOW <- 3 * 24   # hours back from present
GRAPH_TIME_INTERVAL <- "15 minutes"  # used by round_date in graphs
FLAG_TIME_WINDOW <- 1         # hours back from present

NO_DATA_GRAPH <- ggplot() +
    annotate("text", x = 1, y = 1, label = "(No data)", size = 12) +
    theme(axis.title = element_blank(),
          axis.text  = element_blank(),
    )

