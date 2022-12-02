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
EVENT_START <- as_datetime("2022-12-02 06:30:00", tz = "EST")
EVENT_STOP <- as_datetime("2022-12-02 16:30:00", tz = "EST")
EVENT_HOURS <- as.numeric(difftime(EVENT_STOP, EVENT_START, units = "hours"))

NO_DATA_GRAPH <- ggplot() +
    annotate("text", x = 1, y = 1, label = "(No data)", size = 12) +
    theme(axis.title = element_blank(),
          axis.text  = element_blank(),
    )

SAPFLOW_EVENT_RECT <- geom_rect(aes(xmin = EVENT_START, xmax = EVENT_STOP,
                                    ymin = min(SAPFLOW_RANGE), ymax = max(SAPFLOW_RANGE)),
                                fill = "#BBE7E6",
                                alpha = 0.7,
                                col = "#BBE7E6")
