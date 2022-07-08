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

source("../teros_fileread.R", local = TRUE)
source("../read_sapflow.R", local = TRUE)
source("../process_sapflow.R", local = TRUE)
source("../process_teros.R", local = TRUE)
source("../process_aquatroll.R", local = TRUE)

TESTING <- FALSE

GRAPH_TIME_WINDOW <- 3 * 24   # hours back from present
GRAPH_TIME_INTERVAL <- "15 minutes"  # used by round_date in graphs
FLAG_TIME_WINDOW <- 1         # hours back from present
EVENT_START <- as_datetime("2022-06-22 06:30:00", tz = "EST")
EVENT_STOP <- as_datetime("2022-06-22 16:30:00", tz = "EST")
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
