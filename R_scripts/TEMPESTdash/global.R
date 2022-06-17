library(ggplot2)
theme_set(theme_minimal())
library(dplyr)
library(shiny)
library(DT)
library(readr)

source("../teros_fileread.R", local = TRUE)
source("../read_sapflow.R", local = TRUE)
source("../process_sapflow.R", local = TRUE)
source("../process_teros.R", local = TRUE)

TESTING <- FALSE

# TODO: back from present? Or back from latest timestamp in data?
GRAPH_TIME_WINDOW <- 3 * 24   # hours back from present
FLAG_TIME_WINDOW <- 1         # hours back from present

NO_DATA_GRAPH <- ggplot() +
    annotate("text", x = 1, y = 1, label = "(No data)", size = 12) +
    theme(axis.title = element_blank(),
          axis.text  = element_blank(),
    )

#source("../process_sapflow.R")

# sapflow <- read_csv("../../Data/sapflow/sapflow.csv")

# test <- reactiveFileReader(1000,
#                            session,
#                            filePath = "~/Desktop/test_reactive.csv",
#                            readFunc = read.csv)
