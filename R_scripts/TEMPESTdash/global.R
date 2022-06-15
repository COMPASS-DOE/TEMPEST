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


GRAPH_TIME_WINDOW <- 3 * 24   # hours back from present


#source("../process_sapflow.R")

# sapflow <- read_csv("../../Data/sapflow/sapflow.csv")

# test <- reactiveFileReader(1000,
#                            session,
#                            filePath = "~/Desktop/test_reactive.csv",
#                            readFunc = read.csv)
