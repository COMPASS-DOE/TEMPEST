
library(dplyr)
library(shiny)
library(DT)
library(readr)

source("../teros_fileread.R", local = TRUE)
source("../read_sapflow.R", local = TRUE)
source("../process_sapflow.R", local = TRUE)
source("../process_teros.R", local = TRUE)



#source("../process_sapflow.R")

# sapflow <- read_csv("../../Data/sapflow/sapflow.csv")

# test <- reactiveFileReader(1000,
#                            session,
#                            filePath = "~/Desktop/test_reactive.csv",
#                            readFunc = read.csv)
