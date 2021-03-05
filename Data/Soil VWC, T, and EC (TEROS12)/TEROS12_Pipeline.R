#TEMPEST TEROS NETWORK DATA PROCESSING PIPELINE
#Anya Hopple - 03/04/2021

#The following codes are functions and procedures for parsing loggernet data streams into data tables for researcher use on
#the TEMPEST Project. Functions are ordered and annotated for routine implementation.

library(lubridate)
library(tidyr)
library(dplyr)
library(ggplot2)

setwd("~/PNNL PostDoc/GitHub/TEMPEST/Data/Soil VWC, T, and EC (TEROS12)/teros12_data/Raw_Data")

#Creating a list of all TEROS Network data files; recursive = TRUE includes sub-folders
files <- list.files(pattern = NULL, all.files = FALSE,
                full.names = TRUE, recursive = TRUE, ignore.case = FALSE)

#Lines 1, 3, and 4 of the TEROS data files contain sensor metadata that we want to remove
#This function reads the data files into a string vector and then removes those lines
x1<-sapply(seq(1,length(files),1), function(i) readLines(files[i])[-c(1,3,4)])

#Removing data file 4 since it has a non-standard format
x2<-x1[c(1:3,5:62)]

#This function reads each string vector from list x2 as a .csv file and then combines all
#rows in a single data frame
x3<-bind_rows(lapply(seq(1,length(x2),1), function(i) read.csv(textConnection(x2[[i]]),check.names = FALSE, na.strings = "NAN")))

#Setting time stamp
x3$TIMESTAMP <- ymd_hms(x3$TIMESTAMP)

#Converting to 'long' (tidy) form - one observation per row. "-COLUMN NAME" keeps as column
x3_long <- gather(x3, channel, value, -TIMESTAMP, -RECORD, -Statname)

#Parsing the data logger number, channel number, and variable number out of the
#Statname and Channel columns
x3_long %>%
    #Pull data logger ID out of statname
    separate(Statname, into = c("Inst", "Data_Logger_ID"), sep = "_" ) %>%
    mutate(Data_Logger_ID = as.integer(Data_Logger_ID, fixed = TRUE)) %>%
    #Next, parse channel into the data logger channel and variable number
    separate(channel, into = c("Data_Table_ID", "variable"), sep = ",") %>%
    mutate(Data_Table_ID = as.integer(gsub("Teros(", "", Data_Table_ID, fixed = TRUE)),
           variable = as.integer(gsub(")", "", variable, fixed = TRUE))) %>%
    #Give them sensible names
    mutate(variable = case_when(variable == 1 ~ "VWC",
                                variable == 2 ~ "TSOIL",
                                variable == 3 ~ "EC")) ->
    x3_long

#Merging data frame with a mapping files that includes location and sensor ID info

#Reading in mapping file
map <- read.csv("TEMPEST_TEROS_Network_Location&ID.csv")
map <- select(map, Plot, Grid_Square, ID, Depth, Data_Logger_ID, Data_Table_ID)

#Defensive programming: should be exactly three variables
stopifnot(length(unique(x3_long$variable)) == 3)

#Merge the two data frames, pulling plot, grid square, ID, and depth info into teros_long
x3_long %>%
    left_join(map, by = c("Data_Logger_ID", "Data_Table_ID")) ->
    x3_long

#BBL - wanted to do this here but could not get it to work...thoughts?
#Reshape to put each variable into its own column
x3_long %>%
    spread(variable, value) ->
    x3_long

#Removing NAN rows - pulling these out because there are many un-used Data Table channels that make the file much larger
x4_long <- na.omit(x3_long)


#Initial inspection of each environmental variable over time, data set will need some cleaning
p <- ggplot(x4_long[x4_long$variable == "TSOIL",], aes(TIMESTAMP,value, color=Plot.x)) + geom_line()
print(p)

a <- ggplot(x4_long[x4_long$variable == "VWC",], aes(TIMESTAMP,value, color=Plot.x)) + geom_line()
print(a)

b <- ggplot(x4_long[x4_long$variable == "EC",], aes(TIMESTAMP,value, color=Plot.x)) + geom_line()
print(b)
