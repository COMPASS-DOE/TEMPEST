# Read and plot TEROS 12 soil VWC, temperature, and salinity data from TEMPEST experiment
# AMH February 2021

setwd("~/PNNL PostDoc/GitHub/TEMPEST/Data/Soil Temperature, Water Content, and Electrical Conductivity (TEROS 12)/teros12_data")

library(readr)
library(dplyr)
library(ggplot2)
theme_set(theme_bw())
library(lubridate)
library(tidyr)
library(scales)

#PNNL11 - Northwestern third of Control Plot; 14 sensors; could not open first data file - too large 5.4MB

files <- list.files("PNNL11/", pattern = "*.txt", full.names = TRUE)

dat <- bind_rows(lapply(files, read_tsv, col_names = c("TIMESTAMP",	"RECORD",	"Statname",	"Teros(1,1)",	"Teros(1,2)",
                                                       "Teros(1,3)",	"Teros(2,1)",	"Teros(2,2)",	"Teros(2,3)",	"Teros(3,1)",
                                                       "Teros(3,2)",	"Teros(3,3)",	"Teros(4,1)",	"Teros(4,2)",	"Teros(4,3)",
                                                       "Teros(5,1)",	"Teros(5,2)",	"Teros(5,3)",	"Teros(6,1)",	"Teros(6,2)",
                                                       "Teros(6,3)",	"Teros(7,1)",	"Teros(7,2)",	"Teros(7,3)",	"Teros(8,1)",	
                                                       "Teros(8,2)",	"Teros(8,3)",	"Teros(9,1)",	"Teros(9,2)",	"Teros(9,3)",
                                                       "Teros(10,1)",	"Teros(10,2)", "Teros(10,3)",	"Teros(11,1)",	"Teros(11,2)",
                                                       "Teros(11,3)",	"Teros(12,1)",	"Teros(12,2)",	"Teros(12,3)",	"Teros(13,1)",
                                                       "Teros(13,2)",	"Teros(13,3)",	"Teros(14,1)",	"Teros(14,2)",	"Teros(14,3)",
                                                       "Teros(15,1)",	"Teros(15,2)",	"Teros(15,3)",	"Teros(16,1)",	"Teros(16,2)",	
                                                       "Teros(16,3)",	"Teros(17,1)",	"Teros(17,2)",	"Teros(17,3)",	"Teros(18,1)",
                                                       "Teros(18,2)",	"Teros(18,3)",	"Teros(19,1)",	"Teros(19,2)",	"Teros(19,3)",
                                                       "Teros(20,1)",	"Teros(20,2)",	"Teros(20,3)",	"Teros(21,1)",	"Teros(21,2)",
                                                       "Teros(21,3)",	"Teros(22,1)",	"Teros(22,2)",	"Teros(22,3)"), skip = 1))

dat$TIMESTAMP <- as.POSIXct(strptime(dat$TIMESTAMP, format = '%m/%d/%Y %H:%M'))

Time <- dat[,c(1:3)];head(Time)

VWC_raw <- dat[,seq(4,69,3)];head(VWC_raw)
VWC_eq <- 3.879*10^-4*VWC_raw-0.6956
VWC <- cbind(Time,VWC_eq);head(VWC)

Temp <- dat[,c(1:3,seq(5,69,3))];head(Temp)

EC <- dat[,c(1:3,seq(6,69,3))];head(EC)

#Volumetric Water Content
ggplot() + 
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(1,1)`)) + #no gaps; T135
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(2,1)`)) + #gap from late October-present; T100
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(3,1)`)) + #gap from October-February; T132
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(4,1)`)) + #gap from late October-February; T160
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(5,1)`)) + #gap from late November-present; T133
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(6,1)`)) + #gap from late December-present;T019
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(7,1)`)) + #no data until February; T056
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(8,1)`)) + #no data until February;T129
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(9,1)`)) + #gap from mid-October-February; T058
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(10,1)`)) + #no data; T059
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(11,1)`)) + #no data until February; T060
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(12,1)`)) + #NO SENSOR
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(13,1)`)) + #NO SENSOR
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(14,1)`), color="blue") + #no data until February; 5cm; T138
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(15,1)`)) + #gap from November-present; T055
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(16,1)`), color="red") + #big jump sensor; 30 cm; gap from late-October-present; T101
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(17,1)`)) + #NO SENSOR
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(18,1)`)) + #NO SENSOR
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(19,1)`)) + #NO SENSOR
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(20,1)`)) + #NO SENSOR
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(21,1)`)) + #NO SENSOR
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(22,1)`)) + #NO SENSOR
  labs(x = 'Time', y = 'Volumetric Water Content') +
  theme(axis.text=element_text(size=9),axis.title=element_text(size=18,face="bold"), legend.position = "none")

#Temperature
ggplot() + 
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(1,2)`)) +
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(2,2)`)) +
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(3,2)`)) +
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(4,2)`)) +
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(5,2)`)) +
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(6,2)`)) +
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(7,2)`)) +
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(8,2)`)) +
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(9,2)`)) +
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(10,2)`)) +
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(11,2)`)) +
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(12,2)`)) +
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(13,2)`)) +
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(14,2)`),color="blue") +
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(15,2)`)) +
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(16,2)`),color="red") +
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(17,2)`)) +
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(18,2)`)) +
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(19,2)`)) +
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(20,2)`)) +
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(21,2)`)) +
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(22,2)`)) +
  labs(x = 'Time', y = 'Temperature (°C)') +
  theme(axis.text=element_text(size=9),axis.title=element_text(size=18,face="bold"), legend.position = "none")

#Electrical Conductivity
ggplot() + 
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(1,3)`)) +
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(2,3)`)) +
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(3,3)`)) +
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(4,3)`)) +
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(5,3)`)) +
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(6,3)`)) +
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(7,3)`)) +
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(8,3)`)) +
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(9,3)`)) +
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(10,3)`)) +
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(11,3)`)) +
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(12,3)`)) +
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(13,3)`)) +
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(14,3)`), color="blue") +
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(15,3)`)) +
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(16,3)`),color="red") +
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(17,3)`)) +
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(18,3)`)) +
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(19,3)`)) +
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(20,3)`)) +
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(21,3)`)) +
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(22,3)`)) +
  labs(x = 'Time', y = 'Electrical Conductivity (uS/cm)') +
  theme(axis.text=element_text(size=9),axis.title=element_text(size=18,face="bold"), legend.position = "none")

#PNNL13 - Northeastern third of Control Plot; 17 sensors; could not open first data file - too large 5.3MB

files <- list.files("PNNL13/", pattern = "*.txt", full.names = TRUE)

dat <- bind_rows(lapply(files, read_tsv, col_names = c("TIMESTAMP",	"RECORD",	"Statname",	"Teros(1,1)",	"Teros(1,2)",
                                                       "Teros(1,3)",	"Teros(2,1)",	"Teros(2,2)",	"Teros(2,3)",	"Teros(3,1)",
                                                       "Teros(3,2)",	"Teros(3,3)",	"Teros(4,1)",	"Teros(4,2)",	"Teros(4,3)",
                                                       "Teros(5,1)",	"Teros(5,2)",	"Teros(5,3)",	"Teros(6,1)",	"Teros(6,2)",
                                                       "Teros(6,3)",	"Teros(7,1)",	"Teros(7,2)",	"Teros(7,3)",	"Teros(8,1)",	
                                                       "Teros(8,2)",	"Teros(8,3)",	"Teros(9,1)",	"Teros(9,2)",	"Teros(9,3)",
                                                       "Teros(10,1)",	"Teros(10,2)", "Teros(10,3)",	"Teros(11,1)",	"Teros(11,2)",
                                                       "Teros(11,3)",	"Teros(12,1)",	"Teros(12,2)",	"Teros(12,3)",	"Teros(13,1)",
                                                       "Teros(13,2)",	"Teros(13,3)",	"Teros(14,1)",	"Teros(14,2)",	"Teros(14,3)",
                                                       "Teros(15,1)",	"Teros(15,2)",	"Teros(15,3)",	"Teros(16,1)",	"Teros(16,2)",	
                                                       "Teros(16,3)",	"Teros(17,1)",	"Teros(17,2)",	"Teros(17,3)",	"Teros(18,1)",
                                                       "Teros(18,2)",	"Teros(18,3)",	"Teros(19,1)",	"Teros(19,2)",	"Teros(19,3)",
                                                       "Teros(20,1)",	"Teros(20,2)",	"Teros(20,3)",	"Teros(21,1)",	"Teros(21,2)",
                                                       "Teros(21,3)",	"Teros(22,1)",	"Teros(22,2)",	"Teros(22,3)"), skip = 1))

dat$TIMESTAMP <- as.POSIXct(strptime(dat$TIMESTAMP, format = '%m/%d/%Y %H:%M'))

Time <- dat[,c(1:3)];head(Time)

VWC_raw <- dat[,seq(4,69,3)];head(VWC_raw)
VWC_eq <- 3.879*10^-4*VWC_raw-0.6956
VWC <- cbind(Time,VWC_eq);head(VWC)

Temp <- dat[,c(1:3,seq(5,69,3))];head(Temp)

EC <- dat[,c(1:3,seq(6,69,3))];head(EC)

#Volumetric Water Content
ggplot() + 
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(1,1)`)) + #maintenance gap in February; T099
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(2,1)`)) + #no data until February; T113
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(3,1)`)) + #gap from late-October-February; T137
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(4,1)`)) + #gap from late September-February; T109
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(5,1)`)) + #maintenance gap in February; T103 
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(6,1)`)) + #gap from late September-February; T104
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(7,1)`)) + #no data until February; T033
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(8,1)`)) + #gap from November-February; T057
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(9,1)`)) + #maintenance gap in February; T035
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(10,1)`)) + #NO SENSOR
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(11,1)`)) + #NO SENSOR 
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(12,1)`)) + #no data until February; T110
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(13,1)`)) + #gap from mid November-February; T111
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(14,1)`)) + #NO SENSOR
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(15,1)`)) + #NO SENSOR
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(16,1)`)) + #NO SENSOR
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(17,1)`),color="blue") + #gap from late December-February; T134
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(18,1)`)) + #maintenance gap in February; T107
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(19,1)`),color="red") + #no data until February; T117
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(20,1)`),color="blue") + #no data until February; T095
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(21,1)`)) + #no data until February; T039
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(22,1)`),color="red") + #no data until February; T130
  labs(x = 'Time', y = 'Volumetric Water Content') +
  theme(axis.text=element_text(size=9),axis.title=element_text(size=18,face="bold"), legend.position = "none")

#Temperature
ggplot() + 
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(1,2)`)) +
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(2,2)`)) +
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(3,2)`)) +
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(4,2)`)) +
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(5,2)`)) +
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(6,2)`)) +
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(7,2)`)) +
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(8,2)`)) +
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(9,2)`)) +
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(10,2)`)) +
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(11,2)`)) +
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(12,2)`)) +
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(13,2)`)) +
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(14,2)`)) +
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(15,2)`)) +
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(16,2)`)) +
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(17,2)`),color="blue") +
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(18,2)`)) +
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(19,2)`),color="red") +
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(20,2)`),color="blue") +
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(21,2)`)) +
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(22,2)`),color="red") +
  labs(x = 'Time', y = 'Temperature (°C)') +
  theme(axis.text=element_text(size=9),axis.title=element_text(size=18,face="bold"), legend.position = "none")

#Electrical Conductivity
ggplot() + 
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(1,3)`)) +
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(2,3)`)) +
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(3,3)`)) +
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(4,3)`)) +
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(5,3)`)) +
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(6,3)`)) +
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(7,3)`)) +
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(8,3)`)) +
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(9,3)`)) +
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(10,3)`)) +
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(11,3)`)) +
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(12,3)`)) +
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(13,3)`)) +
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(14,3)`)) +
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(15,3)`)) +
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(16,3)`)) +
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(17,3)`),color="blue") +
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(18,3)`)) +
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(19,3)`),color="red") +
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(20,3)`),color="blue") +
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(21,3)`)) +
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(22,3)`),color="red") +
  labs(x = 'Time', y = 'Electrical Conductivity (uS/cm)') +
  theme(axis.text=element_text(size=9),axis.title=element_text(size=18,face="bold"), legend.position = "none")

#PNNL12 - Southern third of Control Plot; 15 sensors; could not open first data file - too large 5.4MB - NEED TO COME BACK TO THIS; EARLY SUMMER DATA????

files <- list.files("PNNL12/", pattern = "*.txt", full.names = TRUE)

dat <- bind_rows(lapply(files, read_tsv, col_names = c("TIMESTAMP",	"RECORD",	"Statname",	"Teros(1,1)",	"Teros(1,2)",
                                                       "Teros(1,3)",	"Teros(2,1)",	"Teros(2,2)",	"Teros(2,3)",	"Teros(3,1)",
                                                       "Teros(3,2)",	"Teros(3,3)",	"Teros(4,1)",	"Teros(4,2)",	"Teros(4,3)",
                                                       "Teros(5,1)",	"Teros(5,2)",	"Teros(5,3)",	"Teros(6,1)",	"Teros(6,2)",
                                                       "Teros(6,3)",	"Teros(7,1)",	"Teros(7,2)",	"Teros(7,3)",	"Teros(8,1)",	
                                                       "Teros(8,2)",	"Teros(8,3)",	"Teros(9,1)",	"Teros(9,2)",	"Teros(9,3)",
                                                       "Teros(10,1)",	"Teros(10,2)", "Teros(10,3)",	"Teros(11,1)",	"Teros(11,2)",
                                                       "Teros(11,3)",	"Teros(12,1)",	"Teros(12,2)",	"Teros(12,3)",	"Teros(13,1)",
                                                       "Teros(13,2)",	"Teros(13,3)",	"Teros(14,1)",	"Teros(14,2)",	"Teros(14,3)",
                                                       "Teros(15,1)",	"Teros(15,2)",	"Teros(15,3)",	"Teros(16,1)",	"Teros(16,2)",	
                                                       "Teros(16,3)",	"Teros(17,1)",	"Teros(17,2)",	"Teros(17,3)",	"Teros(18,1)",
                                                       "Teros(18,2)",	"Teros(18,3)",	"Teros(19,1)",	"Teros(19,2)",	"Teros(19,3)",
                                                       "Teros(20,1)",	"Teros(20,2)",	"Teros(20,3)",	"Teros(21,1)",	"Teros(21,2)",
                                                       "Teros(21,3)",	"Teros(22,1)",	"Teros(22,2)",	"Teros(22,3)"), skip = 1))

dat$TIMESTAMP <- as.POSIXct(strptime(dat$TIMESTAMP, format = '%m/%d/%Y %H:%M'))

Time <- dat[,c(1:3)];head(Time)

VWC_raw <- dat[,seq(4,69,3)];head(VWC_raw)
VWC_eq <- 3.879*10^-4*VWC_raw-0.6956
VWC <- cbind(Time,VWC_eq);head(VWC)

Temp <- dat[,c(1:3,seq(5,69,3))];head(Temp)

EC <- dat[,c(1:3,seq(6,69,3))];head(EC)

#Volumetric Water Content
ggplot() + 
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(1,1)`)) + #on in February; T112
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(2,1)`)) + #data for all chunks; on in February; T136
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(3,1)`)) + #on in February; T114
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(4,1)`)) + #on in February; T115
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(5,1)`)) + #on in February; T116
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(6,1)`)) + #NO SENSOR
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(7,1)`)) + #NO SENSOR
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(8,1)`)) + #NO SENSOR
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(9,1)`)) + #NO SENSOR
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(10,1)`)) + #data for all chunks; on in February; T036
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(11,1)`)) + #on in February; T037
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(12,1)`)) + #on in February; T074
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(13,1)`)) +  #on in February; T062
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(14,1)`),color="blue") + #data for all chunks; on in February; T108
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(15,1)`)) + #data for all chunks; on in February; T097
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(16,1)`),color="red") + #data for all chunks; on in February; T079
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(17,1)`),color="blue") + #on in February; T131
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(18,1)`)) + #on in February; T034
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(19,1)`),color="red") + #on in February; T127
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(20,1)`)) + #NO SENSOR
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(21,1)`)) + #NO SENSOR
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(22,1)`)) + #NO SENSOR
  labs(x = 'Time', y = 'Volumetric Water Content') +
  theme(axis.text=element_text(size=9),axis.title=element_text(size=18,face="bold"), legend.position = "none")

#Temperature
ggplot() + 
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(1,2)`)) +
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(2,2)`)) +
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(3,2)`)) +
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(4,2)`)) +
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(5,2)`)) +
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(6,2)`)) +
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(7,2)`)) +
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(8,2)`)) +
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(9,2)`)) +
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(10,2)`)) +
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(11,2)`)) +
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(12,2)`)) +
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(13,2)`)) +
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(14,2)`),color="blue") +
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(15,2)`)) +
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(16,2)`),color="red") +
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(17,2)`),color="blue") +
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(18,2)`)) +
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(19,2)`),color="red") +
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(20,2)`)) +
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(21,2)`)) +
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(22,2)`)) +
  labs(x = 'Time', y = 'Temperature (°C)') +
  theme(axis.text=element_text(size=9),axis.title=element_text(size=18,face="bold"), legend.position = "none")

#Electrical Conductivity
ggplot() + 
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(1,3)`)) +
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(2,3)`)) +
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(3,3)`)) +
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(4,3)`)) +
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(5,3)`)) +
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(6,3)`)) +
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(7,3)`)) +
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(8,3)`)) +
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(9,3)`)) +
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(10,3)`)) +
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(11,3)`)) +
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(12,3)`)) +
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(13,3)`)) +
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(14,3)`),color="blue") +
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(15,3)`)) +
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(16,3)`),color="red") +
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(17,3)`),color="blue") +
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(18,3)`)) +
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(19,3)`),color="red") +
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(20,3)`)) +
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(21,3)`)) +
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(22,3)`)) +
  labs(x = 'Time', y = 'Electrical Conductivity (uS/cm)') +
  theme(axis.text=element_text(size=9),axis.title=element_text(size=18,face="bold"), legend.position = "none")

#PNNL22 - Northwestern third of Freshwater Plot; 14 sensors; could not open first data file - too large 5.4MB or 02 archived file 6.4MB

files <- list.files("PNNL22/", pattern = "*.txt", full.names = TRUE)

dat <- bind_rows(lapply(files, read_tsv, col_names = c("TIMESTAMP",	"RECORD",	"Statname",	"Teros(1,1)",	"Teros(1,2)",
                                                       "Teros(1,3)",	"Teros(2,1)",	"Teros(2,2)",	"Teros(2,3)",	"Teros(3,1)",
                                                       "Teros(3,2)",	"Teros(3,3)",	"Teros(4,1)",	"Teros(4,2)",	"Teros(4,3)",
                                                       "Teros(5,1)",	"Teros(5,2)",	"Teros(5,3)",	"Teros(6,1)",	"Teros(6,2)",
                                                       "Teros(6,3)",	"Teros(7,1)",	"Teros(7,2)",	"Teros(7,3)",	"Teros(8,1)",	
                                                       "Teros(8,2)",	"Teros(8,3)",	"Teros(9,1)",	"Teros(9,2)",	"Teros(9,3)",
                                                       "Teros(10,1)",	"Teros(10,2)", "Teros(10,3)",	"Teros(11,1)",	"Teros(11,2)",
                                                       "Teros(11,3)",	"Teros(12,1)",	"Teros(12,2)",	"Teros(12,3)",	"Teros(13,1)",
                                                       "Teros(13,2)",	"Teros(13,3)",	"Teros(14,1)",	"Teros(14,2)",	"Teros(14,3)",
                                                       "Teros(15,1)",	"Teros(15,2)",	"Teros(15,3)",	"Teros(16,1)",	"Teros(16,2)",	
                                                       "Teros(16,3)",	"Teros(17,1)",	"Teros(17,2)",	"Teros(17,3)",	"Teros(18,1)",
                                                       "Teros(18,2)",	"Teros(18,3)",	"Teros(19,1)",	"Teros(19,2)",	"Teros(19,3)",
                                                       "Teros(20,1)",	"Teros(20,2)",	"Teros(20,3)",	"Teros(21,1)",	"Teros(21,2)",
                                                       "Teros(21,3)",	"Teros(22,1)",	"Teros(22,2)",	"Teros(22,3)"), skip = 1))

dat$TIMESTAMP <- as.POSIXct(strptime(dat$TIMESTAMP, format = '%m/%d/%Y %H:%M'))

Time <- dat[,c(1:3)];head(Time)

VWC_raw <- dat[,seq(4,69,3)];head(VWC_raw)
VWC_eq <- 3.879*10^-4*VWC_raw-0.6956
VWC <- cbind(Time,VWC_eq);head(VWC)

Temp <- dat[,c(1:3,seq(5,69,3))];head(Temp)

EC <- dat[,c(1:3,seq(6,69,3))];head(EC)

#Volumetric Water Content
ggplot() + 
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(1,1)`)) + #no data until February; T076
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(2,1)`)) + #no data until February; T077
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(3,1)`)) + #no data until February; T078
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(4,1)`)) + #no data until February; T053
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(5,1)`)) + #no data; T080
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(6,1)`)) + #no data until February; T081
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(7,1)`)) + #no data; T082
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(8,1)`)) + #no data until February; T083
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(9,1)`)) + #no data until February; T084
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(10,1)`)) + #no data until February; T085
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(11,1)`)) + #no data until February; T086
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(12,1)`)) + #NO SENSOR
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(13,1)`)) + #NO SENSOR
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(14,1)`),color="blue") + #no data; T040
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(15,1)`)) + #no data until February; T041
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(16,1)`),color="red") + #no data until February; T042
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(17,1)`)) + #NO SENSOR 
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(18,1)`)) + #NO SENSOR
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(19,1)`)) + #NO SENSOR
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(20,1)`)) + #NO SENSOR
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(21,1)`)) + #NO SENSOR
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(22,1)`)) + #NO SENSOR
  labs(x = 'Time', y = 'Volumetric Water Content') +
  theme(axis.text=element_text(size=9),axis.title=element_text(size=18,face="bold"), legend.position = "none")

#Temperature
ggplot() + 
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(1,2)`)) +
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(2,2)`)) +
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(3,2)`)) +
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(4,2)`)) +
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(5,2)`)) +
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(6,2)`)) +
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(7,2)`)) +
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(8,2)`)) +
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(9,2)`)) +
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(10,2)`)) +
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(11,2)`)) +
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(12,2)`)) +
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(13,2)`)) +
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(14,2)`),color="blue") +
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(15,2)`)) +
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(16,2)`),color="red") +
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(17,2)`)) +
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(18,2)`)) +
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(19,2)`)) +
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(20,2)`)) +
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(21,2)`)) +
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(22,2)`)) +
  labs(x = 'Time', y = 'Temperature (°C)') +
  theme(axis.text=element_text(size=9),axis.title=element_text(size=18,face="bold"), legend.position = "none")

#Electrical Conductivity
ggplot() + 
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(1,3)`)) +
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(2,3)`)) +
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(3,3)`)) +
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(4,3)`)) +
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(5,3)`)) +
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(6,3)`)) +
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(7,3)`)) +
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(8,3)`)) +
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(9,3)`)) +
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(10,3)`)) +
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(11,3)`)) +
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(12,3)`)) +
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(13,3)`)) +
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(14,3)`),color="blue") +
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(15,3)`)) +
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(16,3)`),color="red") +
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(17,3)`)) +
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(18,3)`)) +
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(19,3)`)) +
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(20,3)`)) +
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(21,3)`)) +
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(22,3)`)) +
  labs(x = 'Time', y = 'Electrical Conductivity (uS/cm)') +
  theme(axis.text=element_text(size=9),axis.title=element_text(size=18,face="bold"), legend.position = "none")

#PNNL21 - Northeastern third of Freshwater Plot; 17 sensors; could not open first data file - too large 5.4MB

files <- list.files("PNNL21/", pattern = "*.txt", full.names = TRUE)

dat <- bind_rows(lapply(files, read_tsv, col_names = c("TIMESTAMP",	"RECORD",	"Statname",	"Teros(1,1)",	"Teros(1,2)",
                                                       "Teros(1,3)",	"Teros(2,1)",	"Teros(2,2)",	"Teros(2,3)",	"Teros(3,1)",
                                                       "Teros(3,2)",	"Teros(3,3)",	"Teros(4,1)",	"Teros(4,2)",	"Teros(4,3)",
                                                       "Teros(5,1)",	"Teros(5,2)",	"Teros(5,3)",	"Teros(6,1)",	"Teros(6,2)",
                                                       "Teros(6,3)",	"Teros(7,1)",	"Teros(7,2)",	"Teros(7,3)",	"Teros(8,1)",	
                                                       "Teros(8,2)",	"Teros(8,3)",	"Teros(9,1)",	"Teros(9,2)",	"Teros(9,3)",
                                                       "Teros(10,1)",	"Teros(10,2)", "Teros(10,3)",	"Teros(11,1)",	"Teros(11,2)",
                                                       "Teros(11,3)",	"Teros(12,1)",	"Teros(12,2)",	"Teros(12,3)",	"Teros(13,1)",
                                                       "Teros(13,2)",	"Teros(13,3)",	"Teros(14,1)",	"Teros(14,2)",	"Teros(14,3)",
                                                       "Teros(15,1)",	"Teros(15,2)",	"Teros(15,3)",	"Teros(16,1)",	"Teros(16,2)",	
                                                       "Teros(16,3)",	"Teros(17,1)",	"Teros(17,2)",	"Teros(17,3)",	"Teros(18,1)",
                                                       "Teros(18,2)",	"Teros(18,3)",	"Teros(19,1)",	"Teros(19,2)",	"Teros(19,3)",
                                                       "Teros(20,1)",	"Teros(20,2)",	"Teros(20,3)",	"Teros(21,1)",	"Teros(21,2)",
                                                       "Teros(21,3)",	"Teros(22,1)",	"Teros(22,2)",	"Teros(22,3)"), skip = 1))

dat$TIMESTAMP <- as.POSIXct(strptime(dat$TIMESTAMP, format = '%m/%d/%Y %H:%M'))

Time <- dat[,c(1:3)];head(Time)

VWC_raw <- dat[,seq(4,69,3)];head(VWC_raw)
VWC_eq <- 3.879*10^-4*VWC_raw-0.6956
VWC <- cbind(Time,VWC_eq);head(VWC)

Temp <- dat[,c(1:3,seq(5,69,3))];head(Temp)

EC <- dat[,c(1:3,seq(6,69,3))];head(EC)

#Volumetric Water Content
ggplot() + 
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(1,1)`)) + #gap from mid-November-February; T001
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(2,1)`)) + #no data until February; T002
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(3,1)`)) + #maintenance gap in February; T003
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(4,1)`)) + #gap from late October-present; T004
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(5,1)`)) + #maintenance gap in February; T005
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(6,1)`)) + #maintenance gap in February; T006
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(7,1)`)) + #gap from late October-February; T007
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(8,1)`)) + #maintenance gap in February; T008
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(9,1)`)) + #maintenance gap in February; T009
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(10,1)`)) + #NO SENSOR
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(11,1)`)) + #NO SENSOR
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(12,1)`)) + #maintenance gap in February; T012
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(13,1)`)) + #gap from late-November-February; T013
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(14,1)`)) + #NO SENSOR
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(15,1)`)) + #NO SENSOR
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(16,1)`)) + #NO SENSOR
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(17,1)`),color="blue") + #no data until February; T121
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(18,1)`)) + #no data until February; T122
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(19,1)`),color="red") + #maintenance gap in February; T123
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(20,1)`),color="blue") + #maintenance gap in February; T124
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(21,1)`)) + #gap from late November-February; T125
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(22,1)`),color="red") + #no data until February; T126
  labs(x = 'Time', y = 'Volumetric Water Content') +
  theme(axis.text=element_text(size=9),axis.title=element_text(size=18,face="bold"), legend.position = "none")

#Temperature
ggplot() + 
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(1,2)`)) +
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(2,2)`)) +
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(3,2)`)) +
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(4,2)`)) +
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(5,2)`)) +
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(6,2)`)) +
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(7,2)`)) +
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(8,2)`)) +
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(9,2)`)) +
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(10,2)`)) +
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(11,2)`)) +
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(12,2)`)) +
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(13,2)`)) +
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(14,2)`)) +
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(15,2)`)) +
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(16,2)`)) +
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(17,2)`),color="blue") +
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(18,2)`)) +
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(19,2)`),color="red") +
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(20,2)`),color="blue") +
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(21,2)`)) +
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(22,2)`),color="red") +
  labs(x = 'Time', y = 'Temperature (°C)') +
  theme(axis.text=element_text(size=9),axis.title=element_text(size=18,face="bold"), legend.position = "none")

#Electrical Conductivity
ggplot() + 
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(1,3)`)) +
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(2,3)`)) +
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(3,3)`)) +
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(4,3)`)) +
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(5,3)`)) +
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(6,3)`)) +
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(7,3)`)) +
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(8,3)`)) +
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(9,3)`)) +
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(10,3)`)) +
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(11,3)`)) +
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(12,3)`)) +
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(13,3)`)) +
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(14,3)`)) +
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(15,3)`)) +
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(16,3)`)) +
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(17,3)`),color="blue") +
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(18,3)`)) +
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(19,3)`),color="red") +
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(20,3)`),color="blue") +
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(21,3)`)) +
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(22,3)`),color="red") +
  labs(x = 'Time', y = 'Electrical Conductivity (uS/cm)') +
  theme(axis.text=element_text(size=9),axis.title=element_text(size=18,face="bold"), legend.position = "none")

#PNNL23 - Southern third of Freshwater Plot; 15 sensors; could not open first data file - too large 5.4MB

files <- list.files("PNNL23/", pattern = "*.txt", full.names = TRUE)

dat <- bind_rows(lapply(files, read_tsv, col_names = c("TIMESTAMP",	"RECORD",	"Statname",	"Teros(1,1)",	"Teros(1,2)",
                                                       "Teros(1,3)",	"Teros(2,1)",	"Teros(2,2)",	"Teros(2,3)",	"Teros(3,1)",
                                                       "Teros(3,2)",	"Teros(3,3)",	"Teros(4,1)",	"Teros(4,2)",	"Teros(4,3)",
                                                       "Teros(5,1)",	"Teros(5,2)",	"Teros(5,3)",	"Teros(6,1)",	"Teros(6,2)",
                                                       "Teros(6,3)",	"Teros(7,1)",	"Teros(7,2)",	"Teros(7,3)",	"Teros(8,1)",	
                                                       "Teros(8,2)",	"Teros(8,3)",	"Teros(9,1)",	"Teros(9,2)",	"Teros(9,3)",
                                                       "Teros(10,1)",	"Teros(10,2)", "Teros(10,3)",	"Teros(11,1)",	"Teros(11,2)",
                                                       "Teros(11,3)",	"Teros(12,1)",	"Teros(12,2)",	"Teros(12,3)",	"Teros(13,1)",
                                                       "Teros(13,2)",	"Teros(13,3)",	"Teros(14,1)",	"Teros(14,2)",	"Teros(14,3)",
                                                       "Teros(15,1)",	"Teros(15,2)",	"Teros(15,3)",	"Teros(16,1)",	"Teros(16,2)",	
                                                       "Teros(16,3)",	"Teros(17,1)",	"Teros(17,2)",	"Teros(17,3)",	"Teros(18,1)",
                                                       "Teros(18,2)",	"Teros(18,3)",	"Teros(19,1)",	"Teros(19,2)",	"Teros(19,3)",
                                                       "Teros(20,1)",	"Teros(20,2)",	"Teros(20,3)",	"Teros(21,1)",	"Teros(21,2)",
                                                       "Teros(21,3)",	"Teros(22,1)",	"Teros(22,2)",	"Teros(22,3)"), skip = 1))

dat$TIMESTAMP <- as.POSIXct(strptime(dat$TIMESTAMP, format = '%m/%d/%Y %H:%M'))

Time <- dat[,c(1:3)];head(Time)

VWC_raw <- dat[,seq(4,69,3)];head(VWC_raw)
VWC_eq <- 3.879*10^-4*VWC_raw-0.6956
VWC <- cbind(Time,VWC_eq);head(VWC)

Temp <- dat[,c(1:3,seq(5,69,3))];head(Temp)

EC <- dat[,c(1:3,seq(6,69,3))];head(EC)

#Volumetric Water Content
ggplot() + 
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(1,1)`)) + #no data until February; T014
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(2,1)`)) + #briefly on in February, then off; T015
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(3,1)`)) + #no data until February; T016
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(4,1)`)) + #no data until February; T017
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(5,1)`)) + #no data until February; T018
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(6,1)`)) + #NO SENSOR
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(7,1)`)) + #NO SENSOR
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(8,1)`)) + #NO SENSOR
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(9,1)`)) + #NO SENSOR
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(10,1)`)) + #no data until February; T010
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(11,1)`)) + #no data until February; T011
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(12,1)`)) + #no data until February; T025
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(13,1)`)) + #no data until February; T026
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(14,1)`),color="blue") + #no data until February; T118
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(15,1)`)) + #no data until February; T119
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(16,1)`),color="red") + #no data until February; T120
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(17,1)`),color="blue") + #no data until February; T043
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(18,1)`)) + #no data until February; T044
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(19,1)`),color="red") + #gap from late December-February; T045
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(20,1)`)) + #NO SENSOR
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(21,1)`)) + #NO SENSOR
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(22,1)`)) + #NO SENSOR
  labs(x = 'Time', y = 'Volumetric Water Content') +
  theme(axis.text=element_text(size=9),axis.title=element_text(size=18,face="bold"), legend.position = "none")

#Temperature
ggplot() + 
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(1,2)`)) +
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(2,2)`)) +
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(3,2)`)) +
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(4,2)`)) +
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(5,2)`)) +
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(6,2)`)) +
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(7,2)`)) +
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(8,2)`)) +
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(9,2)`)) +
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(10,2)`)) +
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(11,2)`)) +
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(12,2)`)) +
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(13,2)`)) +
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(14,2)`),color="blue") +
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(15,2)`)) +
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(16,2)`),color="red") +
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(17,2)`),color="blue") +
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(18,2)`)) +
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(19,2)`),color="red") +
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(20,2)`)) +
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(21,2)`)) +
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(22,2)`)) +
  labs(x = 'Time', y = 'Temperature (°C)') +
  theme(axis.text=element_text(size=9),axis.title=element_text(size=18,face="bold"), legend.position = "none")

#Electrical Conductivity
ggplot() + 
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(1,3)`)) +
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(2,3)`)) +
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(3,3)`)) +
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(4,3)`)) +
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(5,3)`)) +
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(6,3)`)) +
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(7,3)`)) +
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(8,3)`)) +
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(9,3)`)) +
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(10,3)`)) +
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(11,3)`)) +
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(12,3)`)) +
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(13,3)`)) +
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(14,3)`),color="blue") +
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(15,3)`)) +
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(16,3)`),color="red") +
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(17,3)`),color="blue") +
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(18,3)`)) +
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(19,3)`),color="red") +
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(20,3)`)) +
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(21,3)`)) +
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(22,3)`)) +
  labs(x = 'Time', y = 'Electrical Conductivity (uS/cm)') +
  theme(axis.text=element_text(size=9),axis.title=element_text(size=18,face="bold"), legend.position = "none")

#PNNL31 - Northwestern third of Seawater Plot; 14 sensors; could not open first data file - too large 5.3MB

files <- list.files("PNNL31/", pattern = "*.txt", full.names = TRUE)

dat <- bind_rows(lapply(files, read_tsv, col_names = c("TIMESTAMP",	"RECORD",	"Statname",	"Teros(1,1)",	"Teros(1,2)",
                                                       "Teros(1,3)",	"Teros(2,1)",	"Teros(2,2)",	"Teros(2,3)",	"Teros(3,1)",
                                                       "Teros(3,2)",	"Teros(3,3)",	"Teros(4,1)",	"Teros(4,2)",	"Teros(4,3)",
                                                       "Teros(5,1)",	"Teros(5,2)",	"Teros(5,3)",	"Teros(6,1)",	"Teros(6,2)",
                                                       "Teros(6,3)",	"Teros(7,1)",	"Teros(7,2)",	"Teros(7,3)",	"Teros(8,1)",	
                                                       "Teros(8,2)",	"Teros(8,3)",	"Teros(9,1)",	"Teros(9,2)",	"Teros(9,3)",
                                                       "Teros(10,1)",	"Teros(10,2)", "Teros(10,3)",	"Teros(11,1)",	"Teros(11,2)",
                                                       "Teros(11,3)",	"Teros(12,1)",	"Teros(12,2)",	"Teros(12,3)",	"Teros(13,1)",
                                                       "Teros(13,2)",	"Teros(13,3)",	"Teros(14,1)",	"Teros(14,2)",	"Teros(14,3)",
                                                       "Teros(15,1)",	"Teros(15,2)",	"Teros(15,3)",	"Teros(16,1)",	"Teros(16,2)",	
                                                       "Teros(16,3)",	"Teros(17,1)",	"Teros(17,2)",	"Teros(17,3)",	"Teros(18,1)",
                                                       "Teros(18,2)",	"Teros(18,3)",	"Teros(19,1)",	"Teros(19,2)",	"Teros(19,3)",
                                                       "Teros(20,1)",	"Teros(20,2)",	"Teros(20,3)",	"Teros(21,1)",	"Teros(21,2)",
                                                       "Teros(21,3)",	"Teros(22,1)",	"Teros(22,2)",	"Teros(22,3)"), skip = 1))

dat$TIMESTAMP <- as.POSIXct(strptime(dat$TIMESTAMP, format = '%m/%d/%Y %H:%M'))

Time <- dat[,c(1:3)];head(Time)

VWC_raw <- dat[,seq(4,69,3)];head(VWC_raw)
VWC_eq <- 3.879*10^-4*VWC_raw-0.6956
VWC <- cbind(Time,VWC_eq);head(VWC)

Temp <- dat[,c(1:3,seq(5,69,3))];head(Temp)

EC <- dat[,c(1:3,seq(6,69,3))];head(EC)

#Volumetric Water Content
ggplot() + 
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(1,1)`)) + #no data until February; T027
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(2,1)`)) + #no data until February; T028
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(3,1)`)) + #gap from mid November-February; T029
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(4,1)`)) + #maintenance gap in February; T030
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(5,1)`)) + #gap from late October-February; T031
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(6,1)`)) + #gap from mid December-February; T032
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(7,1)`)) + #gag from late October-February; T020
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(8,1)`)) + #gap from mid January-February; T021
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(9,1)`)) + #gap from mid November-February; T022
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(10,1)`)) + #no data until February; T023
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(11,1)`)) + #no data until February; T024
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(12,1)`)) + #NO SENSOR
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(13,1)`)) + #NO SENSOR
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(14,1)`),color="blue") + #no data until February; T089
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(15,1)`)) + #no data until February; T090
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(16,1)`),color="red") + #no data until February; T091
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(17,1)`)) + #NO SENSOR
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(18,1)`)) + #NO SENSOR
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(19,1)`)) + #NO SENSOR
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(20,1)`)) + #NO SENSOR
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(21,1)`)) + #NO SENSOR
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(22,1)`)) + #NO SENSOR
  labs(x = 'Time', y = 'Volumetric Water Content') +
  theme(axis.text=element_text(size=9),axis.title=element_text(size=18,face="bold"), legend.position = "none")

#Temperature
ggplot() + 
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(1,2)`)) +
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(2,2)`)) +
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(3,2)`)) +
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(4,2)`)) +
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(5,2)`)) +
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(6,2)`)) +
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(7,2)`)) +
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(8,2)`)) +
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(9,2)`)) +
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(10,2)`)) +
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(11,2)`)) +
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(12,2)`)) +
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(13,2)`)) +
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(14,2)`),color="blue") +
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(15,2)`)) +
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(16,2)`),color="red") +
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(17,2)`)) +
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(18,2)`)) +
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(19,2)`)) +
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(20,2)`)) +
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(21,2)`)) +
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(22,2)`)) +
  labs(x = 'Time', y = 'Temperature (°C)') +
  theme(axis.text=element_text(size=9),axis.title=element_text(size=18,face="bold"), legend.position = "none")

#Electrical Conductivity
ggplot() + 
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(1,3)`)) +
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(2,3)`)) +
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(3,3)`)) +
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(4,3)`)) +
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(5,3)`)) +
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(6,3)`)) +
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(7,3)`)) +
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(8,3)`)) +
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(9,3)`)) +
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(10,3)`)) +
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(11,3)`)) +
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(12,3)`)) +
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(13,3)`)) +
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(14,3)`),color="blue") +
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(15,3)`)) +
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(16,3)`),color="red") +
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(17,3)`)) +
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(18,3)`)) +
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(19,3)`)) +
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(20,3)`)) +
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(21,3)`)) +
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(22,3)`)) +
  labs(x = 'Time', y = 'Electrical Conductivity (uS/cm)') +
  theme(axis.text=element_text(size=9),axis.title=element_text(size=18,face="bold"), legend.position = "none")

#PNNL33 - Southwestern third of Seawater Plot; 17 sensors; could not open first data file - too large 5.3MB

files <- list.files("PNNL33/", pattern = "*.txt", full.names = TRUE)

dat <- bind_rows(lapply(files, read_tsv, col_names = c("TIMESTAMP",	"RECORD",	"Statname",	"Teros(1,1)",	"Teros(1,2)",
                                                       "Teros(1,3)",	"Teros(2,1)",	"Teros(2,2)",	"Teros(2,3)",	"Teros(3,1)",
                                                       "Teros(3,2)",	"Teros(3,3)",	"Teros(4,1)",	"Teros(4,2)",	"Teros(4,3)",
                                                       "Teros(5,1)",	"Teros(5,2)",	"Teros(5,3)",	"Teros(6,1)",	"Teros(6,2)",
                                                       "Teros(6,3)",	"Teros(7,1)",	"Teros(7,2)",	"Teros(7,3)",	"Teros(8,1)",	
                                                       "Teros(8,2)",	"Teros(8,3)",	"Teros(9,1)",	"Teros(9,2)",	"Teros(9,3)",
                                                       "Teros(10,1)",	"Teros(10,2)", "Teros(10,3)",	"Teros(11,1)",	"Teros(11,2)",
                                                       "Teros(11,3)",	"Teros(12,1)",	"Teros(12,2)",	"Teros(12,3)",	"Teros(13,1)",
                                                       "Teros(13,2)",	"Teros(13,3)",	"Teros(14,1)",	"Teros(14,2)",	"Teros(14,3)",
                                                       "Teros(15,1)",	"Teros(15,2)",	"Teros(15,3)",	"Teros(16,1)",	"Teros(16,2)",	
                                                       "Teros(16,3)",	"Teros(17,1)",	"Teros(17,2)",	"Teros(17,3)",	"Teros(18,1)",
                                                       "Teros(18,2)",	"Teros(18,3)",	"Teros(19,1)",	"Teros(19,2)",	"Teros(19,3)",
                                                       "Teros(20,1)",	"Teros(20,2)",	"Teros(20,3)",	"Teros(21,1)",	"Teros(21,2)",
                                                       "Teros(21,3)",	"Teros(22,1)",	"Teros(22,2)",	"Teros(22,3)"), skip = 1))

dat$TIMESTAMP <- as.POSIXct(strptime(dat$TIMESTAMP, format = '%m/%d/%Y %H:%M'))

Time <- dat[,c(1:3)];head(Time)

VWC_raw <- dat[,seq(4,69,3)];head(VWC_raw)
VWC_eq <- 3.879*10^-4*VWC_raw-0.6956
VWC <- cbind(Time,VWC_eq);head(VWC)

Temp <- dat[,c(1:3,seq(5,69,3))];head(Temp)

EC <- dat[,c(1:3,seq(6,69,3))];head(EC)

#Volumetric Water Content
ggplot() + 
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(1,1)`)) +  #gap from late January-February; T063
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(2,1)`)) +  #on in February T064
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(3,1)`)) + #gap from mid November-February (no jumps); T065
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(4,1)`)) + #gap from mid November-February; T066
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(5,1)`)) + #no data until February; T067
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(6,1)`)) + #no data until February; T068
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(7,1)`)) + #no data until February; T069
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(8,1)`)) + #no data until February; T070
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(9,1)`)) + #no data until February; T071
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(10,1)`)) + #NO SENSOR
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(11,1)`)) + #NO SENSOR
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(12,1)`)) + #no data until February; T087
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(13,1)`)) + #gap from mid December-February (no jumps); T088
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(14,1)`)) + #NO SENSOR
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(15,1)`)) + #NO SENSOR
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(16,1)`)) + #NO SENSOR
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(17,1)`),color="blue") + #briefly on in February then off; T092
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(18,1)`)) + #gap from mid January-February (no jumps); T093
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(19,1)`),color="red") + #gap mid October-February (no jumps); T094
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(20,1)`),color="blue") + #no data until February; T046
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(21,1)`)) + #no data until February; T047
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(22,1)`),color="red") + #no data until February; T048
  labs(x = 'Time', y = 'Volumetric Water Content') +
  theme(axis.text=element_text(size=9),axis.title=element_text(size=18,face="bold"), legend.position = "none")

#Temperature
ggplot() + 
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(1,2)`)) +
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(2,2)`)) +
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(3,2)`)) +
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(4,2)`)) +
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(5,2)`)) +
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(6,2)`)) +
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(7,2)`)) +
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(8,2)`)) +
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(9,2)`)) +
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(10,2)`)) +
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(11,2)`)) +
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(12,2)`)) +
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(13,2)`)) +
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(14,2)`)) +
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(15,2)`)) +
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(16,2)`)) +
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(17,2)`),color="blue") +
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(18,2)`)) +
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(19,2)`),color="red") +
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(20,2)`),color="blue") +
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(21,2)`)) +
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(22,2)`),color="red") +
  labs(x = 'Time', y = 'Temperature (°C)') +
  theme(axis.text=element_text(size=9),axis.title=element_text(size=18,face="bold"), legend.position = "none")

#Electrical Conductivity
ggplot() + 
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(1,3)`)) +
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(2,3)`)) +
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(3,3)`)) +
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(4,3)`)) +
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(5,3)`)) +
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(6,3)`)) +
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(7,3)`)) +
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(8,3)`)) +
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(9,3)`)) +
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(10,3)`)) +
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(11,3)`)) +
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(12,3)`)) +
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(13,3)`)) +
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(14,3)`)) +
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(15,3)`)) +
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(16,3)`)) +
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(17,3)`),color="blue") +
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(18,3)`)) +
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(19,3)`),color="red") +
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(20,3)`),color="blue") +
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(21,3)`)) +
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(22,3)`),color="red") +
  labs(x = 'Time', y = 'Electrical Conductivity (uS/cm)') +
  theme(axis.text=element_text(size=9),axis.title=element_text(size=18,face="bold"), legend.position = "none")

#PNNL32 - Western (NandS) third of Seawater Plot; 15 sensors; could not open first data file - too large 5.3MB

files <- list.files("PNNL32/", pattern = "*.txt", full.names = TRUE)

dat <- bind_rows(lapply(files, read_tsv, col_names = c("TIMESTAMP",	"RECORD",	"Statname",	"Teros(1,1)",	"Teros(1,2)",
                                                       "Teros(1,3)",	"Teros(2,1)",	"Teros(2,2)",	"Teros(2,3)",	"Teros(3,1)",
                                                       "Teros(3,2)",	"Teros(3,3)",	"Teros(4,1)",	"Teros(4,2)",	"Teros(4,3)",
                                                       "Teros(5,1)",	"Teros(5,2)",	"Teros(5,3)",	"Teros(6,1)",	"Teros(6,2)",
                                                       "Teros(6,3)",	"Teros(7,1)",	"Teros(7,2)",	"Teros(7,3)",	"Teros(8,1)",	
                                                       "Teros(8,2)",	"Teros(8,3)",	"Teros(9,1)",	"Teros(9,2)",	"Teros(9,3)",
                                                       "Teros(10,1)",	"Teros(10,2)", "Teros(10,3)",	"Teros(11,1)",	"Teros(11,2)",
                                                       "Teros(11,3)",	"Teros(12,1)",	"Teros(12,2)",	"Teros(12,3)",	"Teros(13,1)",
                                                       "Teros(13,2)",	"Teros(13,3)",	"Teros(14,1)",	"Teros(14,2)",	"Teros(14,3)",
                                                       "Teros(15,1)",	"Teros(15,2)",	"Teros(15,3)",	"Teros(16,1)",	"Teros(16,2)",	
                                                       "Teros(16,3)",	"Teros(17,1)",	"Teros(17,2)",	"Teros(17,3)",	"Teros(18,1)",
                                                       "Teros(18,2)",	"Teros(18,3)",	"Teros(19,1)",	"Teros(19,2)",	"Teros(19,3)",
                                                       "Teros(20,1)",	"Teros(20,2)",	"Teros(20,3)",	"Teros(21,1)",	"Teros(21,2)",
                                                       "Teros(21,3)",	"Teros(22,1)",	"Teros(22,2)",	"Teros(22,3)"), skip = 1))

dat$TIMESTAMP <- as.POSIXct(strptime(dat$TIMESTAMP, format = '%m/%d/%Y %H:%M'))

Time <- dat[,c(1:3)];head(Time)

VWC_raw <- dat[,seq(4,69,3)];head(VWC_raw)
VWC_eq <- 3.879*10^-4*VWC_raw-0.6956
VWC <- cbind(Time,VWC_eq);head(VWC)

Temp <- dat[,c(1:3,seq(5,69,3))];head(Temp)

EC <- dat[,c(1:3,seq(6,69,3))];head(EC)

#Volumetric Water Content
ggplot() + 
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(1,1)`)) + #data for all chunks; on in February; T050
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(2,1)`)) + #data first two chunks; off in February; T051
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(3,1)`)) + #data for all chunks; on in February; T052
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(4,1)`)) + #data for all chunks; on in February; T102
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(5,1)`)) + #data for first two chunks; off in February; T054
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(6,1)`)) + #NO SENSOR
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(7,1)`)) + #NO SENSOR
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(8,1)`)) + #NO SENSOR
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(9,1)`)) + #NO SENSOR
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(10,1)`)) + #data for all chunks; on in February; T072
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(11,1)`)) + #data for all chunks; on in February; T073
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(12,1)`)) + #data for all chunks; on in February; T038
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(13,1)`)) + #data for first two chunks; briefly on in February, then off; T075
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(14,1)`),color="blue") + #data for first chunk; on in February; T105
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(15,1)`)) + #data for all chunks; on in February; T049
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(16,1)`),color="red") + #data for all chunks; on in February; T098
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(17,1)`),color="blue") + #data for all chunks; on in February; T106
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(18,1)`)) + #data for all chunks; on in February; T096
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(19,1)`),color="red") + #data for all chunks; on in February; T128
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(20,1)`)) + #NO SENSOR
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(21,1)`)) + #NO SENSOR
  geom_line(data = VWC, aes(x = TIMESTAMP, y = `Teros(22,1)`)) + #NO SENSOR
  labs(x = 'Time', y = 'Volumetric Water Content') +
  theme(axis.text=element_text(size=9),axis.title=element_text(size=18,face="bold"), legend.position = "none")

#Temperature
ggplot() + 
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(1,2)`)) +
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(2,2)`)) +
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(3,2)`)) +
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(4,2)`)) +
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(5,2)`)) +
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(6,2)`)) +
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(7,2)`)) +
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(8,2)`)) +
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(9,2)`)) +
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(10,2)`)) +
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(11,2)`)) +
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(12,2)`)) +
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(13,2)`)) +
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(14,2)`),color="blue") +
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(15,2)`)) +
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(16,2)`),color="red") +
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(17,2)`),color="blue") +
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(18,2)`)) +
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(19,2)`),color="red") +
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(20,2)`)) +
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(21,2)`)) +
  geom_line(data = Temp, aes(x = TIMESTAMP, y = `Teros(22,2)`)) +
  labs(x = 'Time', y = 'Temperature (°C)') +
  theme(axis.text=element_text(size=9),axis.title=element_text(size=18,face="bold"), legend.position = "none")

#Electrical Conductivity
ggplot() + 
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(1,3)`)) +
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(2,3)`)) +
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(3,3)`)) +
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(4,3)`)) +
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(5,3)`)) +
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(6,3)`)) +
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(7,3)`)) +
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(8,3)`)) +
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(9,3)`)) +
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(10,3)`)) +
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(11,3)`)) +
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(12,3)`)) +
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(13,3)`)) +
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(14,3)`),color="blue") +
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(15,3)`)) +
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(16,3)`),color="red") +
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(17,3)`),color="blue") +
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(18,3)`)) +
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(19,3)`),color="red") +
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(20,3)`)) +
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(21,3)`)) +
  geom_line(data = EC, aes(x = TIMESTAMP, y = `Teros(22,3)`)) +
  labs(x = 'Time', y = 'Electrical Conductivity (uS/cm)') +
  theme(axis.text=element_text(size=9),axis.title=element_text(size=18,face="bold"), legend.position = "none")

