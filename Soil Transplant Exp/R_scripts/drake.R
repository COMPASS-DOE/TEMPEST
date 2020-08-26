# Main project file
# November 13, 2018 BBL

library(drake)  # 6.1.0
pkgconfig::set_config("drake::strings_in_dots" = "literals")

library(MASS) # 7.3-51.1 
library(readr)
library(dplyr)   # needs to come after MASS above so select() isn't masked
library(tidyr)
library(ggplot2)
theme_set(theme_bw())
library(lubridate)
library(kableExtra)
library(piecewiseSEM)
library(cowplot)

# Load our functions
source("read_licor_data.R")
source("process_licor_data.R")
source("inventory.R")
source("weatherdat.R")
source("litter.R")
source("datahub.R")

do_filedigest <- function(dir) digest::digest(list.files(dir)) # helper function

plan <- drake_plan(
  
  # ----- Inventory and design datasets -----
  # `plot_data` holds information based on the plot code: longitude, latitude,
  # area, and salinity/elevation levels
  plot_data = read_csv(file_in("../design/plots.csv"), col_types = "ccccddi"),
  
  # `collar_data` holds information about the collars, based on collar number: 
  # its origin plot, and (if a transplant collar) into what hole it ended up 
  collar_data = read_csv(file_in("../design/cores_collars.csv"), col_types = "cciiicic"),
  
  # `inventory_data` and `species_codes` hold tree information
  inventory_data = read_inventory(file_in("../inventory_data/inventory.csv")),
  species_codes = read_csv(file_in("../inventory_data/species_codes.csv"), col_types = "ccc"),
  tree_data = make_tree_data(inventory_data, species_codes, plot_data),
  
  # ----- Weather data from Hobo loggers and wells -----
  # We digest the filename list to detect when something changes in the data directories
  # Not perfect--this won't detect a change *within* a file
  wstation_info = read_csv(file_in("../weather_data/wstation_info.csv"), col_types = "cicicc"),
  weather_data = target(command = read_all_wxdat("../weather_data/", read_wxdat, wstation_info),
                        trigger = trigger(change = do_filedigest("../weather_data/"))),
  # Conductivity data from wells
  well_data = target(command = read_all_wxdat("../well_data/", read_single_well),
                     trigger = trigger(change = do_filedigest("../well_data/"))),
  
  # ----- Licor data -----
  # We digest the filename list to detect when something changes in the LI-8100A_data directory
  raw_licor_data = target(command = read_licor_dir("../LI-8100A_data/"),
                          trigger = trigger(change = do_filedigest("../LI-8100A_data/"))),
  # T5 taken by hand because of broken sensor
  temp_data = target(command = read_temp_dir("../LI-8100A_data/temp_dat/"),
                     trigger = trigger(change = do_filedigest("../LI-8100A/temp_dat/"))),
  # Process, adding in treatment etc. information
  licor_data = process_licor_data(raw_licor_data, collar_data, plot_data, temp_data),
  # Reduce observations to a per-day average
  licor_daily_data = calculate_licor_daily_data(licor_data),
  
  raw_con_licor_data = target(command = read_licor_dir("../LI-8100A_data/longterm_dat/"),
                              trigger = trigger(change = do_filedigest("../LI-8100A_data/longterm_dat/"))),
  con_licor_data = process_continuous_data(raw_con_licor_data),
  
  # ----- Litter data -----
  litter_data = target(command = read_litter_data("../litter_data"),
                       trigger = trigger(change = do_filedigest("../litter_data"))),
  
  # # ----- Proximity data for SP's proximity analysis manuscript -----
  # prox_data = read_csv(file_in("../inventory_data/collar_proximity.csv"), col_types = "ccidccdcc"),
  # 
  # # Proximity analysis report
  # prox_report = rmarkdown::render(
  #   knitr_in("proximity_results.Rmd"),
  #   output_file = file_out("proximity_results.html"),
  #   quiet = TRUE),
  
  # ----- Global soil respiration database -----
  srdb = read.csv(file_in("../ancillary_data/srdb-data.csv"), stringsAsFactors = FALSE),
  # 
  # # Trend emergence presentation
  # trend_report = rmarkdown::render(
  #   knitr_in("trend_emergence.Rmd"),
  #   output_file = file_out("trend_emergence.html"),
  #   quiet = TRUE),
  # 
  # ----- BBL's temporal scaling report -----
  # ts_report = rmarkdown::render(
  #   knitr_in("temporal_scaling.Rmd"),
  #   output_file = file_out("temporal_scaling.html"),
  #   quiet = TRUE),
  #
  # ----- BBL exploratory -----
  # autochamber_play = rmarkdown::render(
  #   knitr_in("autochamber_play.Rmd"),
  #   output_file = file_out("autochamber_play.html"),
  #   quiet = TRUE),
  
  # Generate the data to update PNNL's DataHub system
  datahub = make_datahub()
)

message("Now type `make(plan)` at command line")
