# Process weather station data
# Stephanie Pennington | Created July 2018

# Read a single weather station file, reshape, parse, and join with station information
read_wxdat <- function(filename, wstation_info) {
  stopifnot(file.exists(filename))
  
  cat("Reading", filename, "...\n")
  filename %>% 
    read_csv(skip = 1, col_types = "icdddddddddd") %>%
    gather(label, Value, -1:-2) %>%  # first two columns are descriptive
    separate(label, sep = ", SEN S/N: ", into = c("firstpart", "Sensor_SN")) %>% # separate, remove unwanted info
    separate(firstpart, sep = ",", into = c("Sensor_Group", "info")) %>% 
    dplyr::select(-info) ->
    wdat
  wdat$Timestamp <- mdy_hms(wdat$`Date Time, GMT-04:00`)
  wdat$Sensor_SN <- as.integer(gsub(")", "", wdat$Sensor_SN))
  
  wdat %>% 
    left_join(wstation_info, by = "Sensor_SN") %>%  # Join data with station information
    separate(Sensor_Label, sep = "_", into = c("info1", "info2", "Sensor_Depth"), fill = "right") %>% 
    dplyr::select(-`Date Time, GMT-04:00`, -info1, -info2)
}

# Read all available weather or well station files, combine, and remove duplicate rows
read_all_wxdat <- function(dir, read_function, ...) {
  list.files(dir, pattern = "[0-9]{8}\\.csv$", full.names = TRUE) %>% 
    lapply(read_function, ...) %>% 
    bind_rows %>% 
    distinct
}

read_single_well <- function(filename) {
  stopifnot(file.exists(filename))
  
  cat("Reading", filename, "...\n")
  # The first line of the file holds the plot name
  readLines(filename, n = 1) %>% 
    gsub("Plot Title: ", "", .) %>% 
    gsub('"', '', .) ->
    plot
  
  read_csv(filename, skip = 2,
           col_names = c("#", "Timestamp", "Low_Range", "High_Range", "Temp",
                         paste0("X", 1:8)),
           col_types = "icdddcccccccc") %>% 
    dplyr::select(Timestamp, Low_Range, High_Range, Temp) %>% 
    mutate(Timestamp = mdy_hms(Timestamp),
           Plot = plot)
}
