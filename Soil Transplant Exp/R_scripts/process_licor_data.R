# process_licor_data.R
# Stephanie Pennington | March 2018
# Function to process one licor file

process_licor_data <- function(raw_data, collar_data, plot_data, temp_data) {
  stopifnot(is.data.frame(raw_data))
  stopifnot(is.data.frame(collar_data))
  stopifnot(is.data.frame(plot_data))
  stopifnot(is.data.frame(temp_data))
  
  raw_data %>%
    rename(T5 = V4, 
           SMoisture = V3, 
           Collar = Label,      # we record Collar in the label field
           T20 = Comments) %>%  # we record T20 in the comments field
    mutate(T20 = as.numeric(T20),
           Collar = as.integer(Collar), 
           # The Licor doesn't write out timezone information, and when this is read in,
           # UTC is assumed. Change this. *** NOTE *** this code will not work is used
           # for anywhere other than the East Coast (looking at you Beaver Creek!)
           Timestamp = force_tz(Timestamp, tz = "America/New_York")) ->
    rawDat
  
  temp_data %>%
    mutate(Date = mdy(Date, tz = "America/New_York")) ->
    temp_data_time
  
  cat("Joining datasets and calculating...\n")
  
  # Merge these datasets together based on collar number and plot name
  licorDat <- left_join(rawDat, collar_data, by = "Collar") %>% 
    rename(Origin_Plot = Plot) %>%
    select(-Site) %>% 
    left_join(plot_data, by = c("Origin_Plot" = "Plot")) %>%
    rename(Origin_Salinity = Salinity, Origin_Elevation = Elevation) %>%
    select(-Site)
  
  # For any transplant core X, we know (in "Core_placement") the hole in which it 
  # ended up (or rather, the core number of the hole). We actually need to know 
  # the plot. So create a lookup table for this...
  lookup_table <- select(collar_data, Collar, Destination_Plot = Plot)
  
  # The following operations should NOT change number of observations
  nobs <- nrow(licorDat)
  
  # ...and then merge back into main data frame. Now "Lookup_Plot" holds the plot
  # info for where each core ENDED UP, not where it STARTED
  licorDat <- left_join(licorDat, lookup_table, by = c("Core_placement" = "Collar")) %>% 
    # Remove duplicate variables
    select(-Longitude, -Latitude, -Plot_area_m2) %>% 
    left_join(plot_data, by = c("Destination_Plot" = "Plot")) %>%
    rename(Dest_Salinity = Salinity, Dest_Elevation = Elevation)
  
  # Add a factor column that's the site name for pretty printing/plotting
  sitenames <- c("High" = "GCReW", "Medium" = "Canoe Shed", "Low" = "North Branch")
  licorDat$Dest_Site <- factor(sitenames[licorDat$Dest_Salinity], levels = sitenames) 
    
  # The Licor temperature sensor was broken for several months in fall-winter 2018
  # For those times, we took T5 by hand and entered it into a dedicated file
  # Here we merge licor data with these 5cm temperatures
  licorDat %>% 
    mutate(Date = floor_date(Timestamp, unit = "day")) %>% 
    left_join(temp_data_time, by = c("Date", "Collar")) -> 
    licorDat
    
  # Replace error T5 data
  t5_replace <- which(!is.na(licorDat$T5.y))
  licorDat$T5.x[t5_replace] <- licorDat$T5.y[t5_replace]
  
  licorDat %>%
    select(-T5.y) %>%
    rename(T5 = T5.x) -> licorDat

  # Check 
  stopifnot(nobs == nrow(licorDat))  # should not have changed
  
  # Reorder labels by making them into factors and return
  HML <- c("High", "Medium", "Low")
  licorDat %>% 
    mutate(Origin_Salinity = factor(Origin_Salinity, levels = HML),
           Origin_Elevation = factor(Origin_Elevation, levels = HML),
           Dest_Salinity = factor(Dest_Salinity, levels = HML),
           Dest_Elevation = factor(Dest_Elevation, levels = HML),
           Date = as_date(Timestamp),
           Group = paste(Origin_Plot, "->", Destination_Plot),
           Group = if_else(Experiment == "Control", "Control", Group))
}


# Calculate daily averages for flux, temp, and soil moisture for each collar
calculate_licor_daily_data <- function(licor_data) {
  cat("Calculating daily averages, CVs, etc...\n")
  licor_data %>% 
    group_by(Date, Experiment, Group, Destination_Plot, Dest_Salinity, Dest_Site,
             Dest_Elevation, Origin_Plot, Origin_Salinity, Origin_Elevation, Collar) %>%
    summarise(n = n(), 
              Timestamp = mean(Timestamp),
              meanFlux = mean(Flux), 
              sdFlux = sd(Flux), 
              meanSM = mean(SMoisture), 
              meanT5 = mean(T5), 
              meanT20 = mean(T20)) %>% 
    ungroup
}

process_continuous_data <- function(raw_data) {
  # Ports 6 and 8 have no temperature or moisture probes at the moment
  DATE_SENSORS_INSTALLED <- ymd("2018-11-20")
  raw_data %>% 
    filter(Port > 0) %>% 
    rename(T5 = V3, SMoist = V2) %>% 
    mutate(T5 = if_else(Port %in% c(6, 8) & Timestamp < DATE_SENSORS_INSTALLED, NA_real_, T5), 
           SMoist = if_else(Port %in% c(6, 8) & Timestamp < DATE_SENSORS_INSTALLED, NA_real_, SMoist),
           # Chamber 8 was miswired between 2018-08-31 and 2019-03-11
           # For these records the temperature data is in V1 (see issue #82)
           T5 = if_else(T5 > 100 & Port == 8, V1, T5)
    )
}
