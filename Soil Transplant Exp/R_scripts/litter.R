## Function to read in a directory of litter data to combine in a long format for data visualization
## Created 2019-05-29

library(readr)
library(ggplot2)
library(dplyr)

# Read in litter directory and combine to one data frame
read_litter_data <- function(path) {
  litter_data <- list.files(path = path, pattern = "*.csv$", full.names = TRUE) %>% 
  lapply(read_csv) %>% 
  bind_rows()
    
 # Select useful columns 
litter_data %>% 
  gather(key = "Litter_Type", value = "Mass_g", -Plot, -Trap, -Date_weighed, -Date_collected, -Notes) %>% 
  select(-Date_weighed, -Notes)
}
