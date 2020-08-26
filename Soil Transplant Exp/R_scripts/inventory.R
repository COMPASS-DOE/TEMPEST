# Produce summary statistics of the tree inventory data
# PREMIS-ghg April 2018 | Updated March 2020  
# Ben Bond-Lamberty | Stephanie Pennington

# Function to parse multi-year inventory into long format
read_inventory <- function(path) {
  inventory_wide <- read_csv(path, col_types = "cccccdccdccc")
  
  # Gather columns from multiple years 
  inventory_wide %>%
    gather("Type", "Value", matches("_[0-9]{4}$")) %>% 
    separate(Type, into = c("Category", "Unit", "Year"), sep = "_")  %>% 
    select(-Unit, - Year) %>%
    group_by(Category) %>% 
    mutate(grouped_id = row_number()) %>% 
    spread(Category, Value) %>% 
    select(-grouped_id) 

}

# Join inventory data with species and plot information
make_tree_data <- function(inventory_data, species_codes, plot_data) {
  
  # Join the two and check for any unknown species code
  inventory_data %>%
    left_join(species_codes, by = "Species_code") %>% 
    mutate(Date = lubridate::mdy(Date)) -> trees
  
  unmatched <- filter(trees, is.na(Species))
  if(nrow(unmatched)) {
    warning("Species codes not found:", unique(unmatched$Species_code))  
  }
  
  trees$Salinity <- paste("Salinity", substr(trees$Plot, 1, 1))
  trees$Salinity <- factor(trees$Salinity, levels = paste("Salinity", c("H", "M", "L")))
  trees$Elevation <- paste("Elevation", substr(trees$Plot, 3, 3))
  trees$Elevation <- factor(trees$Elevation, levels = paste("Elevation", c("H", "M", "L")))
  
  trees %>% 
    filter(Site == "SERC") %>% # temporary - only handle SERC
    left_join(select(plot_data, Site, Plot, Plot_area_m2), by = c("Site", "Plot")) 
}
