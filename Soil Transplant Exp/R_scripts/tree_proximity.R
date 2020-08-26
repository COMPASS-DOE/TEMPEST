# Process and analyze tree proximity with Licor flux data
# Stephanie Pennington | Created June 2018

library(readr)
library(dplyr)
library(ggplot2)
library(ggrepel)

# ----- Step 1: Read tree data -----
cat("Reading tree proximity data...\n")
proxDat <- read_csv("../inventory_data/collar_proximity.csv")
proxDat$Tag <- as.character(proxDat$Tag)

# ----- Step 2: QC/error check -----
# Warning generated if duplicate tag/tree is found
# cat("Checking for duplicate trees...\n")
# for(i in nrow(proxDat)) {
#   if (any(duplicated(proxDat$Tag))) {
#     stop("\n Tag duplicated: ", proxDat$Tag[duplicated(proxDat$Tag)])
#   } else {
#     cat("No duplicates found.")
#   }
# }

# ----- Step 3: Read Licor and tre inventory data -----
cat("Reading Licor data...\n")
licorDat <- get(load("../outputs/licordat.rda"))
treeDat <- read_csv("../inventory_data/inventory.csv")

# ----- Step 4: Join datasets by collar -----
cat("Joining datasets...\n")
collar_to_tree_prox <- select(proxDat, -Date) %>% 
  left_join(treeDat, by = c("Site", "Plot", "Tag"), na_matches = "never") %>%
  mutate(BA_sqm = (DBH_cm / 100 / 2) ^ 2 * pi)  # from DBH (cm) to area (m2)
write_csv(collar_to_tree_prox, "../inventory_data/collar_to_tree_prox.csv")

# Replace DBH with recorded value for non-tagged trees
no_tags <- is.na(collar_to_tree_prox$Tag)
collar_to_tree_prox$DBH_cm[no_tags] <- collar_to_tree_prox$No_tag_DBH[no_tags]
collar_to_tree_prox$Species[no_tags] <- collar_to_tree_prox$No_tag_species[no_tags]

# Replace DBH with recorded value for non-tagged trees
for (i in nrow(collar_to_tree_prox)) {
  if (is.na(collar_to_tree_prox$Tag[i])) {
    collar_to_tree_prox$DBH_cm[i] <- collar_to_tree_prox$No_tag_DBH[i]
    collar_to_tree_prox$Species[i] <- collar_to_tree_prox$No_tag_species[i]
  }
}
write_csv(collar_to_tree_prox, "../outputs/collar_to_tree_prox.csv")

# ----- Step 5: Plot distance vs. number of trees at each collar -----
tree_frequency <- collar_to_tree_prox %>% group_by(Collar, Distance_m) %>%  
  summarize(tree_num=n(), BA_sqm = sum(BA_sqm)) %>%
  mutate(n=cumsum(tree_num), BA_sqm = cumsum(BA_sqm))
write_csv(tree_frequency, "../outputs/tree_frequency.csv")

tree_cumdist <- ggplot(data = tree_frequency, aes(x = Distance_m, y = n, group = Collar, color = Collar)) +
  geom_line() +
#  geom_text_repel(aes(label = Collar)) +
  scale_color_gradient(low = "red", high = "purple") +
  ggtitle("Cumulative distribution of trees")
print(tree_cumdist)

BA_cumdist <- ggplot(data = tree_frequency, aes(x = Distance_m, y = BA_sqm, group = Collar, color = Collar)) +
  geom_line() +
  geom_point() +
  ggtitle("Cumulative distribution of basal area")
print(BA_cumdist)

BA_dist <- ggplot(data = collar_to_tree_prox, aes( x = Distance_m, y = BA_sqm, group = Collar)) + 
  geom_point() + 
  facet_wrap(~ Collar) +
  ggtitle("Distribution of basal area")
print(BA_dist)

# Calculate cumulative basal area at each distance 
BA_dat <- list()
for (i in 1:10) {
  collar_to_tree_prox %>% 
    filter(Distance_m <= i) %>% 
    group_by(Collar) %>% 
    summarise(n = n(), BA_m2 = sum(BA_sqm, na.rm = TRUE), dist = i) ->
    BA_dat[[i]]
  # m <- collar_to_tree_prox[which(collar_to_tree_prox$Distance_m <= i),]
  # BA_dat[[BA_i]] <- m %>% group_by(Collar) %>%
  #   summarize(n = n(), BA_i = sum(BA_sqm))
}

BA_dat <- bind_rows(BA_dat)
