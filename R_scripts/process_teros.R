library(readr)
library(lubridate)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(kableExtra)
set.seed(7)


process_teros <- function(token, datadir) {
    # Generate list of 'current' teros files
    t_dir <- drop_dir(datadir, dtoken = token)
    t_files <- grep(t_dir$path_display, pattern = "Terosdata\\.dat$", value = TRUE)

    teros_inventory <- read_csv("TEROS_Network_Location copy.csv")

    lapply(t_files, fileread, token, length(t_files)) %>% bind_rows() -> teros_primitive

    teros_primitive %>%
        left_join(teros_inventory, by = c("Logger" = "Data Logger ID",
                                          "Data_Table_ID" = "Terosdata table channel")) %>%
        select(- `Date of Last Field Check`) %>%
        rename("Active_Date" = "Date Online (2020)",
               "Grid_Square" = "Grid Square") ->
        teros

    nomatch <- anti_join(teros, teros_inventory, by = c("Logger" = "Data Logger ID",
                                                            "Data_Table_ID" = "Terosdata table channel"))
    if(nrow(nomatch) > 0) {
        warning("There were logger/channel combinations that I couldn't find in teros_inventory.csv:")
        nomatch %>%
            distinct(Logger, Data_Table_ID) %>%
            kable()
    }

    return(teros)
}
