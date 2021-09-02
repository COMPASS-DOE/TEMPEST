library(readr)
library(lubridate)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(kableExtra)
set.seed(7)


process_teros <- function(token) {

    # Generate list of 'current' teros files
    t_dir <- drop_dir("TEMPEST_PNNL_Data/Current_Data/", dtoken = token)
    t_files <- grep(t_dir$path_display, pattern = "Terosdata\\.dat$", value = TRUE)

    teros_inventory <- read_csv("../../Data/TEROS12/TEROS_Network_Location.csv")

    lapply(t_files, fileread, token) %>% bind_rows() -> teros_primative

    teros_primative %>%
        mutate(diff = difftime(Sys.time(), TIMESTAMP, units = "days")) %>%
        filter(diff < 5) %>%
        #na count needs to be before this
        # NAs = sum(!is.finite(M_Value)),
        left_join(teros_inventory, by = c("Logger" = "Data Logger ID",
                                          "Data_Table_ID" = "Terosdata table channel")) %>%
        select(- `Date of Last Field Check`) %>%
        rename("Active_Date" = "Date Online (2020)") -> teros

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
