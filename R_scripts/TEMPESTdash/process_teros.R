library(readr)
library(lubridate)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(kableExtra)
set.seed(7)


process_teros <- function(token, datadir) {

    teros_inventory <- read_csv("TEROS_Network_Location copy.csv")

    if(!is.null(getDefaultReactiveDomain())) {
        progress <- incProgress
    } else {
        progress <- NULL
    }
    teros_primitive <- compasstools::process_teros_dir(datadir, tz = "EST",
                                                      dropbox_token = token,
                                                      progress_bar = progress)

    teros_primitive %>%
        left_join(teros_inventory, by = c("Logger" = "Data Logger ID",
                                          "Data_Table_ID" = "Terosdata table channel")) %>%
        select(- `Date of Last Field Check`) %>%
        rename("Active_Date" = "Date Online (2020)",
               "Grid_Square" = "Grid Square") %>%
        filter(!is.na(ID)) ->
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
