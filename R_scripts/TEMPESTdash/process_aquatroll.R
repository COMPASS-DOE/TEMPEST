# TODO: this is preliminary, as I don't have Dropbox access

library(readr)
library(lubridate)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(kableExtra)
set.seed(7)


process_aquatroll <- function(token, datadir) {

    if(!is.null(getDefaultReactiveDomain())) {
        progress <- incProgress
    } else {
        progress <- NULL
    }

    change_date <- "2021-03-10 00:00:00"

    change_IDs <- c("PNNL_13", "PNNL_23", "PNNL_32")
    change_instrument <- "TROLL600"

    troll_inventory <- read_csv("aquatroll_inventory copy.csv")

    atroll <- compasstools::process_aquatroll_dir(datadir, "EST",
                                                  dropbox_token = token,
                                                  progress_bar = progress)

    atroll %>%
        mutate(Install = if_else(Timestamp >= change_date &
                                     Logger_ID %in% change_IDs &
                                     Instrument == change_instrument, 2, 1)) %>%
        left_join(troll_inventory, by = c("Logger_ID", "Instrument", "Install"))
}
