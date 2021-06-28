# TEMPEST TEROS NETWORK DATA PROCESSING FUNCTION
# Anya Hopple - 2021-03-04

# (moved into separate script for broader usage on 2021-06-25 by SCP)

# Parse loggernet data streams into data tables for researcher use on
# the TEMPEST Project. Functions are ordered and annotated for routine implementation.

# Helper function: read string vector into a data frame, reshape, and drop NA
fileread <- function(fn) {
    message("Reading ", basename(fn), "...")
    # Lines 1, 3, and 4 of the TEROS data files contain sensor metadata that we want to remove
    # Read the data files into a string vector, remove those lines, and then pass to read.csv()
    rawdata <- readLines(fn)[-c(1, 3, 4)]
    textConnection(rawdata) %>%
        read.csv(check.names = FALSE, na.strings = "NAN", stringsAsFactors = FALSE) %>%
        as_tibble() %>%
        # Reshape the data frame to one observation per row
        gather(channel, value, -TIMESTAMP, -RECORD, -Statname) %>%
        filter(!is.na(value)) %>%
        # Pull data logger ID out of statname
        separate(Statname, into = c("Inst", "Data_Logger_ID"), sep = "_" ) %>%
        # Parse the data logger number, channel number, and variable number out of the
        # Statname and Channel columns
        mutate(Data_Logger_ID = as.integer(Data_Logger_ID, fixed = TRUE),
               TIMESTAMP = ymd_hms(TIMESTAMP)) %>%
        select(-Inst) %>%  # unneeded
        # Next, parse channel into the data logger channel and variable number
        separate(channel, into = c("Data_Table_ID", "variable"), sep = ",") %>%
        mutate(Data_Table_ID = as.integer(gsub("Teros(", "", Data_Table_ID, fixed = TRUE)),
               variable = as.integer(gsub(")", "", variable, fixed = TRUE)),
               # Give them sensible names
               variable = case_when(variable == 1 ~ "VWC",
                                    variable == 2 ~ "TSOIL",
                                    variable == 3 ~ "EC"))
}
