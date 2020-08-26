
#----- Function to parse a file and return data frame -----
read_licor8100_data <- function(filename, debug = FALSE) {
  stopifnot(file.exists(filename))
  
  # Read file into memory and find records
  filedata <- readLines(filename)
  record_starts <- grep(pattern = "^LI-8100", filedata)
  cat("Reading...", basename(filename), " lines =", length(filedata), "observations =", length(record_starts), "\n")
  
  # Helper function to pull out data from line w/ specific label prefix
  find_parse <- function(tabletext, lbl) {
    line <- tail(grep(lbl, tabletext), n = 1)
    if(length(line)) {
      gsub(lbl, "", tabletext[line]) }
    else {
      ""
    }
  }
  
  results <- tibble(table = seq_along(record_starts),
                    Timestamp = as_datetime(NA),
                    Label = NA_character_,
                    Port = NA_integer_,
                    Flux = NA_character_,
                    R2 = NA_character_,
                    Tcham = NA_real_,
                    V1 = NA_real_, V2 = NA_real_, V3 = NA_real_, V4 = NA_real_,
                    RH = NA_real_,
                    Cdry = NA_real_,
                    Comments = NA_character_)
  
  for (i in seq_along(record_starts)) {
    if(i < length(record_starts)) {
      record_end <- record_starts[i+1]-1 
    } else {
      record_end <- length(filedata)
    }
    # Isolate the lines of this record...
    record <- filedata[record_starts[i]:record_end]
    # ...and get rid of blank lines because that can screw up paste(collapse()) below
    record <- record[grep("^$", record, invert = TRUE)]
    
    if(debug) cat("Record", i, "lines", record_starts[i], ":", 
                  record_end, "length", length(record), "\n")
    
    # Find the data table start
    table_start <- tail(grep("^Type\t", record), n = 1)
    # Look for the next non-numeric line; this marks the end
    table_stop <-  head(grep("^[A-Z]", record[-(1:table_start)]), n = 1) + table_start - 1
    
    # Sometimes the Licor aborts in the middle of a measurement. Handle gracefully
    if(length(table_stop) == 0) {
      message("Skipping table ", i, " ", record_starts[i], ":", record_end)
      next()
    }
    # Find names, discarding any trailing 'Annotation' column, because if it's empty
    # the Licor software doesn't add any trailing comma, which read_tsv can't handle
    col_names <- strsplit(record[table_start], "\t", fixed = TRUE)[[1]]
    col_names <- col_names[!grepl("Annotation", col_names)]
    if(debug) cat("\tReading table at record lines", table_start, ":", table_stop, "...")
    record[(table_start+1):table_stop] %>% 
      paste(collapse = "\n") %>% 
      readr::read_tsv(col_names = col_names) ->
      df
    
    errorlines <- which(df$Type < 0)
    if(length(errorlines)) {
      message("Error message in ", basename(filename), ":\n", 
              paste(df$Tcham[errorlines], collapse = ";"))
      message("Skipping")
      next()
    }
    
    # Pull out the data we're interested in
    index <- which(df$Type == 1)
    results$Timestamp[i] <- mean(df$Date)
    results$Label[i] <- find_parse(record, "^Label:\t")
    results$Port[i] <- find_parse(record, "^Port#:\t")
    results$Flux[i] <- find_parse(record, "^Exp_Flux:\t")
    results$R2[i] <- find_parse(record, "^Exp_R2:\t")
    results$Tcham[i] <- mean(df$Tcham[index])
    results$V1[i] <- mean(df$V1[index])
    results$V2[i] <- mean(df$V2[index])
    results$V3[i] <- mean(df$V3[index])
    results$V4[i] <- mean(df$V4[index])
    results$RH[i] <- mean(df$RH[index])
    results$Cdry[i] <- mean(df$Cdry[index])
    results$Comments[i] <- find_parse(record, "^Comments:\t")
    if(debug) cat(as.character(results$Timestamp[i]), results$Label[i], 
                  results$Port[i], results$Flux[i], results$Comments[i], "\n")
  }
  
  # Clean up and return
  results %>% 
    mutate(Port = as.integer(Port),
           Flux = as.numeric(Flux),
           R2 = as.numeric(R2))
}

#----- Function to loop through directory and call function to read licor data -----
read_licor_dir <- function(path) {
  stopifnot(dir.exists(path))
  files <- list.files(path, pattern = ".81x$", full.names = TRUE)
  lapply(files, read_licor8100_data) %>%
    bind_rows
}

# The Licor temperature was broken for several months in fall-winter 2018
# For those times, we took T5 by hand and entered it into a dedicated file
# Here we read in the T5 data directory
read_temp_dir <- function(path) {
  stopifnot(dir.exists(path))
  files <- list.files(path, pattern = ".csv", full.names = TRUE)
  lapply(files, read_csv, col_types = "cdd") %>% 
    bind_rows %>%
    # Occasionally we measured a collar's temperature twice, so average
    group_by(Date, Collar) %>%
    summarise(T5 = mean(T5)) %>%
    ungroup
}
