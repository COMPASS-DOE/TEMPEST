## This script imports raw data for NPOC and TDN measured using a Shimadzu TOC-L
## at PNNL MCRL and exports clean, Level 0B QC'ed data. 
## Data are read in from GitHub
## 
## Created: 2022-01-15 by Peter Regier for EXCHANGE
## Updated: 2022-07-19 by Allison Myers-Pigg for TEMPEST
## 
##
# #############
# #############

### QUESTION: How do we want to handle LODs when run across multiple instruments?

# 1. Setup ---------------------------------------------------------------------

# load packages
require(pacman)
pacman::p_load(tidyverse, # keep things tidy
               janitor, # useful for simplifying column names
               googlesheets4, # read_sheet 
               googledrive) # drive_ functions

## Set theme
theme_set(theme_bw())

## Set LOD (for all data after 10/11/2021)
## If any data were before 10/11/21, lod_npoc = 0.27, lod_tdn = 0.070
#lod_npoc <- 0.076
#lod_tdn <- 0.014

#NEED TO UPDATE LODS FOR TMP RUNS##

## Set Github filepath for NPOC raw data files
setuser <- "/Users/myer056/OneDrive - PNNL" #I cant get user agnostic file paths to work
directory = paste0(setuser,"/Documents/GitHub/TEMPEST/June_Event/Porewaters/pw_raw_npoc_tn_data/")

# 2. Functions -----------------------------------------------------------------

## Create a function to read in data
read_data <- function(data){
  # First, scrape date from filename
  date <- str_extract(data, "[0-9]{8}")
  # Second, read in data
  read_delim(file = data, skip = 10, delim = "\t") %>% 
    rename(sample_name = `Sample Name`, 
           npoc_raw = `Result(NPOC)`, 
           tdn_raw = `Result(TN)`,
           run_datetime = `Date / Time`) %>% 
    select(sample_name, npoc_raw, tdn_raw,run_datetime) %>% 
    mutate(date = date)
}

read_mes <- function(readme){
  # First, scrape date from filename
  date <- str_extract(readme, "[0-9]{8}")
  # Second, read in Read Me
  readxl::read_excel(path = readme, sheet = 1) %>% 
    rename(sample_name = `Sample Name`,
           sample_vol = `Sample Wt (g)`,
           total_vol = `Sample`) %>% 
    select(sample_name, Action) %>% 
    mutate(date = date)
}
# 3. Import data ---------------------------------------------------------------

## Create a list of files to download
files <- list.files(path = directory, pattern = "Summary", full.names = TRUE) 
ReadMes <- list.files(path = directory, pattern = "Readme", full.names = TRUE) 

## Read in data, filter to TMP samples, and add sample name, add readme actions
npoc_raw <- files %>% 
  map_df(read_data) %>% 
  filter(grepl("TMP", sample_name)) %>% # filter to TMP samples only
  bind_rows() 

readmes_dilution_action <- ReadMes %>% 
  map_df(read_mes) %>% 
  filter(grepl("TMP", sample_name)) %>% # filter to TMP samples only
  filter(Action %in% "Dilution correction needed") %>%
  bind_rows() 

######Stopped here 7/19######
# 4. Dilution Corrections ------------------------------------------------------

dilutions = 
  readmes_dilution_action %>% 
  rename(Name = `Sample Name`) %>% 
  mutate(date_run = str_extract(source, "[0-9]{8}"),
         date_run = lubridate::as_date(date_run)) %>% 
  dplyr::select(date_run, Name, Action, Dilution) %>% 
  force()


samples_dilution_corrected = 
  samples_blank_corrected %>% 
  left_join(dilutions, by = c("Name", "date_run")) %>% 
  filter(!Action %in% "Omit") %>% 
  mutate(Amount_bl_dil_corrected = Amount_bl_corrected * Dilution) %>% 
  mutate(Amount_bl_dil_corrected = as.numeric(Amount_bl_dil_corrected),
         Amount_bl_dil_corrected = round(Amount_bl_dil_corrected, 3)) %>% 
  dplyr::select(Name, date_run, Ion, Amount_bl_dil_corrected, flag) %>% 
  filter(Amount_bl_dil_corrected > 0)

samples_dilution_corrected

# 5. Calculate blanks and add to data ------------------------------------------

blanks <- npoc_raw %>% 
  filter(grepl("^Blank", sample_name)) %>% 
  group_by(date) %>% 
  summarize(npoc_blank_raw = round(mean(npoc_raw[!is.na(npoc_raw)]), 2), 
            tdn_blank_raw = round(mean(tdn_raw[!is.na(tdn_raw)]), 2)) %>% 
  mutate(npoc_blank = ifelse(npoc_blank_raw > lod_npoc, npoc_blank_raw, 0), 
         tdn_blank = ifelse(tdn_blank_raw > lod_tdn, tdn_blank_raw, 0)) %>% 
  select(date, npoc_blank, tdn_blank)


# 6. Add blanks data -----------------------------------------------------------

npoc_blank_corrected <- npoc_raw %>% 
  filter(grepl("EC1_K", sample_name)) %>% # filter to EC1 samples only
  mutate(campaign = "EC1", 
         kit_id = substr(sample_name, 5, 9), 
         transect_location = "water") %>% 
  inner_join(blanks, by = "date") %>% 
  mutate(npoc_mgl = npoc_raw - npoc_blank, 
         tdn_mgl = tdn_raw - tdn_blank)


# 7. Clean data ----------------------------------------------------------------



## Helper function to calculate mean if numeric, otherwise first (needed to 
## preserve dates, which are different for duplicated kits)
mean_if_numeric <- function(x){
  ifelse(is.numeric(x), mean(x, na.rm = TRUE), first(x))
}

## Another step before finalizing is taking care of pesky duplicates from reruns
npoc_duplicates_removed <- npoc_blank_corrected %>% 
  select(campaign, transect_location, kit_id, date, npoc_mgl, tdn_mgl, npoc_blank, tdn_blank) %>% 
  group_by(kit_id) %>% 
  summarize(across(everything(), .f = mean_if_numeric))

## The last step is flagging data
npoc_raw_flags <- npoc_duplicates_removed %>% 
  ## First, round each parameter to proper significant figures
  mutate(npoc_mgl = round(npoc_mgl, 2), 
         tdn_mgl = round(tdn_mgl, 3)) %>% 
  ## Second, add flags for outside LOD
  mutate(npoc_flag = ifelse(npoc_mgl < lod_npoc | npoc_mgl > 30, "npoc outside range", NA), #per cal curve upper limit
         tdn_flag = ifelse(tdn_mgl < lod_tdn | tdn_mgl > 3, "tdn outside range", NA))

npoc <- npoc_raw_flags %>% 
  select(date, campaign, kit_id, transect_location, npoc_mgl, tdn_mgl, contains("_flag"))


# 7. Write data ----------------------------------------------------------------

write_csv(npoc, paste0("/Documents/GitHub/TEMPEST/June_Event/Porewaters/TMP_PW_NPOC_TDN_L0B_", Sys.Date(), ".csv"))


