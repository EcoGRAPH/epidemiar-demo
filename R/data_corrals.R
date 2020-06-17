# Environmental Data Corral -----------------------------------------------

# Documentation for user-defined function corral_environment()
#
# This function is designed specific for combining environmental data from
# Google Earth Engine and our previous EPIDEMIA environmental data, for use in
# model and report generation with epidemiar package.
#
# Overview: All environmental data files are stored in subfolder
# "data_environmental/". The function will loop through all csv (GEE) files and
# bind them, append that to the old EPIDEMIA data, then remove any duplicates
# (key: woreda, environmental variable, date), favoring data from file more
# recently modified. It checks for any gaps in data per env variable, and will
# stop if gaps are found, with a log file of missing.
#
# Function is flexible to accept new GEE variables and files, as long as the
# file has: wid, doy, year, [env variable - any number, as long as variables
# have a unique name. To be included in modeling, they must have an entry in
# environ_info.xlsx and included in the model variables, e.g.
# falciparum_model.csv]. wid: shapefile ID of the woreda used by GEE to get
# woreda-level data, doy: day of year, year: Gregorian calendar year
#
# Function depends on libraries: dplyr, tidyr, lubridate, readxl, readr
#
# There is a single argument, a tibble of report woredas. To reduce processing
# time, only data for these woredas will be included in the processing and
# output tibble. This is also used to match the woreda name from the WID
# returned by GEE. 
#
# The data directory is hard-coded and should not be changed.
#
# Function returns a tibble with combined and checked daily environmental data
# per woreda per variable.
# 
#
# There are two associated helper functions that are used internally for
# pre-processing of environmental data files: 
# env_csv_processing(): transforms a read in GEE csv file into a standard long data format.
# get_mtime(): captures the last modified time of a file, used to get the most recent GEE data when there are multiple observations for the same woreda-variable-day combination. 
#

env_csv_processing <- function(df, mtime){
  this_data <- df %>% 
    #if present, drop "woreda" field (will use wid to link) 
  {if("woreda" %in% names(.)) dplyr::select(., -woreda) else dplyr::select(., dplyr::everything())} %>% 
    #gather all environmental variables into long format (all columns that are not wid, doy, or year)
    tidyr::gather(key = "environ_var_code", value = "obs_value", -wid, -doy, -year) %>% 
    #add ISO week date (day of year added to Jan 1 of that year, index 0, so subtract 1)
    dplyr::mutate(obs_date = as.Date(doy - 1, origin = lubridate::ymd(year, truncated = 2)),
                  data_time = mtime) %>% 
    #drop doy and year fields now that we have obs_date
    dplyr::select(-doy, -year) %>% 
    #rename wid field to match capitalization
    dplyr::rename(WID = wid)
}
#custom function for getting mtime of file
get_mtime <- function(fname){
  file.info(fname)$mtime
}



corral_environment <- function(report_woredas){

  message("Reading environmental data...")
    
  # GEE data
  #get list of csv files
  env_csv_files_raw <- list.files("data_environmental/", full.names = TRUE, pattern="*.csv$")
  
  #keep the names of only csv files that are not empty
  file_condition <- sapply(env_csv_files_raw, function(x) {length(readr::count_fields(x, readr::tokenizer_csv())) > 1})
  env_csv_files <- env_csv_files_raw[file_condition]
  
  #initialize list of correct length
  env_data_list_raw <- vector("list", length(env_csv_files))
  
  #lapply to read in all GEE data files, into a list of datasets
  env_data_list_raw <- lapply(env_csv_files, readr::read_csv, col_types = cols())
  
  #also get file modified times 
  env_file_times <- lapply(env_csv_files, get_mtime)
  
  #apply to process all environmental csv input data: long form data with field for file modified time
  env_data_list <- mapply(env_data_list_raw, env_file_times, FUN = env_csv_processing, SIMPLIFY = FALSE)
  
  #bind each list item (from each csv file) into one dataset
  env_data <- dplyr::bind_rows(env_data_list)
  
  message("Processing environmental data...")
  
  # To reduce processing time, filter only report woredas here
  env_data <- env_data %>% 
    dplyr::filter(WID %in% report_woredas$WID)
  
  # Remove duplicates
  env_data <- env_data %>% 
    #group by woreda & environment variable & date
    dplyr::group_by(WID, environ_var_code, obs_date) %>% 
    #order by data time, with the most recent first
    dplyr::arrange(desc(data_time)) %>% 
    #take the first (or only) one
    dplyr::slice(1) %>% 
    #drop data time
    dplyr::select(-data_time) %>% 
    #ungroup to finish
    dplyr::ungroup()
  
  
  #Find/warn if gaps
  # only done per variable, not also per woreda because of processing time, 
  # and with using GEE, the data should be even across all woredas
  #create date table
  env_dts <- env_data %>% 
    #by each env var
    dplyr::group_by(environ_var_code) %>% 
    #first and last dates
    dplyr::summarize(mindt = min(obs_date), maxdt = max(obs_date)) %>% 
    #making a list of dates (per row calc for seq(), list column for dates)
    dplyr::rowwise() %>% 
    dplyr::mutate(dts = list(seq(mindt, maxdt, by = "1 day"))) 
  
  #examine dates against full list
  env_miss <- env_data %>% 
    #per environment variable (not woreda, because processing takes too much time)
    dplyr::group_by(environ_var_code) %>% 
    #list of dates present in data
    dplyr::summarize(unidts = list(unique(obs_date))) %>% 
    #join to get full list of dates that should be present
    dplyr::left_join(env_dts %>% dplyr::select(environ_var_code, dts), by = "environ_var_code") %>% 
    #per row (kept for next step as well)
    dplyr::rowwise() %>% 
    #get list of dates NOT present (setdiff loses date format, so reset)
    dplyr::mutate(missing = list(as.Date(setdiff(dts, unidts), origin = lubridate::origin))) %>% 
    #clean up
    dplyr::select(-unidts, -dts)
  
  #count number of missing dates
  num_env_missing <- env_miss %>% 
    dplyr::summarize(len = length(missing)) %>% 
    dplyr::summarize(nummiss = sum(len))
  
  if (num_env_missing > 0) {
    readr::write_csv(env_miss %>% tidyr::unnest(), "log_missing_environ.csv")
    stop("Some dates in the environmental data are missing. Check error file 'log_missing_environ.csv' for details.")
  }
  
  # Get woreda names
  env_data <- env_data %>% 
    dplyr::left_join(report_woredas %>% dplyr::select(WID, woreda_name),
                     by = "WID") %>% 
    #order columns nicely
    dplyr::select(WID, woreda_name, environ_var_code, obs_date, obs_value)
  
  # Message with last date of env data
  max_env <- env_data %>% group_by(environ_var_code) %>% summarize(start_dt = min(obs_date), end_dt = max(obs_date))
  message("Environmental data date range (YYYY-MM-DD):")
  message(paste0(capture.output(max_env), collapse = "\n"))
  
  env_data
  
}



# Epidemiological Data Corral ---------------------------------------------

# Documentation for user-defined function corral_epidemiological()
#
# This function is designed specific for combining epidemiology data
# from more recent collected surveillance data and our previous EPIDEMIA data,
# for use in model and report generation with epidemiar package.
# 
# Overview: All epidemiological data files are stored in subfolder "data_epidemiological/".
# The function will loop through all xlsx files and bind them, append that 
# to the old EPIDEMIA data, then remove any duplicates (key: woreda, date), 
# favoring data from file more recently modified.
# Function should be flexible enough to handle overlapping or partial year files, 
# as long as there are no gaps in dates. It will check for any gaps in data per woreda, 
# and will stop if found, with a log file of missing.
#
# Dates:
# Converion from Ethiopian date to Gregorian date was done by
# adding 7 (ISO week >= 28) or 8 (ISO week < 28) years, 
# e.g. budget year 2011 is in year 2018 for weeks 28 - 52, and 2019 for weeks 1 - 27. 
# Currently, no R function exists for converting full Ethiopian dates to full Gregorian calendar dates.
# obs_date: last day of ISO-8601 week.
#
# Data manipulation:
# The epidemiar package needs total malaria case counts per species - to create that, we add the 
# count of positive blood tests and the count of positive rapid diagnositic tests, per species. 
# test_pf_tot = `Blood film P. falciparum` + `RDT P. falciparum`,
# test_pv_only = `Blood film P. vivax` + `RDT P. vivax`.
# 
# Function depends on libraries: dplyr, lubridate, readxl, readr, epidemiar
# Needs additional metadata files: 
#  Spelling crosswalk: data/woreda_spellings.xlsx
#  Split woreda information: data/woredas_split.xlsx
#  Population data: data/population_byweek_v201811.csv
#   Note: this population data is from the EPIDEMIA project, with population of at risk and total numbers,
#         that were then extended out into the past and future, using an estimated population growth factor
#
# The function takes a single argument, a vector of woreda names to include in the report, 
# up to the user to ensure that these woredas have both epidemiological and environmental data, and cluster id for model.
# Directories are hard-coded and should not be changed.
# 
# Function returns a tibble with combined and checked weekly epidemiological data per woreda.
#

corral_epidemiological <- function(report_woreda_names){
  
  message("Reading epidemiological data...")
  
  # Read in metadata files
  #spelling crosswalk
  spell <- readxl::read_xlsx("data/woreda_spellings.xlsx")
  #woredas that have since been split, but need to be combined for reports
  split <- readxl::read_xlsx("data/woredas_split.xlsx")
  #population data
  pop <- readr::read_csv("data/population_weekly_2012-2030.csv", col_types = cols()) %>% 
    dplyr::mutate(obs_date = epidemiar::make_date_yw(year, week_of_year, 7))
  
  
  # Read in past epi data, processed through old EPIDEMIA system
  if (file.exists("data_epidemiological/epi_data_20120712_20170708.xlsx")){
    past_epi <- readxl::read_xlsx("data_epidemiological/epi_data_20120712_20170708.xlsx", na = "NA") %>% 
      #drop unneeded columns
      dplyr::select(-WID, -tot_case, -mal_case) %>% 
      #data type match
      dplyr::mutate(obs_date = as.Date(obs_date),
                    #set a data time of 2001-01-01 (very "old" date)
                    #  so that local/original collected data would be preferentially used instead
                    data_time = as.POSIXct("2001-01-01 00:00:00"))
  }
  
  # Read in new data
  #get list of xlsx files
  epi_xlsx_files <- list.files("data_epidemiological/", full.names = TRUE, pattern="*.xlsx$") %>% 
    #exclude Windows/MS temporary files (having ~$ in the file name)
    grep("~\\$", ., value = TRUE, invert = TRUE) %>% 
    #pull out epi_data_20120712_20170708.xlsx (dealt with separately). 
    grep("*epi_data_20120712_20170708.xlsx$", ., value = TRUE, invert = TRUE)
  
  #initialize tibble to collect all data
  epi_data <- dplyr::tibble()
  #loop
  for (i in 1:length(epi_xlsx_files)){
    #need to set certain column types to avoid excessive warnings
    #Ethiopian dates do not fit well into other date classes due to calendar format
    #grab column names
    nms <- names(suppressMessages(suppressWarnings(read_xlsx(epi_xlsx_files[[i]]))))
    
    #quick check for an empty/blank file, if blank, it does nothing to skip to next file
    if (length(nms) > 0) {
      
      #set the Ethiopian dates to text, and have it guess from data on the rest
      ct <- ifelse(grepl("Report Started Date|Report End Date", nms), "text", "guess")
      #read in, "NA"s to be read as NA values 
      this_data <- suppressMessages(suppressWarnings(readxl::read_xlsx(epi_xlsx_files[[i]], col_types = ct, na = "NA"))) 
      #process data columns
      this_data <- this_data %>% 
        dplyr::transmute(woreda_name = `Woreda/Hospital`,
                         #add number of years to convert Ethiopian budget year to Gregorian year, 
                         # budget years seems to always start Week 28
                         yrs_to_add = dplyr::if_else(`Epi- Week` >= 28, 7, 8),
                         #ISO end date
                         obs_date = epidemiar::make_date_yw(`Budget Year` + yrs_to_add, `Epi- Week`, 7 ),
                         #all falciparam positive tests (or mixed)
                         test_pf_tot = `Blood film P. falciparum` + `RDT P. falciparum`,
                         #all P. vivax positive tests
                         test_pv_only = `Blood film P. vivax` + `RDT P. vivax`,
                         #time file last modified, for deduplication (should be rare, and give warnings)
                         data_time = file.info(epi_xlsx_files[[1]])$mtime) %>% 
        #drop intermediate/testing columns
        dplyr::select(-yrs_to_add)
      
      # append
      epi_data <- dplyr::bind_rows(epi_data, this_data)
      
    }
    
  }
  
  message("Processing epidemiological data...")
  
  # Woreda matching, spelling changes
  spell_list <- stats::setNames(as.character(spell$woreda_name), spell$to_replace)
  #replace
  epi_data <- epi_data %>% 
    dplyr::mutate(woreda_name = dplyr::recode(woreda_name, !!!spell_list))
  
  # Append old and new
  if (exists("past_epi")){
    epi_data <- dplyr::bind_rows(past_epi, epi_data) %>% 
      dplyr::arrange(obs_date, woreda_name) %>% 
      #but drop popul_malar: this won't be added if past epi data was not present
      # and pop_at_risk will be added in anyway later on, when we link up for the newer data
      dplyr::select(-popul_malar)
  }
  
  # De-duplication
  epi_data <- epi_data %>% 
    #group by woreda & date
    dplyr::group_by(woreda_name, obs_date) %>% 
    #order by data time, with the most recent first
    dplyr::arrange(desc(data_time)) %>% 
    #take the first (or only) one
    dplyr::slice(1) %>% 
    #drop data time
    dplyr::select(-data_time) %>% 
    #ungroup to finish
    dplyr::ungroup()
  
  
  # Missing check, find/warn if gaps
  #create date table
  epi_dts <- epi_data %>% 
    #by each woreda (important for split woredas to test individually)
    dplyr::group_by(woreda_name) %>% 
    #first and last dates
    dplyr::summarize(mindt = min(obs_date), maxdt = max(obs_date)) %>% 
    #making a list of dates (per row calc for seq(), list column for dates)
    dplyr::rowwise() %>% 
    dplyr::mutate(dts = list(seq(mindt, maxdt, by = "1 week"))) 
  #note: for any and all woredas in any data source
  
  #examine dates against full list
  epi_miss <- epi_data %>% 
    #per woreda
    dplyr::group_by(woreda_name) %>% 
    #list of dates present in data
    dplyr::summarize(unidts = list(unique(obs_date))) %>% 
    #get full list of dates that should be present
    dplyr::left_join(epi_dts %>% select(woreda_name, dts), by = "woreda_name") %>% 
    #per row (kept for next step as well)
    dplyr::rowwise() %>% 
    #get list of dates NOT present (setdiff loses date format, so reset)
    dplyr::mutate(missing = list(as.Date(setdiff(dts, unidts), origin = lubridate::origin))) %>% 
    #clean up
    dplyr::select(-unidts, -dts)
  #note: for any and all woredas in any data source
  
  #only stop for missing report woredas OR SPLIT woredas
  epi_imp <- c(report_woreda_names, split$split_1, split$split_2)
  epi_miss <- epi_miss %>% 
    dplyr::filter(woreda_name %in% epi_imp)
  
  num_epi_missing <- epi_miss %>% 
    dplyr::summarize(len = length(missing)) %>% 
    dplyr::summarize(nummiss = sum(len))
  
  if (num_epi_missing > 0) {
    readr::write_csv(epi_miss %>% tidyr::unnest(), "log_missing_report_epidemiology.csv")
    warning("Some dates in the epidemiological data are missing. Check error file 'log_missing_report_epidemiology.csv' for details.")
  }
  
  
  # Split Woredas
  # Woredas may have split since project started, 
  # but currently do not have env data or modeling clusters on them, so need to combine
  
  #named list of splits
  split_list <- stats::setNames(c(as.character(split$woreda_name), as.character(split$woreda_name)),
                                c(split$split_1, split$split_2))
  
  #add column for combined woreda name, for grouping on
  epi_data <- epi_data %>% 
    dplyr::mutate(woreda_master = dplyr::recode(woreda_name, !!!split_list))
  
  #check for combined & split data for same date 
  epi_split_overlap <- epi_data %>% 
    dplyr::group_by(woreda_master, obs_date) %>% 
    #1: not split, 2: split with non-overlapping data, 3: split with overlapping data
    dplyr::summarize(entries = n()) 
  
  #on these woreda-dates, want to filter out rows woreda_name == woreda_master 
  epi_data <- epi_data %>% 
    dplyr::left_join(epi_split_overlap, by = c("woreda_master", "obs_date")) %>% 
    dplyr::filter(!(entries == 3 & woreda_name == woreda_master))
  
  #combining
  epi_data <- epi_data %>% 
    #rename fields to avoid later confusion
    dplyr::rename(woreda_prev = woreda_name,
                  woreda_name = woreda_master) %>% 
    #group by master/combined woreda & date
    dplyr::group_by(woreda_name, obs_date) %>% 
    #use summarize to add split woreda rows together 
    #(for non-split, will only have one row in woreda-date grouping)
    # specifically NOT using na.rm here, want to preserve NAs, and if 1/2 woreda was NA, then combined should be as well
    dplyr::summarize(test_pf_tot = sum(test_pf_tot),
                     test_pv_only = sum(test_pv_only)) %>% 
    #end with ungrouping
    dplyr::ungroup()
  
  
  # Add population (pop_at_risk in pop dataset)
  epi_data <- epi_data %>% 
    dplyr::left_join(pop, by = c("woreda_name", "obs_date")) %>%
    dplyr::select(-c(population, year, week_of_year))

  # Limit to report woredas
  epi_data <- epi_data %>% dplyr::filter(woreda_name %in% report_woreda_names)
  
  # Order nicely
  epi_data <- epi_data %>% dplyr::select("WID", "woreda_name", dplyr::everything())
  
  # Message with last date of epi data
  message("Epidemiological data date range is ", min(epi_data$obs_date), " to ", max(epi_data$obs_date), " (YYYY-MM-DD).")
  
  #returning
  epi_data
  
}


