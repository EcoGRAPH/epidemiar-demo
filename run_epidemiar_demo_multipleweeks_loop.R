# ###############################################################################################
#
# This script does all the data collating and processing before calling the
# epidemiar function to run the modeling, forecasting, and early detection and
# early warning alert algorithms.
#
# At the end, this script will generate a pdf report using
# epidemia_report_demo.Rnw
#
# See documentation/walkthrough.pdf for more details on each step.
#
# ###############################################################################################

# 0. Looping version ------------------------------------------------------
#
# This version of the script can be used to loop through multiple weeks to
# generate reports for each.
#
# Set the loop variable to TRUE, and change the isoyear and isoweeks wanted.
#
# This is different from just adjusting the fc_start_date, because this censors
# epidemiological and environmental data as well, mimicking what data would be
# available at the time, rather than changing when forecasting started but using
# all available data.


loop <- TRUE
wk_list <- c(epidemiar::make_date_yw(year = 2016, week = c(23:24), weekday = 7),
             epidemiar::make_date_yw(year = 2017, week = c(37:40), weekday = 7))


# 1. Libraries & Functions ------------------------------------------------------

## Load packages necessary for script

#make sure pacman is installed
if (!require("pacman")) install.packages("pacman")

#load packages
#note: data corrals use dplyr, tidyr, lubridate, readr, readxl, epidemiar
#note: pdf creation requires knitr, tinytex
pacman::p_load(dplyr,
               knitr,
               lubridate,
               parallel,
               readr, 
               readxl,
               tidyr,
               tinytex,
               tools)
#load specialized package (https://github.com/EcoGRAPH/epidemiar)
library(epidemiar)

## Locally-defined Functions
source("R/data_corrals.R") 
source("R/report_save_create_helpers.R")

#due to experimental dplyr::summarise() parameter
options(dplyr.summarise.inform=F)

# 2. Reading in the Data -----------------------------------------------------

# read in woreda metadata
report_woredas <- read_csv("data/amhara_woredas.csv") %>% 
  filter(report == 1)
#Note: report woredas must have sufficient epi data, env data, and model cluster information, in appropriate files

# read & process case data needed for report
epi_data <- corral_epidemiological(report_woreda_names = report_woredas$woreda_name)

# read & process environmental data for woredas in report
env_data <- corral_environment(report_woredas = report_woredas)

## Optional: For slight speed increase, 
# date filtering to remove older environmental data.
# older env data was included to demo epidemiar::env_daily_to_ref() function.
env_start_date <- epidemiar::make_date_yw(year = 2012, week = 1, weekday = 7) #week is always end of the week, 7th day
env_data <- env_data %>%
  filter(obs_date >= env_start_date)

# read in climatology / environmental reference data
env_ref_data <- read_csv("data/env_ref_data_2002_2018.csv", col_types = cols())

# read in environmental info file
env_info <- read_xlsx("data/environ_info.xlsx", na = "NA")

# read in forecast and event detection parameters
source("data/epidemiar_settings_demo.R")


# 3-4B. Loop run epidemiar ------------------------------------------------

# Keep full data. Within each loop it will filter from full dataset. 
epi_data_full <- epi_data
env_data_full <- env_data

#Enter into loop if loop var is set to TRUE, and datasets exist
if (loop == TRUE & exists("epi_data") & exists("env_data")){
  
  for (i in seq_along(wk_list)){
    
    #print to console the week running
    this_week <- wk_list[i]
    this_yr <- isoyear(this_week)
    this_wnum <- isoweek(this_week)
    print(paste0(this_week, ": ", this_yr, "W", this_wnum))
    
    this_epi_data <- epi_data_full %>%
      filter(obs_date <= this_week)
    this_env_data <- env_data_full %>%
      filter(obs_date <= this_week)
    
    #if report in past, this will automatically happen because of epi data end
    # if forecasting past existing epi data, need to set specifically 
    this_fc_start_date <- this_week + lubridate::weeks(1)
    #update report_settings (will overwrite each week)
    pfm_report_settings$fc_start_date <- this_fc_start_date
    pv_report_settings$fc_start_date <- this_fc_start_date
    
    # P. falciparum & mixed
    message("Running P. falciparum & mixed")
    pfm_reportdata <- run_epidemia(
      #data
      epi_data = this_epi_data, #this week
      env_data = this_env_data, #this week
      env_ref_data = env_ref_data, 
      env_info = env_info,
      #fields
      casefield = test_pf_tot, 
      groupfield = woreda_name, 
      populationfield = pop_at_risk,
      obsfield = environ_var_code, 
      valuefield = obs_value,
      #required settings
      fc_model_family = fc_model_family,
      #other settings
      report_settings = pfm_report_settings)
    
    # P. vivax
    message("Running P. vivax")
    pv_reportdata <- run_epidemia(
      #data
      epi_data = this_epi_data, #this week
      env_data = this_env_data, #this week
      env_ref_data = env_ref_data, 
      env_info = env_info,
      #fields
      casefield = test_pv_only, 
      groupfield = woreda_name, 
      populationfield = pop_at_risk,
      obsfield = environ_var_code, 
      valuefield = obs_value,
      #required settings
      fc_model_family = fc_model_family,
      #other settings
      report_settings = pv_report_settings)
    

    #merging pfm & pv data, save out, and create pdf
    merge_save_report(rpt_data_main = pfm_reportdata, 
                      rpt_data_secd = pv_reportdata,
                      #mark sections as P. falciparum and mixed (pfm) or P. vivax (pv)
                      # used in the epidemia_report_demo.Rnw file for the formatted report
                      var_labs = c("pfm","pv"),
                      #save out the report data in the file that the formatting file reads
                      save_file = "report/report_data.RData",
                      #save out a second copy of the report data with year and week numbers in the name file
                      second_save = TRUE, 
                      #create the pdf
                      create_report = TRUE,
                      #which Rnw file to use to create pdf
                      formatting_file = "epidemia_report_demo.Rnw",
                      #append tag to file name (optional)
                      #file_name_postfix = "multirun",
                      #show the pdf immediately after creating
                      show_report = FALSE)
    
  } #end for loop
} #end if loop


