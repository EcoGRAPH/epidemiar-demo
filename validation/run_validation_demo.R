# ###############################################################################################
#
# This script runs validation metrics on the EPIDEMIA modeling and forecasting for a 
# user-specified start week, time range, and n-weeks ahead predictions. 
#
# The user must select in Section 1, the starting week, number of weeks, and weeks ahead to
# run validation (number of weeks to forecast into the future).
# The user can also assume a delay in reporting with reporting_lag. 
# Skill test will compare against naive models and generate skill scores. This must be run 
#     to generate the formatted pdf report.
#
# NOTE: Because these are creating forecasts from each week in the validation period,
#  these will take some time to run. 
#
# See documentation/epidemia_howto_run_validation.pdf for more details. 
#
# ###############################################################################################


# 1. Validation settings -----------------------------------------------------

# This is the week to start the validation from
date_start <- epidemiar::make_date_yw(year = 2018, week = 1, weekday = 7)

# This is the number of weeks from the start to run validation through.
total_timesteps <- 52

# This is the number of weeks ahead (1 through this number) to validate.
timesteps_ahead <- 12

# This is the number of weeks of assumed delay in reporting
reporting_lag  <- 0

# This will compare against naive, or very simple, models
# Must be TRUE to generate the formatted pdf reports
skill_test <- TRUE 


# 2. Libraries & Functions ------------------------------------------------------

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

# 3. Reading in the Data -----------------------------------------------------

# read in woreda metadata
report_woredas <- read_csv("data/amhara_woredas.csv") %>% 
  filter(report == 1)

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

# read in forecast and modeling parameters
source("data/epidemiar_settings_demo.R")


# 4. Validation P. falciparum ------------------------------------------------------

# Because these are creating forecasts from each week in the validation period,
#  these will take some time to run. 

# P. falciparum & mixed
if (exists("epi_data") & exists("env_data")){
  
  message("Running P. falciparum & mixed")
  pfm_validation <- run_validation(date_start = date_start, 
                                   total_timesteps = total_timesteps, 
                                   timesteps_ahead = timesteps_ahead, 
                                   skill_test = skill_test,
                                   reporting_lag = reporting_lag, 
                                   #copy of run_epidemia() parameters 
                                   #data
                                   epi_data = epi_data, 
                                   env_data = env_data, 
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
} else {
  message("Error: Epidemiological and/or environmental datasets are missing.
          Check Section 3 for data error messages.")
}

# saves the report data, creates and saves the pdf report
create_validation_report(pfm_validation)


# 5. Validation P. vivax ------------------------------------------------------

# Because these are creating forecasts from each week in the validation period,
#  these will take some time to run. 

# P. vivax
if (exists("epi_data") & exists("env_data")){
  
  message("Running P. vivax")
  pv_validation <- run_validation(date_start = date_start, 
                                  total_timesteps = total_timesteps, 
                                  timesteps_ahead = timesteps_ahead, 
                                  skill_test = skill_test,
                                  reporting_lag = reporting_lag, 
                                  #copy of run_epidemia() parameters 
                                  #data
                                  epi_data = epi_data, 
                                  env_data = env_data, 
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
} else {
  message("Error: Epidemiological and/or environmental datasets are missing.
          Check Section 3 for data error messages.")
}

# saves the report data, creates and saves the pdf report
create_validation_report(pv_validation)
