# ###############################################################################################
#
# This script runs validation metrics on the EPIDEMIA modeling and forecasting for a 
# user-specified start week, time range, and n-weeks ahead predictions. 
#
# See documentation/run_validation.pdf for more details. 
#
# ###############################################################################################


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

# 2. Reading in the Data -----------------------------------------------------

# read in woreda metadata
report_woredas <- readxl::read_xlsx("data/woredas.xlsx", na = "NA") %>% 
  dplyr::filter(report == 1)
#Note: report woredas must have sufficient epi data, env data, and model cluster information, in appropriate files

# read & process case data needed for report
epi_data <- corral_epidemiological(report_woreda_names = report_woredas$woreda_name)

# read & process environmental data for woredas in report
env_data <- corral_environment(report_woredas = report_woredas)

# read in climatology / environmental reference data
env_ref_data <- read_csv("data/env_GEE_ref_data.csv", col_types = cols())

# read in environmental info file
env_info <- read_xlsx("data/environ_info.xlsx", na = "NA")

# read in forecast and event detection parameters
source("data/model_parameters_amhara.R")



# 3. Validation settings -----------------------------------------------------

# This is the week to start the validation from
week_start <- epidemiar::make_date_yw(year = 2018, week = 26, weekday = 7)

# This is the number of weeks from the start to run validation through.
total_weeks <- 8

# This is the number of weeks ahead (1 through this number) to validate.
weeks_ahead <- 6

# This will compare against naive, or very simple, models
skill_test <- TRUE #FALSE


# Running Validation ---------------------------------------------------------

# Because these are creating forecasts from each week in the validation period,
#  these will take some time to run. 
# The different species have separated, so they can be run individually.

# Note: Model caching can be included here as well. 
# The model that is fed into model_cached will be used for all weeks in validation.

# P. falciparum & mixed
if (exists("epi_data") & exists("env_data")){
  
  message("Running P. falciparum & mixed")
  pfm_validation <- run_validation(week_start = week_start, 
                                   total_weeks = total_weeks, 
                                   weeks_ahead = weeks_ahead, 
                                   skill_test = skill_test,
                                   #copy of run_epidemia() parameters 
                                   epi_data = epi_data, 
                                   casefield = test_pf_tot, 
                                   populationfield = pop_at_risk,
                                   inc_per = inc_per,
                                   groupfield = woreda_name, 
                                   week_type = "ISO",
                                   report_period = report_period, 
                                   ed_summary_period = ed_summary_period,
                                   ed_method = ed_method, 
                                   ed_control = pfm_ed_control,
                                   env_data = env_data, 
                                   obsfield = environ_var_code, 
                                   valuefield = obs_value, 
                                   forecast_future = forecast_future, 
                                   fc_control = pfm_fc_control,
                                   env_ref_data = env_ref_data, 
                                   env_info = env_info,
                                   model_choice = pfm_model_choice)
} else {
  message("Error: Epidemiological and/or environmental datasets are missing.
          Check Section 2 for data error messages.")
}


# P. vivax
if (exists("epi_data") & exists("env_data")){
  
  message("Running P. vivax")
  pv_validation <- run_validation(week_start = week_start, 
                                  total_weeks = total_weeks, 
                                  weeks_ahead = weeks_ahead, 
                                  skill_test = skill_test,
                                  #copy of run_epidemia() parameters
                                  epi_data = epi_data, 
                                  casefield = test_pv_only, 
                                  populationfield = pop_at_risk,
                                  inc_per = inc_per,
                                  groupfield = woreda_name, 
                                  week_type = "ISO",
                                  report_period = report_period, 
                                  ed_summary_period = ed_summary_period,
                                  ed_method = ed_method, 
                                  ed_control = pv_ed_control,
                                  env_data = env_data, 
                                  obsfield = environ_var_code, 
                                  valuefield = obs_value, 
                                  forecast_future = forecast_future, 
                                  fc_control = pv_fc_control,
                                  env_ref_data = env_ref_data, 
                                  env_info = env_info,
                                  model_choice = pv_model_choice)
  
} else {
  message("Error: Epidemiological and/or environmental datasets are missing.
          Check Section 2 for data error messages.")
}


# Viewing & saving validation results -------------------------------------

# View the overall model validations 
view_overall_validations(pfm_validation)
view_overall_validations(pv_validation)



#Saving files
validation_filename_ending <- paste0("_start", format(week_start, "%Y%m%d"),
                                     "_", total_weeks, "weeks",
                                     "_", weeks_ahead, "ahead", ".csv")

# Save the overall validation to file
# Change the file name to better identify the validation settings you have used
save_overall_validations(pfm_validation,
                         save_file = paste0("pfm_validation", validation_filename_ending))
save_overall_validations(pv_validation,
                         save_file = paste0("pv_validation", validation_filename_ending))


# Save the validations per woreda to file
# Change the file name to better identify the validation settings you have used
save_geog_validations(pfm_validation,
                      save_file = paste0("pfm_validation_woreda", validation_filename_ending))

save_geog_validations(pv_validation,
                      save_file = paste0("pv_validation_woreda", validation_filename_ending))

