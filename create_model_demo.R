# ###############################################################################################
#
# This script creates only the MODEL that can be used to generate
# a weekly EPIDEMIA forecast report for synthetic malaria data 
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

#due to experimental dplyr::summarise() parameter
options(dplyr.summarise.inform=F)

# 2. Reading in the Data -----------------------------------------------------

# read in woreda metadata
report_woredas <- read_csv("data/amhara_woredas.csv") %>% 
  filter(report == 1)
# report_woredas <- readxl::read_xlsx("data/woredas.xlsx", na = "NA") %>% 
#   dplyr::filter(report == 1)
#Note: report woredas must have sufficient epi data, env data, and model cluster information, in appropriate files

# read & process case data needed for report
epi_data <- corral_epidemiological(report_woreda_names = report_woredas$woreda_name)

# read & process environmental data for woredas in report
env_data <- corral_environment(report_woredas = report_woredas)

## Optional: For slight speed increase, 
# date filtering to remove older environmental data.
# older env data was included to demo epidemiar::env_daily_to_ref() function.
#week is always end of the week, 7th day
env_start_date <- epidemiar::make_date_yw(year = 2012, week = 1, weekday = 7) 
env_data <- env_data %>%
filter(obs_date >= env_start_date)

# read in climatology / environmental reference data
env_ref_data <- readr::read_csv("data/env_ref_data_2002_2018.csv", col_types = readr::cols())

# read in environmental info file
env_info <- read_xlsx("data/environ_info.xlsx", na = "NA")

# read in forecasting and report settings file
source("data/epidemiar_settings_demo.R")

# 3. Run epidemia & create model only ---------------------------------------

#UPDATE model run to TRUE
pfm_report_settings$model_run <- TRUE
pv_report_settings$model_run <- TRUE

#Run with check on current epidemiology and environmental data sets

if (exists("epi_data") & exists("env_data")){
  
  # P. falciparum & mixed
  message("Running P. falciparum & mixed")
  pfm_model <- run_epidemia(
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
  
  # P. vivax
  message("Running P. vivax")
  pv_model <- run_epidemia(
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
          Check Section 2 for data error messages.")
}

# model object:
# $model_obj is regression object
# $model_info is a log of parameters and data ranges used to generate model


# 4. Save models for later use ---------------------------------------------

# add last epidemiological known data date, and today's date to file name
save_filetail <- paste0("_", isoyear(max(epi_data$obs_date)), 
                        "W", isoweek(max(epi_data$obs_date)),
                        "_", format(Sys.Date(), "%Y%m%d"))
pfm_name <- paste0("pfm_model", save_filetail, ".RDS")
pv_name <- paste0("pv_model", save_filetail, ".RDS")

#save to /data
saveRDS(pfm_model, file.path("data/models", pfm_name))
saveRDS(pv_model, file.path("data/models", pv_name))

