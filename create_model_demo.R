# -----------------------------------------------------------------------------
#
#
# This script creates only the MODEL that can be used to generate
# a weekly EPIDEMIA forecast report for synthetic malaria data 
#
#

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


# 3. Run epidemia & create model only ---------------------------------------

#Run modeling to get report data
# with check on current epidemiology and environmental data sets

if (exists("epi_data") & exists("env_data")){
  
  # P. falciparum & mixed
  message("Running P. falciparum & mixed")
  pfm_model <- run_epidemia(epi_data = epi_data, 
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
                            model_run = TRUE)
  
  # P. vivax
  message("Running P. vivax")
  pv_model <- run_epidemia(epi_data = epi_data, 
                           casefield = test_pv_only, 
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
                           fc_control = pv_fc_control,
                           env_ref_data = env_ref_data, 
                           env_info = env_info,
                           model_run = TRUE)
  
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

