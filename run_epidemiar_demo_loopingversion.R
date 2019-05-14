# ###############################################################################################
#
# This script does all the data collating and processing before calling the epidemiar function
# to run the modeling, forecasting, and early detection and early warning alert algorithms. 
#
# At the end, this script will generate a pdf report using epidemia_report_demo.Rnw
#
# See documentation/walkthrough.pdf for more details on each step. 
#
# ###############################################################################################

# 0. Looping version ------------------------------------------------------
#
# This version of the script can be used to loop through multiple weeks to generate reports for each.
#
# Set the loop variable to TRUE, and change the isoyear and isoweeks wanted. 
#

loop <- TRUE
wk_list <- c(epidemiar::make_date_yw(year = 2016, week = c(23:24), weekday = 7),
             epidemiar::make_date_yw(year = 2017, week = c(37:38), weekday = 7))


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

# ## Optional: Date Filtering for running certain week's report
# req_date <- epidemiar::make_date_yw(year = 2018, week = 52, weekday = 7) #week is always end of the week, 7th day
# epi_data <- epi_data %>% 
#   filter(obs_date <= req_date)
# env_data <- env_data %>% 
#   filter(obs_date <- req_date)

# read in climatology / environmental reference data
env_ref_data <- read_csv("data/env_GEE_ref_data.csv", col_types = cols())

# read in environmental info file
env_info <- read_xlsx("data/environ_info.xlsx", na = "NA")

# read in forecast and event detection parameters
source("data/model_parameters_amhara.R")

# read in latest model to use - select model per species with latest file created time
# if you are running historical reports, make sure the model used makes sense
# pfm
all_pfm_models <- file.info(list.files("data/models/", full.names = TRUE, pattern="^pfm.*\\.RDS$"))
if (nrow(all_pfm_models) > 0){
  latest_pfm_model <- rownames(all_pfm_models)[which.max(all_pfm_models$ctime)]
  pfm_model_obj <- readRDS(latest_pfm_model)$model_obj
} else { latest_pfm_model <- ""; pfm_model_obj <- NULL }
#or select specific file
#latest_pfm_model <- "data/pfm_model_xxxxxxx.RDS"
#pfm_model_obj <- readRDS(latest_pfm_model)$model_obj

#pv
all_pv_models <- file.info(list.files("data/models/", full.names = TRUE, pattern="^pv.*\\.RDS$"))
if (nrow(all_pv_models) > 0){
  latest_pv_model <- rownames(all_pv_models)[which.max(all_pv_models$ctime)]
  pv_model_obj <- readRDS(latest_pv_model)$model_obj
} else { latest_pv_model <- ""; pv_model_obj <- NULL}
#or select specific model
#latest_pv_model <- "data/pv_model_xxxxxxxx.RDS"
#pv_model_obj <- readRDS(latest_pv_model)$model_obj


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
  
    # P. falciparum & mixed
    message("Running P. falciparum & mixed")
    pfm_reportdata <- run_epidemia(epi_data = this_epi_data, # 
                                   casefield = test_pf_tot, 
                                   populationfield = pop_at_risk,
                                   inc_per = inc_per,
                                   groupfield = woreda_name, 
                                   week_type = "ISO",
                                   report_period = report_period, 
                                   ed_summary_period = ed_summary_period,
                                   ed_method = ed_method, 
                                   ed_control = pfm_ed_control,
                                   env_data = this_env_data, #
                                   obsfield = environ_var_code, 
                                   valuefield = obs_value, 
                                   forecast_future = forecast_future, 
                                   fc_control = pfm_fc_control,
                                   env_ref_data = env_ref_data, 
                                   env_info = env_info,
                                   model_obj = pfm_model_obj)
    
    # P. vivax
    message("Running P. vivax")
    pv_reportdata <- run_epidemia(epi_data = this_epi_data, #
                                  casefield = test_pv_only, 
                                  populationfield = pop_at_risk,
                                  inc_per = inc_per,
                                  groupfield = woreda_name, 
                                  week_type = "ISO",
                                  report_period = report_period, 
                                  ed_summary_period = ed_summary_period,
                                  ed_method = ed_method, 
                                  ed_control = pv_ed_control,
                                  env_data = this_env_data, #
                                  obsfield = environ_var_code, 
                                  valuefield = obs_value, 
                                  forecast_future = forecast_future, 
                                  fc_control = pv_fc_control,
                                  env_ref_data = env_ref_data, 
                                  env_info = env_info,
                                  model_obj = pv_model_obj)
    
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
                    #show the pdf immediately after creating
                    show_report = FALSE)
  
  } #end for loop
} #end if loop


