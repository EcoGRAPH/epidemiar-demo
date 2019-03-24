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


# 1. Libraries & Functions ------------------------------------------------------

## Load packages necessary for script

#make sure pacman is installed
if (!require("pacman")) install.packages("pacman")

#load packages
  #note: data corrals use dplyr, tidyr, lubridate, readr, readxl
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
library(epidemiar)

## Locally-defined Functions
source("R/date_functions.R")
source("R/data_corrals.R") 
source("R/report_save_create_helpers.R")

# 2. Reading in the Data -----------------------------------------------------

# read in woreda metadata
report_woredas <- readxl::read_xlsx("data/woredas.xlsx", na = "NA") %>% 
  dplyr::filter(report == 1)
  #Note: report woredas must have sufficient epi data, env data, and model cluster information, in appropriate files

# read & process case data needed for report
am_epi_data <- corral_epidemiological(report_woreda_names = report_woredas$woreda_name)

# read & process environmental data for woredas in report
am_env_data <- corral_environment(report_woredas = report_woredas)

# read in climatology / environmental reference data
am_env_ref_data <- read_csv("data/env_ref_data.csv", col_types = cols())

# read in environmental info file
am_env_info <- read_xlsx("data/environ_info.xlsx", na = "NA")


# 3. Set up Forecast controls ---------------------------------------------

#total number of weeks in report (including forecast period)
am_report_period <- 26

# forecast 8 weeks into the future
am_forecast_future <- 8

#read in model environmental variables to use
am_pfm_model_env <- read_csv("data/falciparum_model_envvars.csv", col_types = cols())
am_pv_model_env <- read_csv("data/vivax_model_envvars.csv", col_types = cols())

#read in model cluster information
am_pfm_clusters <- read_csv("data/falciparum_model_clusters.csv", col_types = cols())
am_pv_clusters <- read_csv("data/vivax_model_clusters.csv", col_types = cols())

#set maximum environmental lag length (in days)
am_lag_length <- 181

#model fit frequency: fit once ("once), or fit every week ("week")
am_fit_freq <- "once"

#set number of cores to use on computer for parallel processing
#default value is the number of physical cores minus 1, minimum 1 core.  Can be set to different here.
am_cores <- max(detectCores(logical=FALSE) - 1, 1)

#make control lists
am_pfm_fc_control <- list(env_vars = am_pfm_model_env,
                          clusters = am_pfm_clusters,
                          lag_length = am_lag_length,
                          fit_freq = am_fit_freq,
                          ncores = am_cores)

am_pv_fc_control <- list(env_vars = am_pv_model_env,
                         clusters = am_pv_clusters,
                         lag_length = am_lag_length,
                         fit_freq = am_fit_freq,
                         ncores = am_cores)


# 4. Set up Early Detection controls ---------------------------------------

#number of weeks in early detection period (last n weeks of known epidemiological data to summarize alerts)
am_ed_summary_period <- 4

#settings for Farrington event detection algorithm
am_pfm_ed_control <- list(
  w = 3, reweight = TRUE, weightsThreshold = 2.58,
  trend = TRUE, pThresholdTrend = 0,
  populationOffset = TRUE,
  noPeriods = 12, pastWeeksNotIncluded = 4,
  thresholdMethod = "nbPlugin")

am_pv_ed_control <- list(
  w = 4, reweight = TRUE, weightsThreshold = 2.58,
  trend = TRUE, pThresholdTrend = 0,
  populationOffset = TRUE,
  noPeriods = 10, pastWeeksNotIncluded = 4,
  thresholdMethod = "nbPlugin")


# 5. Run epidemia & create report data ---------------------------------------

#Run modeling to get report data
# with check on current epidemiology and environmental data sets

if (exists("am_epi_data") & exists("am_env_data")){
  
  # P. falciparum & mixed
  message("Running P. falciparum & mixed")
  pfm_reportdata <- run_epidemia(epi_data = am_epi_data, 
                                 casefield = test_pf_tot, 
                                 populationfield = pop_at_risk,
                                 #incidence rates per 1000
                                 inc_per = 1000,
                                 groupfield = woreda_name, 
                                 week_type = "ISO",
                                 report_period = am_report_period, 
                                 ed_summary_period = am_ed_summary_period,
                                 ed_method = "Farrington", 
                                 ed_control = am_pfm_ed_control,
                                 env_data = am_env_data, 
                                 obsfield = environ_var_code, 
                                 valuefield = obs_value, 
                                 forecast_future = am_forecast_future, 
                                 fc_control = am_pfm_fc_control,
                                 env_ref_data = am_env_ref_data, 
                                 env_info = am_env_info)
  
  # P. vivax
  message("Running P. vivax")
  pv_reportdata <- run_epidemia(epi_data = am_epi_data, 
                                casefield = test_pv_only, 
                                populationfield = pop_at_risk,
                                #incidence rates per 1000
                                inc_per = 1000,
                                groupfield = woreda_name, 
                                week_type = "ISO",
                                report_period = am_report_period, 
                                ed_summary_period = am_ed_summary_period,
                                ed_method = "Farrington", 
                                ed_control = am_pfm_ed_control,
                                env_data = am_env_data, 
                                obsfield = environ_var_code, 
                                valuefield = obs_value, 
                                forecast_future = am_forecast_future, 
                                fc_control = am_pv_fc_control,
                                env_ref_data = am_env_ref_data, 
                                env_info = am_env_info)
  
} else {
  message("Error: Epidemiological and/or environmental datasets are missing.
          Check Section 2 for data error messages.")
}


# 6. Merge species data, Save, & Create PDF Report -------------------------------

if (exists("pfm_reportdata") & exists("pv_reportdata")){
  
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
                    show_report = TRUE)
  
} else {
  message("Error: Report data for P. falciparum and/or P. vivax have not been generated and are missing.")
}


# Alternative: Create pdf report ----------------------------------------------------------

# If you want to later recreate a pdf from a saved report_data file:
# Change the input report_data_file to the previously saved version
# And add a file/name for the saved output.

# create_pdf(new_data = "report/report_data_2018W52.RData",
#            #file that is loaded in the formatting file
#            report_data_file = "report/report_data.RData",
#            formatting_file = "epidemia_report_demo.Rnw",
#            #specific output file name
#            report_save_file = "report/report_data_2018W52.pdf",
#            show = TRUE)

