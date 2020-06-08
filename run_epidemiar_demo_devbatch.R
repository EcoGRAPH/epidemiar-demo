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

## <<RAM>> These would take you into debugging mode with an R browser
# They are useful if you need to step through each and every code line internally
# You run debugonce(function) and then the next time you run the function 
# it'll jump into debug mode
#debugonce(epidemiar::run_epidemia)
#debugonce(clusterapply::batch_bam)

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
report_woredas <- read_csv("data/amhara_woredas.csv") %>% 
  filter(report == 1)
#Note: report woredas must have sufficient epi data, env data, and model cluster information, in appropriate files

# read & process case data needed for report
epi_data <- corral_epidemiological(report_woreda_names = report_woredas$woreda_name)

# read & process environmental data for woredas in report
env_data <- corral_environment(report_woredas = report_woredas)

## <<RAM>> after running the corral_environment() once and making sure it works
# you can switch to the below import instead, which will be a LOT faster
# the demo version has a lot of historical data because I also show how to build
# a climatology/reference dataset in the documentation
#env_data <- readRDS("data_environmental/env_data_2012_2018.RDS")


# read in climatology / environmental reference data
env_ref_data <- read_csv("data/env_ref_data_2002_2018.csv", col_types = cols())

# read in environmental info file
env_info <- read_xlsx("data/environ_info.xlsx", na = "NA")

# read in forecast and event detection parameters
source("data/epidemiar_settings_amhara.R")

## <<RAM>> Thin plate will currently not run all the way through until prediction is changed
# But if you wanted to toy around with debug mode on either/both functions and see 
# what happens step by step, that might be interesting for you. 
#update fc_splines
pfm_report_settings$fc_splines <- "modbs"
pv_report_settings$fc_splines <- "modbs"

# pfm_report_settings$fc_splines <- "tp"
# pv_report_settings$fc_splines <- "tp"


# 3. Run epidemia & create report data ---------------------------------------

## <<RAM>> if you need to do a quick test, you can pull pfm out by itself,
#because the code below will run P. falciparum and P. vivax

#Run modeling to get report data
# with check on current epidemiology and environmental data sets

if (exists("epi_data") & exists("env_data")){
  
  # P. falciparum & mixed
  message("Running P. falciparum & mixed")
  pfm_reportdata <- run_epidemia(
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
  pv_reportdata <- run_epidemia(
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
  
  #if using cached models:
  #append model information to report data metadata
  if (exists('pfm_model_cached')){
    pfm_reportdata$params_meta$model_used <- latest_pfm_model
  }
  if (exists('pv_model_cached')){
    pv_reportdata$params_meta$model_used <- latest_pv_model
  }
  
} else {
  message("Error: Epidemiological and/or environmental datasets are missing.
          Check Section 2 for data error messages.")
}


# 4. Merge species data, Save, & Create PDF Report -------------------------------

if (exists("pfm_reportdata") & exists("pv_reportdata")){
  
  #merging pfm & pv data, save out, and create pdf
  merge_save_report(rpt_data_main = pfm_reportdata,
                    rpt_data_secd = pv_reportdata,
                    #mark sections as P. falciparum and mixed (pfm) or P. vivax (pv)
                    # used in the epidemia_report_demo.Rnw file for the formatted report
                    var_labs = c("pfm","pv"),
                    #save out the report data in the file that the formatting file reads
                    save_file = "report/report_data.RData",
                    #save out a second copy of the report data with year and week numbers in the name
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
#            report_save_file = "report/epidemia_report_demo_2018W52.pdf",
#            show = TRUE)


