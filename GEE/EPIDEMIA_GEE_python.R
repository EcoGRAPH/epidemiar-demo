# #####################################################################
#
# This script can be used to request environmental data from GEE
#   by going through the R package reticulate to a 
#   custom python package, epidemia-gee, and requesting our custom 
#   processed summarized daily data to be downloaded to a Google Drive.
#
# Please see the install instructions for epidemia-gee, including
#   Anaconda and set-up here:
#
#        https://github.com/EcoGRAPH/epidemia-gee/releases/latest
#
#
# The python package is built around a national data set, and 
#   the environmental data cannot to used directly in this project.
# However, it was included to show how this set up works.
#
# #####################################################################

#load packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(reticulate)

#use the conda environment we set up earlier in Anaconda
use_condaenv("gee-demo", conda = "auto", required = TRUE)

#import the Earth Engine library
ee <- import("ee")          
#authenticate
ee$Initialize()

#import the epidemia-gee package
eth_gee <- import("Ethiopia")  

#Now we have access to the gee_to_drive() function
#   which accepts a start and end date
#   and requests our daily summarized data for that range.
# The resulting .csv file will be downloaded to an "Ethiopiadata" folder 
#   in the Google Drive of the authenticated account.

#example 1: start date of Jan 1, 2009 & end date of Feb 1, 2009
eth_gee$Et$gee_to_drive('2009-01-01','2009-02-01')  

#example 2: start date of April 23, 2019 & end date of June 1, 2020
eth_gee$Et$gee_to_drive('2019-04-23','2020-06-01') 


