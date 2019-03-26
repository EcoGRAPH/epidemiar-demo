This is a R project for creating demo EPIDEMIA forecasting reports using the epidemiar package. The final report presents a malaria forecasting report for 47 woredas in the Amhara region for the past 18 weeks through 8 weeks forecasted into the future (26 total weeks). Malaria is broken out by species: _Plasmodium falciparum_ and mixed species, and also _P. vivax_. The report includes environmental and epidemiological surveillance data.

Demo is based off a real implementation for forecasting malaria in the Amhara region of Ethiopia, however surveillance data in this demo have been *simulated*.

Open `epidemiar_demo.RProj`, and the main script to open and run is `run_epidemiar_demo.R`.

Many of the customized scripts are based around issues in the real situation which do not exist in the simulated data, but are retained in the code here for demonstration on how it could work (e.g. data cleaning functions inside data corral functions for collating surveillance data). 

See walkthrough and other documentation in this project under the `documentation` folder, specifically `walkthrough.pdf`. 

This demo is to be used alongside the package `epidemiar`, which you can find here: https://github.com/EcoGRAPH/epidemiar. 
