# All the modeling, forecast and event detection parameters

# Set up forecast controls ---------------------------------------------

#total number of weeks in report (including forecast period)
report_period <- 26

#forecast 8 weeks into the future
forecast_future <- 8

#report out in incidence
pfm_value_type <-  "incidence"
pv_value_type <-  "incidence"

#report incidence rates per 1000 people
inc_per <- 1000

#Model choice and parameters
pfm_model_choice <- "poisson-bam"
pv_model_choice <- "poisson-bam"

#read in model environmental variables to use
pfm_model_env <- read_csv("data/falciparum_model_envvars.csv", col_types = cols())
pv_model_env <- read_csv("data/vivax_model_envvars.csv", col_types = cols())

#environmental data should be transformed to anomalies
anom_env_var <- TRUE

#read in model cluster information
pfm_clusters <- read_csv("data/falciparum_model_clusters.csv", col_types = cols())
pv_clusters <- read_csv("data/vivax_model_clusters.csv", col_types = cols())

#set maximum environmental lag length (in days)
lag_length <- 181

#model fit frequency: fit once ("once), or fit every week ("week")
fit_freq <- "once"

#set number of cores to use for parallel processing (nthreads in bam discretization)
#default value is the number of physical cores minus 1, minimum 1 core.  Can be set to different here.
cores <- max(detectCores(logical=FALSE) - 1, 1)

#make control lists
pfm_fc_control <- list(env_vars = pfm_model_env,
                       anom_env = anom_env_var,
                       clusters = pfm_clusters,
                       lag_length = lag_length,
                       value_type = pfm_value_type,
                       fit_freq = fit_freq,
                       ncores = cores)

pv_fc_control <- list(env_vars = pv_model_env,
                      anom_env = anom_env_var,
                      clusters = pv_clusters,
                      lag_length = lag_length,
                      value_type = pv_value_type,
                      fit_freq = fit_freq,
                      ncores = cores)


# Set up early detection controls ---------------------------------------

#number of weeks in early detection period 
# (last n weeks of known epidemiological data to summarize alerts)
ed_summary_period <- 4

#event detection algorithm
ed_method <- "Farrington"

#settings for Farrington event detection algorithm
pfm_ed_control <- list(
  w = 3, reweight = TRUE, weightsThreshold = 2.58,
  trend = TRUE, pThresholdTrend = 0,
  populationOffset = TRUE,
  noPeriods = 12, pastWeeksNotIncluded = 4,
  thresholdMethod = "nbPlugin")

pv_ed_control <- list(
  w = 4, reweight = TRUE, weightsThreshold = 2.58,
  trend = TRUE, pThresholdTrend = 0,
  populationOffset = TRUE,
  noPeriods = 10, pastWeeksNotIncluded = 4,
  thresholdMethod = "nbPlugin")
