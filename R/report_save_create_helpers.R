# Function to combine P. falciparum and P. vivax model runs together, to generate one report
#merging pfm & pv data at end
merge_save_report <- function(rpt_data_main = NULL, 
                              rpt_data_secd = NULL, 
                              var_labs = NULL,
                              save_reg = FALSE,
                              save_file = "report/report_data.RData",
                              second_save = TRUE,
                              create_report = TRUE,
                              formatting_file = "epidemia_report_demo.Rnw",
                              show_report = TRUE){
  #accept epidemia outputs for 2 report_data over same time range and merges
  #using main results to drive which environmental variables to show
  
  if(save_reg){
    regobj_main <- rpt_data_main$regression_object
    if(!is.null(rpt_data_secd)){
      regobj_secd <- rpt_data_secd$regression_object
    }
  } else {
    regobj_main <- NULL
    regobj_secd <- NULL
  }
  
  ## If single report data, then just save out
  if(is.null(rpt_data_secd)){
    save(rpt_data_main$summary_data, 
         rpt_data_main$epi_summary, 
         rpt_data_main$modeling_results_data, 
         rpt_data_main$environ_timeseries, 
         rpt_data_main$environ_anomalies, 
         rpt_data_main$params_meta,
         regobj_main,
         file = save_file)
    message(paste0("Saved results to /", save_file))
  } 
  
  
  ## For two data reports:
  
  ## Check same time ranges
  if (!all.equal(rpt_data_main$params_meta$report_dates, 
                 rpt_data_secd$params_meta$report_dates)){
    stop("Reports were not run with the same report date ranges")
  }
  
  ## Merge with new respon_var column
  #summary results
  summary_main <- rpt_data_main$summary_data %>% 
    mutate(respon_var = var_labs[1])
  summary_secd <- rpt_data_secd$summary_data %>% 
    mutate(respon_var = var_labs[2])
  summary_data <- bind_rows(summary_main, summary_secd)
  
  #modeling results
  mod_main <- rpt_data_main$modeling_results_data %>% 
    mutate(respon_var = var_labs[1])
  mod_secd <- rpt_data_secd$modeling_results_data %>% 
    mutate(respon_var = var_labs[2])
  modeling_results_data <- bind_rows(mod_main, mod_secd)
  
  #epidemiology results
  epi_main <- rpt_data_main$epi_summary %>% 
    mutate(respon_var = var_labs[1])
  epi_secd <- rpt_data_secd$epi_summary %>% 
    mutate(respon_var = var_labs[2])
  epi_summary <- bind_rows(epi_main, epi_secd)
  
  ## Use Main results for Environmental timeseries and anomalies
  environ_timeseries <- rpt_data_main$environ_timeseries
  environ_anomalies <- rpt_data_main$environ_anomalies
  
  ## Use Main results as base for params_meta
  params_meta <- rpt_data_main$params_meta
  # but keep others
  params_meta[[var_labs[2]]][["params_meta"]] <- rpt_data_secd$params_meta
  
  ## Keep both regression objects
  regression_object <- c(regobj_main, regobj_secd)
  
  ##Save out
  save(summary_data, epi_summary, modeling_results_data, 
       environ_timeseries, environ_anomalies, params_meta, regression_object,
       file = save_file)
  message(paste0("Saved results to /", save_file))
  
  #Optional second save
  if (second_save){
    save_filetail <- paste0("_", isoyear(rpt_data_main$params_meta$report_dates$known$max), 
                            "W", isoweek(rpt_data_main$params_meta$report_dates$known$max))
    save_name <- file_path_sans_ext(save_file)
    save_ext <- file_ext(save_file)
    second_save_file <- paste0(save_name, save_filetail, ".", save_ext)
    
    file.copy(from = save_file,
              to = second_save_file, 
              overwrite = TRUE)
    
    message(paste0("Saved results to /", second_save_file))
  }
  
  
  ## Creating report
  if (create_report){
    #generate file name
    report_filetail <- paste0("_", isoyear(rpt_data_main$params_meta$report_dates$known$max), 
                              "W", isoweek(rpt_data_main$params_meta$report_dates$known$max))
    report_output_file <- paste0("report/",
                                 file_path_sans_ext(formatting_file),
                                 report_filetail, ".pdf")
    
    create_pdf(new_data = save_file, 
               formatting_file = formatting_file, 
               report_save_file = report_output_file,
               show = show_report,
               skip_check = FALSE)
  }
  
}

# Function to compile pdf from rnw from script
# Reference: https://stackoverflow.com/questions/34591487/difference-compile-pdf-button-in-rstudio-vs-knit-and-knit2pdf
create_pdf <- function(new_data = "report/report_data.RData",
                       #whichever file is called in the formatting file (nearly unalterable by function call, so coded here)
                       report_data_file = "report/report_data.RData",
                       formatting_file = "epidemia_report_demo.Rnw",
                       report_save_file = NULL,
                       show = TRUE,
                       skip_check = FALSE){
  
  if (!skip_check & new_data != report_data_file){
    #warning about overwrite
    msg_txt <- paste0("This will overwrite '", report_data_file, "' with ", new_data, 
                      ".\nDo you want to proceed?")
    continue_ans <- menu(c("Yes", "No"), title = msg_txt)
  } else continue_ans <- 1 #skipping check
  
  if (continue_ans == 1){ #Yes == 1
    
    if (new_data != report_data_file){
      #save report_data file as name that Rnw loads
      file.copy(from = new_data,
                to = report_data_file,
                overwrite = TRUE)
    }
    
    # Compile the pdf, setting working directory specifically for knit run
    setwd("report")
    knitcall <- paste0("library(knitr); knit2pdf('", formatting_file, "')")
    
    #tryCatch to keep pdf generation and saving to new file together
    tryCatch({
      #new R session for clean environment
      system2("Rscript", c("-e", shQuote(knitcall)))
      setwd("..")
      
      #this is the format of the automatically generated pdf by the Rnw file
      base_output <- paste0("report/", file_path_sans_ext(formatting_file), ".pdf") 
      
      #if saving to a specific file name
      if (exists("report_save_file")){
        #check that original pdf was created
        file.copy(from = base_output,
                  to = report_save_file,
                  overwrite = TRUE)
        message(paste0("Saved report to /", report_save_file))
      } #end if exist report_save_file   
    }, error = function(e){
      stop("Error generating and saving pdf report.")
    })
  
  if (show){
    if (exists("report_save_file")){
      open_call <- paste0('open "', report_save_file, '"')
    } else open_call <- paste0('open "', base_output, '"')
    #open
    system(open_call)
  } #end if show
  
} #end continue_ans == 1

} #end create_pdf


