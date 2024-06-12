library(tidyverse)
library(googledrive)
library(googlesheets4)
library(writexl)
library(dplyr)

survey2_process_data <- function(input, raw_data, drive_folder_id) {
  
  #-----------------support functions (if needed)--------------------------------
  
  
  
  
  #-----------------main processing function-----------------------------
  
  
  # average the data. function define above 
  averaged_data <- avg_data(raw_data)
  
  #define salt concentrations and their corresponding column prefixes
  salt_concentrations <- c('0mM' = 'A', '200mM' = 'B', '500mM' = 'C')
  
  #define the column ranges for each peptoid
  peptoid_column_ranges <- list('1' = 1:3, '2' = 5:7, '3' = 9:11, '4' = 1:3, '5' = 5:7, '6' = 9:11)
  
  
  
  for (i in 1:6) {
    peptoid_name <- input[[paste0("peptoid_name_", i)]]
    num_salt_conc <- input[[paste0("num_dif_salt_conc", i)]]
    file_salt_concentrations <- if (num_salt_conc == 1) c("0mM") else names(salt_concentrations)
    
    for (salt_conc in file_salt_concentrations) {
      prefix <- salt_concentrations[[salt_conc]]
      column_range <- peptoid_column_ranges[[as.character(i)]]
      if (i > 3) { prefix <- 'E' }  # For peptoids 4, 5, 6, use 'E' instead of 'A'
      
      # construct the column names for this peptoid and salt concentration
      relevant_column_names <- paste0(prefix, column_range, "_.*_avg")
      
      # find the column name that matches the pattern
      relevant_column <- grep(relevant_column_names, names(averaged_data), value = TRUE)
      
      if (length(relevant_column) == 1) {
        # filter data for the current peptoid and salt concentration
        filtered_data <- averaged_data %>%
          select(Wavelength, Absorbance = all_of(relevant_column))
        
        # construct file name based on user inputs
        file_name <- paste(peptoid_name, salt_conc, input$salt_type, input$pH_tested, input$buffer_type, sep = "_")
        file_name <- paste0(file_name, ".xlsx")
        
        # write the filtered data to an Excel file
        output_file_path <- paste0("/Users/jackpetersen/Desktop/code/AuNP-Peptoid/data", file_name)
        write_xlsx(filtered_data, output_file_path)
        
        # define the Google Drive folder IDs --- MAKE THIS BASED ON NANOPARTICLE SIZE IN FUTURE 
        data_drive_folder_id <- '16VeTz69DEirMMgS6sFvBJVqaW7Lax7cn'
        
        # upload the file to Google Drive
        drive_upload(media = output_file_path, path = as_id(data_drive_folder_id), name = file_name, overwrite = TRUE)
        
        
        
        #delete the local file after uploading
        if (file.exists(output_file_path)) {
          file.remove(output_file_path)
        }
        
        
        print(file_name)
        
        
      } else {
        print(paste0("No data found for ", peptoid_name, " at ", salt_conc))
      }
      
      
    }
  } #end for 
} #end survey2_process_data()








