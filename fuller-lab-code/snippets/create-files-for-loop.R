library(googledrive)

#list of measurements to process
measurements <- colnames(data)[-1]

for (measurement in measurements) {
  
  #filter data for the current measurement and make the column "absorbance" (the original column name will be used as the file name)
  filtered_data <- data %>% select(Wavelength, Absorbance = all_of(measurement))
  
  #create a file name (same as original column name)
  output_file_name <- paste0(measurement)
  
  #write the filtered data to an Excel file
  write_xlsx(filtered_data, output_file_name)
  
  #OPTIONAL: upload Excel file to Google Drive. make explicit definitions of media, path, and name to avoid errors. NEED GOOGLE DRIVE LIBRARY
  drive_upload(
    media = output_file_name, 
    path = as_id(folder_id), 
    name = output_file_name
  )
}
