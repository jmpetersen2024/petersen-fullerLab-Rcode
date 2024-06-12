#loop through the columns in sets of three, averaging and adding to the data set 
for (i in seq(1, length(column_names), by = 3)) {
  
  #ensure we have a group of three columns
  if (i + 2 <= length(column_names)) {
    
    #group 3 columns together 
    cols_to_average <- column_names[i:(i+2)]
    
    #name the averaged column all three original columns + _avg
    averaged_column_name <- paste(cols_to_average, collapse = "_") %>% paste0("_avg")
    
    #organize data and bind it to result data frame
    avg_data <- average_columns(df, cols_to_average)
    colnames(avg_data)[2] <- averaged_column_name
    result <- bind_cols(result, avg_data[2])
  }
}