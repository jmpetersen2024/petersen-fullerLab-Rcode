library(shiny)
library(googlesheets4)
#this file contains three functions to get the MW and charge of each peptoid from the peptoid info data sheet for the survey1 peptoids 

#ADD COMMENTS TO THIS CODE (it is pretty simple, so not urgert, but still should)


survey1_map_input <- function(input) {
  
#simple function to get the number and letter part of the code so that it will math the labeling in the peptoid info sheet
  
  num_part <- gsub("[^0-9]", "", input)
  letter_part <- tolower(gsub("[^A-Za-z]", "", input))
  label <- paste0("L", num_part, letter_part)
  return(label)
  
}


#get MW
survey1_get_molecular_weight <- function(input) {
  mw_label <- survey1_map_input(input)
  
  # Find the row index of the label
  row_index <- match(mw_label, peptoid_info$`Compound (DLN link)`)
  
  # Check if the label was found
  if (!is.na(row_index)) {
    # Access the MW cell directly
    return(peptoid_info$MW[row_index])
  } else {
    shiny::showNotification(
      paste("No data found for input:", input),
      type = "warning",
      duration = 5 
    )
    return(NULL)
  }
}

#get charge

survey1_get_charge <- function(input) {
  charge_label <- survey1_map_input(input)
  
  # Find the row index of the label
  row_index <- match(charge_label, peptoid_info$`Compound (DLN link)`)
  
  # Check if the label was found
  if (!is.na(row_index)) {
    # Access the charge cell directly
    return(peptoid_info$`est. net charge at pH 7`[row_index])
  } else {
    shiny::showNotification(
      paste("No data found for input:", input),
      type = "warning",
      duration = 5 
    )
    return(NULL)
  }
}