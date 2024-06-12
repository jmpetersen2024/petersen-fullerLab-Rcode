preview_master_sheet <- function(input, output, session) {
  
  # master google sheet ID 
  sheet_data <- read_sheet("1YtaESoC86Csj8eVWf6RSiYNfEl1eL85Bl4PyF1vi1kI")
  
  # Display only the first few rows, similar to head()
  head(sheet_data)
  
  #this formats the data table so that the table doesnt expand past the box
  output$sheetPreview <- DT::renderDataTable({
   
    DT::datatable(
      sheet_data,
      options = list(
        scrollX = TRUE,
        autoWidth = TRUE
      )
    )
    
   
    
  })
}