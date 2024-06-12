library(readr)
library(writexl)

convert_txt_to_xlsx <- function(base_path) {
  #list all subfolders
  subfolders <- list.dirs(path = base_path, full.names = TRUE, recursive = FALSE)
  
  #iterate through each subfolder
  for (subfolder in subfolders) {
    #list all .txt files in the current subfolder
    txt_files <- list.files(path = subfolder, pattern = "\\.txt$", full.names = TRUE)
    
    #convert each .txt file to .xlsx
    for (txt_file in txt_files) {
      #read the .txt file with UTF-16 encoding (chatGPT helped me with syntax on this)
      file_content <- read.delim(txt_file, header = FALSE, fileEncoding = "UTF-16LE", check.names = FALSE, fill = TRUE)
      
      #construct the .xlsx file name
      xlsx_file <- sub("\\.txt$", ".xlsx", txt_file)
      
      #write to .xlsx
      write_xlsx(file_content, xlsx_file)
    }
  }
}