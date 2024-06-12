#---------------------------DEFINE VARIABLES and PREPROCESSING THE DATA------------------
#define a file name to allow for easy naming of the files later on 
data_file_name <- "1B_"

#load data file as a .xlsx file using the readxl library. the range gets rid of all of the metadata in the header of the data file 
data <- read_excel(data_path, range = "1B - pH Plate!A3:CT129") 

#delete the temperature column 
data <- data[, -which(names(data) == "Temperature(¡C)")]

#delete columns with all NA values for all rows (delete empty wells)
data <- data[, colSums(!is.na(data)) > 0]

#define how many columns there are using the ncol() function after deleteing all the empty colmns. Will be used for the for loop to create files. 
num_columns <- ncol(data)

#extract wavelength column 
wavelength <- data$Wavelength