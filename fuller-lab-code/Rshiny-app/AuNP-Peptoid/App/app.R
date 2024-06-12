library(shiny)
library(shinydashboard)
library(googledrive)
library(googlesheets4)
library(writexl)
library(readxl)
library(DT)
#library(googleAuthR)
#library(shinyjs)



# Identify the current working directory
current_working_dir <- getwd()

# Define the project directory name
project_dir_name <- "AuNP-Peptoid"

# Find the path to the project directory
# This method splits the current working directory by the system's file separator and reconstructs the path up to the project directory
path_parts <- unlist(strsplit(current_working_dir, .Platform$file.sep))
project_dir_index <- which(path_parts == project_dir_name)
if (length(project_dir_index) == 0) {
  stop("Project directory not found in the current working directory path.")
}
project_path <- paste(path_parts[1:project_dir_index], collapse = .Platform$file.sep)

#construct the paths to the scripts
scripts_dir <- file.path(project_path, "scripts")
preview_master_sheet_script <- file.path(scripts_dir, "preview_master_sheet.R")
survey1_process_data_script <- file.path(scripts_dir, "survey1_process_data.R")
survey1_get_peptoid_info_script <- file.path(scripts_dir, "survey1_get_peptoid_info.R")

#source the scripts
source(preview_master_sheet_script)
source(survey1_process_data_script)
source(survey1_get_peptoid_info_script)



#options for authenticating google drive 

ui <- dashboardPage(
  
 
  
  dashboardHeader(title = "Main Menu"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Upload File", tabName = "upload", icon = icon("upload")),
      menuItem("Make Plots", tabName = "uv_plots", icon = icon("chart-line"))

    )
  ),
  dashboardBody(
    
   
    tabItems(
      
      
      # -------------------------------------------Upload File page--------------------------------
      tabItem(tabName = "upload",
              h2("Upload File Page"),
              
              
              fluidRow(
                column(width = 6,
                       box(width = NULL, title = "Experimental Conditions", status = "primary", solidHeader = TRUE, collapsible = FALSE,
                           
                           #input for choosing experiment
                           selectInput("firstInput", "Choose Experiment:", choices = c("6 Peptoid Survey", "Other")),
                           
                           # conditional panels for different experiments
                           conditionalPanel(condition = "input.firstInput == '6 Peptoid Survey'",
                                            
                                            # 6 Peptoid Survey inputs
                                            
                                            #choose raw data file 
                                            #NOTE: you must resave the file on your computer as an .xlsx because the plate reader exports the data .txt even it displays as .xlsx (i don't know why)
                                            #ALSO: make sure Sheet 1 is named 'Sheet1'
                                            fileInput("file1", "Choose XLSX File", accept = ".xlsx"),
                                            
                                            selectInput("np_size", "Nanoparticle Size:",
                                                        choices = c("10 nm"= "10 nm",
                                                                    "20 nm" = "20 nm",
                                                                    "50 nm" = "50 nm",
                                                                    "80 nm" = "80 nm")),
                                   
                  
                              #for the sizes, i just played around wiht it. you have a total width of 12
                                      fluidRow(
                                              #peptoid 1 
                                          column(2,
                                             textInput("peptoid_name_1", "Peptoid 1 Name", value = "")),
                                          column(2,
                                                 numericInput("num_dif_salt_conc1", "# of Salt Conc.(1 or 3)", value = 1, min = 1, max = 3), style = "font-size: 10px;"),
                                          column(1,
                                                 textInput("SMILES1", "SMILES Code", value = ""), style = "font-size: 10px;"),
                                          column(1, 
                                                 checkboxGroupInput("chiral_checkbox1", label = "Chiral?", 
                                                                    choices = c("Yes" = "chiral_y",
                                                                                "No" = "chiral_n")), style = "font-size: 10px;"),
                                          column(1, 
                                                 checkboxGroupInput("aromatic_checkbox1", label = "Aromatic?", 
                                                                    choices = c("Yes" = "aromatic_y",
                                                                                "No" = "aromatic_n")), style = "font-size: 10px;")
                                          
                                      
                                      ),
                                      
                                      fluidRow(
                                        #peptoid 2
                                        column(2,
                                               textInput("peptoid_name_2", "Peptoid 2 Name", value = "")),
                                        column(2,
                                               numericInput("num_dif_salt_conc2", "# of Salt Conc.(1 or 3)", value = 1, min = 1, max = 3), style = "font-size: 10px;"),
                                        column(1,
                                               textInput("SMILES2", "SMILES Code", value = ""), style = "font-size: 10px;"),
                                        column(1, 
                                               checkboxGroupInput("chiral_checkbox2", label = "Chiral?", 
                                                                  choices = c("Yes" = "chiral_y",
                                                                              "No" = "chiral_n")), style = "font-size: 10px;"),
                                        column(1, 
                                               checkboxGroupInput("aromatic_checkbox2", label = "Aromatic?", 
                                                                  choices = c("Yes" = "aromatic_y",
                                                                              "No" = "aromatic_n")), style = "font-size: 10px;")
                                      ),
                                      
                                      fluidRow(
                                        #peptoid 3
                                        column(2,
                                               textInput("peptoid_name_3", "Peptoid 3 Name", value = "")),
                                        column(2,
                                               numericInput("num_dif_salt_conc3", "# of Salt Conc.(1 or 3)", value = 1, min = 1, max = 3), style = "font-size: 10px;"),
                                        column(1,
                                               textInput("SMILES3", "SMILES Code", value = ""), style = "font-size: 10px;"),
                                        column(1, 
                                               checkboxGroupInput("chiral_checkbox3", label = "Chiral?", 
                                                                  choices = c("Yes" = "Yes",
                                                                              "No" = "Not")), style = "font-size: 10px;"),
                                        column(1, 
                                               checkboxGroupInput("aromatic_checkbox3", label = "Aromatic?", 
                                                                  choices = c("Yes" = "Yes",
                                                                              "No" = "No")), style = "font-size: 10px;")
                                      ),
                                      
                                      fluidRow(
                                        #peptoid 4
                                        column(2,
                                               textInput("peptoid_name_4", "Peptoid 4 Name", value = "")),
                                        column(2,
                                               numericInput("num_dif_salt_conc4", "# of Salt Conc.(1 or 3)", value = 1, min = 1, max = 3), style = "font-size: 10px;"),
                                        column(1,
                                               textInput("SMILES4", "SMILES Code", value = ""), style = "font-size: 10px;"),
                                        column(1, 
                                               checkboxGroupInput("chiral_checkbox4", label = "Chiral?", 
                                                                  choices = c("Yes" = "chiral_y",
                                                                              "No" = "chiral_n")), style = "font-size: 10px;"),
                                        column(1, 
                                               checkboxGroupInput("aromatic_checkbox4", label = "Aromatic?", 
                                                                  choices = c("Yes" = "aromatic_y",
                                                                              "No" = "aromatic_n")), style = "font-size: 10px;")
                                      ),
                                      
                                      fluidRow(
                                        #peptoid 5
                                        column(2,
                                               textInput("peptoid_name_5", "Peptoid 5 Name", value = "")),
                                        column(2,
                                               numericInput("num_dif_salt_conc5", "# of Salt Conc.(1 or 3)", value = 1, min = 1, max = 3), style = "font-size: 10px;"),
                                        column(1,
                                               textInput("SMILES5", "SMILES Code", value = ""), style = "font-size: 10px;"),
                                        column(1, 
                                               checkboxGroupInput("chiral_checkbox5", label = "Chiral?", 
                                                                  choices = c("Yes" = "chiral_y",
                                                                              "No" = "chiral_n")), style = "font-size: 10px;"),
                                        column(1, 
                                               checkboxGroupInput("aromatic_checkbox5", label = "Aromatic?", 
                                                                  choices = c("Yes" = "aromatic_y",
                                                                              "No" = "aromatic_n")), style = "font-size: 10px;"),
                                      ),
                                      
                                      fluidRow(
                                        #peptoid 6 
                                        column(2,
                                               textInput("peptoid_name_6", "Peptoid 6 Name", value = "")),
                                        column(2,
                                               numericInput("num_dif_salt_conc6", "# of Salt Conc.(1 or 3)", value = 1, min = 1, max = 3), style = "font-size: 10px;"),
                                        column(1,
                                               textInput("SMILES6", "SMILES Code", value = ""), style = "font-size: 10px;"),
                                        column(1, 
                                               checkboxGroupInput("chiral_checkbox6", label = "Chiral?", 
                                                                  choices = c("Yes" = "chiral_y",
                                                                              "No" = "chiral_n")), style = "font-size: 10px;"),
                                        column(1, 
                                               checkboxGroupInput("aromatic_checkbox6", label = "Aromatic?", 
                                                                  choices = c("Yes" = "aromatic_y",
                                                                              "No" = "aromatic_n")), style = "font-size: 10px;"),
                                      ),
                                          
                                            #experimental conditions 
                                            selectInput("salt_type", "Type of Salt", choices = c("NaCl")),
                                            selectInput("pH_tested", "pH", choices = c("7")),
                                            selectInput("buffer_type", "Buffer", choices = c("Phospate Buffer")),
                                            
                                            
                                            #upload button
                                            actionButton("survey1_process", "Process Data") 
                    
                           ),
                           
                           
                           #-----------------------------USE FOR FUTURE EXPERIMENTS -----------------------------
                           conditionalPanel(condition = "input.firstInput == 'Other'",
                                            textInput("textInput2", "Placeholder for Other Experiments")
                           ),
                           
                           
                           
                           
                           #------------------------------DELETE DUPLICATE FILES BUTTON-------------------------
                           actionButton("deleteDupes", "Delete Duplicate Files")
                           
                       ) #box end 
                ), #column end 
                
                

                
                
                                     #--------------MASTER GOOGLE SHEET PREVIEW ---------------------
            
                column(width = 6,
                       box(title = "Data Table Preview", status = "primary", solidHeader = TRUE, collapsible = FALSE,
                           
                           #from DT library
                           DT::dataTableOutput("sheetPreview")
                  ),
                  
                        actionButton("refresh_master_sheet_upload", "Refresh")
                
            )
                
      )),
      
      
      
      
      
      #-------------------------------------------------------------------------------------------------------------
      #-------------------------------------------------Make UV Plots page--------------------------------------------- 
      #-------------------------------------------------------------------------------------------------------------
      
      
      tabItem(tabName = "uv_plots",
              h2("Make UV Plots"),
              
              
              fluidRow(
                #file pickers column 
                column(width = 3,
                       box(
                         title = "File Selection", 
                         status = "primary", 
                         solidHeader = TRUE, 
                         collapsible = FALSE,
                         width = NULL,  # Auto width
                         div(style = "padding-bottom: 10px;", 
                             numericInput("numFilesInput", "Number of Files:", value = 1, min = 1, max = 10)),
                         div(style = "padding-bottom: 10px;", 
                             uiOutput("filePickersUI")),
                         div(style = "padding-bottom: 10px;", 
                             actionButton("plotButton", "Generate Plot", class = "btn-primary"))
                         
                       ) #box end 
                       
                       
                ), #column 1 end 
                
                
                
                
                #plot column
                column(width = 9,
                       box(
                         title = "Plot",
                         status = "primary", 
                         solidHeader = TRUE, 
                         collapsible = FALSE,
                         width = NULL,  
                         
                         #plot output area
                         plotOutput("plot", height = "600px"),
                         
                         #plot title 
                         textInput("uv_plotTitleInput", "Plot Title"),
                         
                         #ADD SLIDERS FOR AXIS LENGTH
                         
                       ), #box end 
                       
                       
                       #box for saving file stuff
                       box(
                         title = "Save Plot",
                         status = "primary", 
                         solidHeader = TRUE, 
                         collapsible = FALSE,
                         width = NULL, 
                         
                         #NEED TO DO SERVER LOGIC FOR THESE BUTTONS 
                         #file save name text input 
                         textInput("uv_plotFilename", "Enter the name for the plot file (don't forget to include .png):"),
                         
                         #save plot button 
                         actionButton("uv_savePlot", "Save Plot to Drive"),
                         
                         #email plot button 
                         actionButton("uv_emailPlot", "Email Plot")
                       )
                       
                    ) #column 2 end
                
               ) #fluid row end 
              
           ) #tabItem end
      
      ) #tab items end 
    )
   
    ) #dashboard body end 
  
    #google drive div end 
  
 
  
   #app end 
















#----------------------------------------------------------------------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------SERVER CODE---------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------


server <- function(input, output, session) {
  
  

  drive_auth()
  
#--------------------------------------------------------------------
#---------------------UPLOAD FILE----------------------------------
#--------------------------------------------------------------------
  
  
#--------------------------preview master sheet--------------------
  #function to preview the master sheet on the "upload file" page. 
  #this file can be found in the scripts folder with name 'preview_master_sheet.R'
  preview_master_sheet(input, output, session)
  
  
  
  #-----------------store uploaded file------------------
  

  peptoid_info <- reactiveVal()
  
  observe({
    req(input$refresh_master_sheet_upload)  # Trigger the data refresh when the specific input is triggered
    peptoid_info(read_sheet(as_sheets_id("1EgJfQSK5q3OaotoQKZ_wQVaNpf1uG8WaR8SoS2J70BQ")))
  })
  
  
  
  #'reactive expression' to store the uploaded file as a dataframe. in shiny, a reactive expression basically means that it is constantly listening to 
  #'the page inputs and can interact with the data
  uploadedData <- reactive({
    
    #check if a file is uploaded <-- good practice to code this in just in case even though it probably isnt needed
    req(input$file1)
    
    #assign input to variable 
    inFile <- input$file1
    
    #assign excel sheet to data frame. this range is specifically for the 6 peptoid survey experiment
    data <- readxl::read_excel(inFile$datapath, range = "Sheet1!A3:CT129")
    
    
    #this returns the RAW DATA SET in a dataframe. no real processing is taking place, just getting it uploaded to your R env
    return(data)
  })
  
  
  
  #------------------Survey1 Process Data Button-------------------------
  
  #observeEvent for 'Process Data' button click. basically, it is the logic or code behind the button. 
  #in this, we are going to call survey1_process_data.R to process all of our and upload it to Google Drive 
  #see the above file if you want to know more about how the function works
  observeEvent(input$survey1_process, {
    
    
    
    if (isTruthy(input$file1)) {
     
      #-------------------CREATE FILES AND UPLOAD THEM TO DRIVE----------------------------
      inFile <- input$file1
      raw_data <- readxl::read_excel(inFile$datapath, range = "Sheet1!A3:CT129")
      
      
      output$error_message <- renderText({ "" })
      tryCatch({
        
        #main function to process data for 6 peptoid survey experiment. file is named: survey1_process_data.R
        #for the near future, we will only be using 10 nm AuNP, but in the future, lets define this based on the np_size input 
        survey1_process_data(input, raw_data, '16VeTz69DEirMMgS6sFvBJVqaW7Lax7cn')
        
       }, error = function(e) {
       output$error_message <- renderText({ 
         paste("An error occurred:", e$message) 
       })
       
     })
     

      
      
      #------------------UPDATE MASTER GOOGLE SHEET WITH NEW PEPTODS---------------------------
  
      #set up google sheet variables 
      master_sheet_id <- "1YtaESoC86Csj8eVWf6RSiYNfEl1eL85Bl4PyF1vi1kI" 
      
      
      #a reactive expression to create the dataframe from user input 
      peptoid_data <- reactive({
  
        #create an empty dataframe
        master_gs_data <- data.frame()
        
        #loop through numbers 1-6 for peptoids 1-6
        for (i in 1:6) {
          
          #assign variables
          #see survey1_get_peptoid_info.R for function calls
          peptoid_name <- input[[paste0("peptoid_name_", i)]]
          SMILES <- input[[paste0("SMILES", i)]]
          num_residues <- "5"
          MW <- survey1_get_molecular_weight(input[[paste0("peptoid_name_", i)]])
          peptoid_charge <- survey1_get_charge(input[[paste0("peptoid_name_", i)]])
          chiral <- input[[paste0("chiral_checkbox", i)]]
          aromatic <- input[[paste0("aromatic_checkbox", i)]]
          
          #this is in the for loop, so it creates variables, appends them, then creates new variables and a new row during the next for loop cycle
          peptoid_row <- data.frame(
            peptoid_name = peptoid_name,
            SMILES = SMILES,
            num_residues = num_residues,
            MW = MW,
            peptoid_charge = peptoid_charge,
            chiral = chiral,
            aromatic = aromatic
          )
          
          #bind the row to the dataframe
          master_gs_data<- rbind(master_gs_data, peptoid_row)
          
        } #end peptoid 1-6 for loop 
  
        #return the complete dataframe. this is a reactive expression so the syntax is that you dont use return()
        master_gs_data
      })
      
      #append dataframe to google sheet 
      sheet_append(master_sheet_id,peptoid_data())
      
      #lets user know the google sheet was uploaded successfully 
      output$status <- renderText("Google Sheet updated successfully!")
      
    } #end of isTruthy statement 
  }) #end of observeEvent 
  

  
  
  #---------REFRESH MASTER GOOGLE SHEET ON UPLOAD FILES PAGE BUTTON LOGIC ----------------------------
  
  observeEvent(input$refresh_master_sheet_upload, {
    preview_master_sheet(input, output, session)
    
  })# end of observe event 
  
  

  
  #---------------------------DELETE DUPLICATE FILES BUTTON LOGIC--------------------------------------------
  observeEvent(input$deleteDupes, {
    
    # MAKE THIS BASED ON NANOPARTICLE SIZE IN FUTURE 
    #for right now, this is just 10nm folder 
    folder_id <- "16VeTz69DEirMMgS6sFvBJVqaW7Lax7cn"
    
    #make a list to store all of the files in the Google Drive folder
    files <- drive_ls(as_id(folder_id))
    

    #find duplicates 
    dupes <- files$name[duplicated(files$name)]
    
    #loop through duplicates and delete them
    for (dupe in dupes) {

      file_id <- files$id[files$name == dupe]
      
      #google drive function to delete files 
      drive_rm(as_id(file_id))
    }
    
    #output result on screen
    num_dupes <- length(dupes)
    if (num_dupes > 0) {
      msg <- paste(num_dupes, "duplicate files deleted.")
    } else {
      msg <- "No duplicate files found."
    }
    
    showNotification(msg, type = "message", duration = 5)
    
  }) #end of observe event 

  
  
  
  
  
  
  
#--------------------------------------------------------------------
#---------------------GENERATE PLOT----------------------------------
#--------------------------------------------------------------------
  
  
  #create reactive expression for the files in the file list 
  #right now this is set for just the 10nm particles, in the future allow for the user to select different folder options 
  files <- reactive({
    drive_ls(as_id("16VeTz69DEirMMgS6sFvBJVqaW7Lax7cn"))  
  })
  
  
  
  
  #-------------------filePickersUI---------------
    
 #output for the file pickers. gest the number of files input and then 
  output$filePickersUI <- renderUI({
    
    numFiles <- input$numFilesInput
    
    # create a select input for each file based off user input 
    #note: selectize is a cool function i found online that uses javascript to make the input fields searchable 
    filePickerList <- lapply(1:numFiles, function(i) {
      selectizeInput(
        inputId = paste0("filePicker", i),
        label = paste("Select File", i),
        choices = files()$name,
        selected = NULL,
        options = list(placeholder = 'Type to search', 
                       onInitialize = I('function() { this.setValue(""); }') # clears initial selection from selectize 
                       ) 
      )
    })

    #update UI
    do.call(tagList, filePickerList)
  })#end of output 
  
  
  
  #reactive value to store data from each file
  all_plot_data <- reactiveValues()

  
  
 

  #----------------------Generate Plot Button-------------------------------
  observeEvent(input$plotButton, {
    
    #this function is basically just a loop to fetch data for each file picker and plot the data when the plot button is clicked
    
    #require number files input (auto set to 1 so that it wont be empty by default)
    req(input$numFilesInput) 
    #assign variable name to input value 
    numFiles <- input$numFilesInput
    
    
    #for loop to get all file names from input and download it from google drive as a temp file, then store in all_plot_data()
    for (i in 1:numFiles) {
      
      #specific file for each loop cycle
      selectedFileName <- input[[paste0("filePicker", i)]]
      #req file (prevents unwanted errors/crashes)
      req(selectedFileName)
      
      #define googleID of each file from the id tag of the selectedFileName
      selectedFileID <- files()$id[files()$name == selectedFileName]
      
      #download the file from Google Drive as a temp file (wont save to computer and take up space.This effectively keeps everything on google drive only)
      plot_local_path <- tempfile(fileext = ".xlsx")
      drive_download(as_id(selectedFileID), path = plot_local_path, overwrite = TRUE)
      
      #read data to plot_data 
      plot_data <- read_excel(plot_local_path)
      
      #add a source coumn for the file name to allow for use in the ggplot legend
      plot_data$Source <- selectedFileName  
      
      #store data in all_plot_data, which is the reactiveValue() we made earlier 
      all_plot_data[[paste0("data", i)]] <- plot_data
    }
    
    #reactive expression to combine data from all files. this is done because ggplot expects a dataframe as an input, not a reactive value, so we have to convert 
    #all_plot_data to a reactive expression here, which will then be converted to a normal dataframe in the output$plot renderPlot() below
    combined_data <- reactive({
      data_list <- lapply(1:10, function(i) {
        all_plot_data[[paste0("data", i)]]
      })
      
      #remove NULL elements from the list. i don't really know why this is necessary, but i was getting an error without and chatGPT said this was the solution and it works so im rolling with it 
      data_list <- Filter(Negate(is.null), data_list)
      
      #combine the data frames in the list into one data frame
      if (length(data_list) > 0) {
        do.call(rbind, data_list)
      } else {
        NULL  
      }
    })
    
    
    #-------------------PLOT OUTPUT-------------
    output$plot <- renderPlot({
      
      #convert reactive expression to R data frame 
      data_to_plot <- combined_data()  
      
      #axis variables -- CHANGE LATER TO USER INPUT FOR SLIDER 
      # Define your fixed values
      x_min <- 450
      x_max <- 700
      x_breaks <- 50  # Interval for x axis ticks
      
      y_min <- 0
      y_max <- 1
      y_breaks <- 0.1  # Interval for y axis ticks
      
      
      #check if data_to_plot is not NULL and has rows. this is put in an if else statement so that there is no error before the genreate plot button is presse d
      if (is.data.frame(data_to_plot) && nrow(data_to_plot) > 0) {
        ggplot(data_to_plot, aes(x = Wavelength, y = Absorbance, color = Source)) +
          geom_line() +
          labs(
            title = input$uv_plotTitleInput,  #title input 
            x = "Wavelength (nm)", 
            y = "Absorbance (AU)", 
            color = "Source"  # Or "Legend" if you prefer
          ) +
          theme_bw() +  #black and white theme 
          theme(
            text = element_text(size = 14),  # Base size for all text in the plot
            plot.title = element_text(size = 20, hjust = 0.5),  # Title size and centering
            axis.title = element_text(size = 18),  # Axis titles size
            axis.text = element_text(size = 16),  # Axis text size
            legend.title = element_text(size = 16),  # Legend title size
            legend.text = element_text(size = 14)  # Legend text size
          ) +
          scale_x_continuous(limits = c(x_min, x_max), breaks = seq(x_min, x_max, by = x_breaks), expand = c(0, 0)) +  
          scale_y_continuous(limits = c(y_min, y_max), breaks = seq(y_min, y_max, by = y_breaks), expand = c(0.005, 0))
      } else {
        ggplot() + geom_blank() + labs(title = "No data to display", x = "", y = "") + theme_void()
      }
    })
  })
 
  
}






shinyApp(ui, server)

