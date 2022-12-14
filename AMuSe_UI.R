# AMuSe
# Contributors: GK, EV
# Nov 2022
# Version 1.0

# Clean Environment
rm(list = ls(all.names = TRUE)) #will clear all objects including hidden objects
gc() #free up memory and report the memory usage

library(DT) # to show the dataset
library(openxlsx) # for opening excel files
library(plotly) # interactive/live plots
library(scales) # for additional ggplot functionality 
library(shiny) # shiny :)
library(shinythemes) # for themes
library(shinyWidgets) # for more input types
library(tidyverse) # for data wrangling 

SUMMARY_DIR = "./Combined_Output"
ui <- fluidPage(
  
  theme = shinytheme("readable"),
  
  titlePanel("AMuSe"),
  
  tabsetPanel(id= "TabPanel",type = "tabs",                
              # This is the first panel    
              tabPanel("Load Files", 
                       uiOutput("select_filesUI"),
                       actionButton("load_button", "Load"),
                       tags$h5("If your desire summary doesn't appear, click update"),
                       actionButton("create_summaries", "Update"),
              ),
              tabPanel("Line plots",
                       tags$h2("Line plots")),
              tabPanel("Beadcount",
                       tags$h2("Beadscount")),
              tabPanel("Boxplots",
                       tags$h2("Boxplots")),
              tabPanel("Others",
                       tags$h2("Other Plots"))
  )
)

# Build server ================================================================= 
server <- function(input, output, session) {
  
  loaded_summary_files <- reactiveValues()
  
  output$select_filesUI <-renderUI({
    # get all files available  
    files_list <- list.files(SUMMARY_DIR)
    # get dates from those files
    date_list <- unique(str_extract(files_list, "\\d\\d\\.\\d\\d\\.\\d\\d\\d\\d"))
    date_list <- date_list[!is.na(date_list)]
    # show dates as a checkbox
    checkboxGroupInput("files",
                 h4("Select summary files to load"),
                 choices = date_list,
                 selected = date_list[1])
  })
  
  # When load is pressed it calls the function that updates the reactive container
  observeEvent(input$load_button, load_data())
  
  observeEvent(input$create_summaries,{
    # here we call the script to create summaries
    source("./Read_in_function/Read_in.R")
  })
  
  load_data <- reactive({ 
    for (i in 1:length(input$files)){
      myDF <-read.csv(paste0(SUMMARY_DIR,"/",input$files[i]), header = T, stringsAsFactors = F)
    }
    })
  
  
}


# Create Shiny app ----
shinyApp(ui = ui, server = server) # local
# shinyApp(ui = ui, server = server,options=list(host = "0.0.0.0", port=7201)) # dev.station
