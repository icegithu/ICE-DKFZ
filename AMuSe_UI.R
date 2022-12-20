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

# Set Working directories ======================================================

SUMMARY_DIR = "./Combined_Output"
FUNCTIONS_DIR = "./R_functions"

source(paste0(FUNCTIONS_DIR,"/","Plotting_Functions.R"))

# Build UI =====================================================================

ui <- fluidPage(
  
  theme = shinytheme("readable"),
  
  titlePanel("AMuSe"),

  tabsetPanel(id= "TabPanel",type = "tabs",
              tabPanel("Load Files",
                       uiOutput("select_filesUI"),
                       actionButton("load_button", "Load"),
                       tags$h5("If your desire summary doesn't appear, click update"),
                       actionButton("create_summaries", "Update"),
              ),
              tabPanel("MFI Bridging",
                       tags$h2("Line plots Bridging data"),
                       fluidRow(
                           column(2, align = "left",
                                  actionButton("download_MFI_bridge", "Download"),
                           ),
                           column(2,
                               div(id='my_log', materialSwitch(inputId = "log_linear", value = F,
                                                           status = "danger", label = "Log Scale"))
                           )
                       ),
                       tags$h3("Mean MFI Bridging data"),
                       plotlyOutput(outputId = "Mean_MFI_Bridging",width = "100%", height = "700px"),
                       tags$h3("Median MFI Bridging data"),
                       plotlyOutput(outputId = "Median_MFI_Bridging",width = "100%", height = "700px")
              ),
              tabPanel("Counts",
                       tags$h2("Box plots of Sample and Bridging Data"),
                       actionButton("download_box_count", "Download"),
                       tags$h3("Mean Count Bridging"),
                       plotlyOutput(outputId = "Mean_Count_Bridging", height = "700px"),
                       tags$h3("Mean Count Sample"),
                       plotlyOutput(outputId = "Mean_Count_Sample", height = "700px"),

              ),
              tabPanel("Blank values",
                       tags$h2("Blank Values"),
                       actionButton("download_blanck", "Download"),
                       tags$h3("Sample Data"),
                       plotlyOutput(outputId = "Blank_Sample",width = "100%", height = "700px"),
                       tags$h3("Bridging Data"),
                       plotlyOutput(outputId = "Blank_Bridging",width = "100%", height = "700px")
              ),
              tabPanel("Temperature",
                       tags$h2("Temperature delta for Sample and Bridging Data"),
                       actionButton("download_deltaT", "Download"),
                       tags$h3("Sample Data"),
                       plotlyOutput(outputId = "DeltaT_Sample",width = "100%", height = "700px"),
                       tags$h3("Bridging Data"),
                       plotlyOutput(outputId = "DeltaT_Bridging",width = "100%", height = "700px")
              ),
              tabPanel("MFI Sample",
                       tags$h2("Line plots for MFI per plate"),
                       fluidRow(
                           column(2, align="left", actionButton("download_MFI_perplate", "Download")),
                           column(2, align="right",tags$h3("Display")),
                           column(4, align="left",
                              radioButtons("perplate_display","", c("daywise" = "daywise", "weekwise" = "weekwise","per plate" = "platewise"), inline=T)
                           )
                       ),
                       tags$h3("Mean MFI"),
                       plotlyOutput(outputId = "Mean_MFI_perplate",width = "100%", height = "700px"),
                       tags$h3("Median MFI"),
                       plotlyOutput(outputId = "Median_MFI_perplate",width = "100%", height = "700px")
              )
  )
)

# Build server =================================================================
server <- function(input, output, session) {

  loaded_files <- reactiveValues()

  # Loading files functions ====
  output$select_filesUI <-renderUI({
    # get all files available
    files_list <- list.files(SUMMARY_DIR)
    # get dates from those files
    date_list <- unique(str_extract(files_list, "\\d\\d\\.\\d\\d?\\.\\d\\d\\d\\d"))
    date_list <- date_list[!is.na(date_list)]
    # show dates as a checkbox
    checkboxGroupInput("dates",
                 h4("Select summary files to load"),
                 choices = date_list,
                 selected = date_list[1])
  })

  # When load is pressed it calls the function that updates the reactive container
  observeEvent(input$load_button, load_data())

  observeEvent(input$create_summaries,{
    # here we call the script to create summaries
    source(paste0(FUNCTIONS_DIR,"/","Data_read_in_function.R"))
  })

  load_data <- reactive({
    all_files_list <- list.files(SUMMARY_DIR)
    Sample_df <- data.frame()
    Bridge_df <- data.frame()
    for (i in 1:length(input$dates)){
      date <- input$dates[i]
      sample_file <- all_files_list[str_detect(all_files_list, date) & str_detect(all_files_list,"Sample")]
      if(! is_empty(sample_file)){
        myDF <-read.csv(paste0(SUMMARY_DIR,"/",sample_file), header = T, stringsAsFactors = F)
        Sample_df <- rbind(Sample_df, myDF)
      }
      bridge_file <- all_files_list[str_detect(all_files_list, date) & str_detect(all_files_list,"Bridge")]
      if(! is_empty(bridge_file)){
        myDF <-read.csv(paste0(SUMMARY_DIR,"/",bridge_file), header = T, stringsAsFactors = F)
        Bridge_df <- rbind(Bridge_df, myDF)
      }
    }
    loaded_files$Bridge_mm <- get_mean_median(Bridge_df)
    loaded_files$Bridge <- Bridge_df
    loaded_files$Sample_mm <- get_mean_median(Sample_df)
    loaded_files$Sample <- Sample_df
    loaded_files$Sample_mm_per_plate <- get_mean_median_per_plate(Sample_df)

  })


  # Call_plotting functions ====
  output$Mean_MFI_Bridging  <- renderPlotly({
      ggplotly(mean_median_lineplots(loaded_files$Bridge_mm)[["Mean"]])
  })

  output$Median_MFI_Bridging  <- renderPlotly({
      ggplotly(mean_median_lineplots(loaded_files$Bridge_mm)[["Median"]])
  })

  output$Mean_Count_Bridging  <- renderPlotly({
      ggplotly(mean_median_boxplots(loaded_files$Bridge_mm)[["Mean"]])
  })

  output$Mean_Count_Sample  <- renderPlotly({
      ggplotly(mean_median_boxplots(loaded_files$Sample_mm)[["Mean"]])
  })

  output$Blank_Sample  <- renderPlotly({
      ggplotly(blank_boxplots(get_blanks(loaded_files$Sample)))
  })

  output$Blank_Bridging  <- renderPlotly({
      ggplotly(blank_boxplots(get_blanks(loaded_files$Bridge)))
  })

  output$DeltaT_Bridging  <- renderPlotly({
      ggplotly(delta_t_pointplots(loaded_files$Bridge))
  })
  output$DeltaT_Sample  <- renderPlotly({
      ggplotly(delta_t_pointplots(loaded_files$Sample))
  })

  output$Mean_MFI_perplate  <- renderPlotly({
      ggplotly(mm_per_plate_lineplots(loaded_files$Sample_mm_per_plate)[["Mean"]])
  })

  output$Median_MFI_perplate  <- renderPlotly({
      ggplotly(mm_per_plate_lineplots(loaded_files$Sample_mm_per_plate)[["Median"]])
  })
}


# Create Shiny app ----
shinyApp(ui = ui, server = server) # local
# shinyApp(ui = ui, server = server,options=list(host = "0.0.0.0", port=7201)) # dev.station
