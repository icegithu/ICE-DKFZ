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
library(ggbeeswarm) # for beeswarm

# Set Working directories ======================================================

SUMMARY_DIR = "./Combined_Output"
RAW_DATA_DIR = "./CKB/Rohdaten"
FUNCTIONS_DIR = "./R_functions"
BRIDGE_INFO_FILE = "./CKB/Orga Studie/Plattenbelegungsplan_Bridging.xlsx"
SAMPLE_INFO_FILE = "./CKB/Orga Studie/Plattenbelegungsplan_StudySamples.xlsx"

# Other Global variables ======================================================
PLOT_HEIGHT = "200px"
source(paste0(FUNCTIONS_DIR,"/","Amuse_Functions.R"))

# Build UI =====================================================================

ui <- fluidPage(
  
  themeSelector(),
  
  titlePanel("AMuSe"),

  tabsetPanel(id= "TabPanel",type = "tabs",
              tabPanel("Load Files",
                       checkboxGroupInput("dates",
                                          h4("Select summary files to load"),
                                          choices = get_avaliable_dates(SUMMARY_DIR)),
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
                       plotlyOutput(outputId = "Mean_MFI_Bridging", height = PLOT_HEIGHT),
                       tags$h3("Median MFI Bridging data"),
                       plotlyOutput(outputId = "Median_MFI_Bridging", height = PLOT_HEIGHT),
              ),
              tabPanel("Counts",
                       tags$h2("Box plots of Sample and Bridging Data"),
                       actionButton("download_box_count", "Download"),
                       tags$h3("Mean Count Bridging"),
                       plotlyOutput(outputId = "Mean_Count_Bridging", height = PLOT_HEIGHT),
                       tags$h3("Mean Count Sample"),
                       plotlyOutput(outputId = "Mean_Count_Sample", height = PLOT_HEIGHT),

              ),
              tabPanel("Blank values",
                       tags$h2("Blank Values"),
                       actionButton("download_blank", "Download"),
                       tags$h3("Sample Data"),
                       plotlyOutput(outputId = "Blank_Sample", height = PLOT_HEIGHT),
                       tags$h3("Bridging Data"),
                       plotlyOutput(outputId = "Blank_Bridging", height = PLOT_HEIGHT),
              ),
              tabPanel("Temperature",
                       tags$h2("Temperature delta for Sample and Bridging Data"),
                       actionButton("download_deltaT", "Download"),
                       tags$h3("Combined Data"),
                       plotlyOutput(outputId = "DeltaT_Combined", height = PLOT_HEIGHT),
              ),
              tabPanel("MFI Sample",
                       tags$h2("Line plots for MFI per plate"),
                       fluidRow(
                           column(2, align="left", actionButton("download_MFI_perplate", "Download")),
                           column(2, align="right",tags$h3("Display")),
                           column(4, align="left",
                              radioButtons("perplate_display","", c("daywise" = "Date", "weekwise" = "Week","per plate" = "Plate_daywise"), inline=T)
                           )
                       ),
                       tags$h3("Mean MFI"),
                       plotlyOutput(outputId = "Mean_MFI_perplate", height = PLOT_HEIGHT),
                       tags$h3("Median MFI"),
                       plotlyOutput(outputId = "Median_MFI_perplate", height = PLOT_HEIGHT),
              ),
              tabPanel("KT3 Plot",
                       tags$h2("KT3"),
                       actionButton("download_KT3", "Download"),
                       tags$h3("Sample Data"),
                       plotlyOutput(outputId = "KT3_Sample", height = PLOT_HEIGHT),
                       tags$h3("Bridging Data"),
                       plotlyOutput(outputId = "KT3_Bridge", height = PLOT_HEIGHT), 
              ),
              tabPanel("GST Plot",
                       tags$h2("GST"),
                       actionButton("download_GST", "Download"),
                       tags$h3("Sample Data"),
                       plotlyOutput(outputId = "GST_Sample", height = PLOT_HEIGHT),
                       tags$h3("Bridging Data"),
                       plotlyOutput(outputId = "GST_Bridge", height = PLOT_HEIGHT), 
              ),
              
  )
)

# Build server =================================================================
server <- function(input, output, session) {

  loaded_files <- reactiveValues()

  # When load is pressed it calls the function that updates the reactive container
  observeEvent(input$load_button, load_data())

  observeEvent(input$create_summaries,{
    # here we call the script to create summaries
    week_list <- list.files(RAW_DATA_DIR)
    sample_info <- read.xlsx(SAMPLE_INFO_FILE)
    bridge_info <- read.xlsx(BRIDGE_INFO_FILE)
    for (i in 1:length(week_list)){
        read_in_sample_data(paste0(RAW_DATA_DIR,"/",week_list[i]), sample_info)
        read_in_bridging_data(paste0(RAW_DATA_DIR,"/",week_list[i]), bridge_info)
    }
    # get new files after they have been created
    date_list <- get_avaliable_dates(SUMMARY_DIR)
    # show dates as a checkbox
    updateCheckboxGroupInput(session, "dates",
                       choices = date_list)
    
  })

  load_data <- reactive({
    all_files_list <- list.files(SUMMARY_DIR)
    Sample_df <- data.frame()
    Bridge_df <- data.frame()
    for (i in 1:length(input$dates)){
      date <- input$dates[i]
      sample_file <- all_files_list[str_detect(all_files_list, date) & str_detect(all_files_list,"Sample")]
      if(! is_empty(sample_file)){
        myDF <-read.csv(paste0(SUMMARY_DIR,"/",sample_file), header = T)
        Sample_df <- rbind(Sample_df, myDF)
      }
      bridge_file <- all_files_list[str_detect(all_files_list, date) & str_detect(all_files_list,"Bridging")]
      if(! is_empty(bridge_file)){
        myDF <-read.csv(paste0(SUMMARY_DIR,"/",bridge_file), header = T)
        Bridge_df <- rbind(Bridge_df, myDF)
      }
    }
    
    ################################################################################
    ### THIS IS ONLY UNTIL WE GET NEW GOOD Dummy data
    ################################################################################
    # Add KT-3 info (for now) to make those plots later
    Bridge_df$Sample.id[Bridge_df$Sample.id == "AB_1"] <- "KT-3"
    Bridge_df %>% filter(Sample.id == "KT-3")
    Sample_df$Sample.id[Sample_df$Sample.id == "ABC123"| Sample_df$Sample.id == "DEF123"] <- "KT-3"
    Sample_df %>% filter(Sample.id == "KT-3")
    # Rename one analyte to GST tag
    colnames(Bridge_df)[ncol(Bridge_df)] <- "GST_tag"
    Bridge_df$GST_tag <- rnorm(n = nrow(Bridge_df), mean = 90, sd = 5)
    
    colnames(Sample_df)[ncol(Sample_df)] <- "GST_tag"
    Sample_df$GST_tag <- rnorm(n = nrow(Sample_df), mean = 85, sd = 8)
    
    ################################################################################
    ################################################################################
    
    loaded_files$Bridge_mm <- get_mean_median(Bridge_df)
    loaded_files$Bridge <- Bridge_df
    loaded_files$Bridge_blanks <- get_blanks_kt(Bridge_df) 
    loaded_files$Sample_mm <- get_mean_median(Sample_df)
    loaded_files$Sample <- Sample_df
    loaded_files$Sample_mm_per_plate <- get_mean_median_per_plate(Sample_df)
    loaded_files$Sample_blanks <- get_blanks_kt(Sample_df) 

  })


  # Call_plotting functions ====
  output$Mean_MFI_Bridging  <- renderPlotly({
      p <- ggplotly(mean_median_lineplots(loaded_files$Bridge_mm,
                                         input$log_linear)[["Mean"]])
      if(!input$log_linear){p<- remove_hover_duplicate(p)}
      p
  })

  output$Median_MFI_Bridging  <- renderPlotly({
      p <- ggplotly(mean_median_lineplots(loaded_files$Bridge_mm,
                                          input$log_linear)[["Median"]])
      if(!input$log_linear){p<- remove_hover_duplicate(p)}
      p
  })

  output$Mean_Count_Bridging  <- renderPlotly({
      ggplotly(mean_boxplots(loaded_files$Bridge_mm))
  })

  output$Mean_Count_Sample  <- renderPlotly({
      ggplotly(mean_boxplots(loaded_files$Sample_mm))
  })

  output$Blank_Sample  <- renderPlotly({
      ggplotly(blank_bees(loaded_files$Sample_blanks))
  })

  output$Blank_Bridging  <- renderPlotly({
      ggplotly(blank_bees(loaded_files$Bridge_blanks))
  })

  output$DeltaT_Combined  <- renderPlotly({
      ggplotly(delta_t_pointplot(loaded_files$Sample, loaded_files$Bridge))
  })

  output$Mean_MFI_perplate  <- renderPlotly({
      remove_hover_duplicate(ggplotly(mm_per_plate_lineplots(loaded_files$Sample_mm_per_plate,input$perplate_display)[["Mean"]]))
  })

  output$Median_MFI_perplate  <- renderPlotly({
      remove_hover_duplicate(ggplotly(mm_per_plate_lineplots(loaded_files$Sample_mm_per_plate,input$perplate_display)[["Median"]]))
  })
  
  output$KT3_Sample  <- renderPlotly({
      remove_hover_duplicate(ggplotly(KT3_lineplot(loaded_files$Sample_blanks)))
  })
  
  output$KT3_Bridge  <- renderPlotly({
      remove_hover_duplicate(ggplotly(KT3_lineplot(loaded_files$Bridge_blanks)))
  })
  
  output$GST_Sample  <- renderPlotly({
      ggplotly(GST_bees(loaded_files$Sample))
  })
  
  output$GST_Bridge  <- renderPlotly({
      ggplotly(GST_bees(loaded_files$Bridge))
  })
}


# Create Shiny app ----
shinyApp(ui = ui, server = server) # local
# shinyApp(ui = ui, server = server,options=list(host = "0.0.0.0", port=7201)) # dev.station
