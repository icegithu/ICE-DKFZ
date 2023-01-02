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
PLOT_HEIGHT = "300px"
PLOT_WIDTH = "700px"
source(paste0(FUNCTIONS_DIR,"/","Amuse_Functions.R"))

# Build UI =====================================================================

ui <- fluidPage(
  
  themeSelector(),
  
  titlePanel("AMuSe"),

  tabsetPanel(id= "TabPanel",type = "tabs",
              tabPanel("Load Files",
                       tags$div(tags$p()),
                       fluidRow(
                          column(4, airDatepickerInput("datemultiple", "Select individual dates:", multiple = T, inline = T,firstDay = 1)),
                          # column(2, airDatepickerInput("daterange", "Select a date range:", range = T)),
                          column(4, align = "left", 
                                 verbatimTextOutput("files_to_load_text"),
                                 tags$h5("If your desire summary doesn't appear, click update"),
                                 actionButton("create_summaries", "Update"),     
                          ),
                       ),
                       actionButton("load_button", "Load"),
              ),
              tabPanel("MFI Bridging",
                       tags$h2("Line plots Bridging data"),
                       fluidRow(
                           column(4, align = "left",
                                  downloadButton("download_MFI_bridge_mean", "Download Mean Plot"),
                           ),
                           column(4, align = "left",
                                  downloadButton("download_MFI_bridge_median", "Download Median Plot"),
                           ),
                           column(2,
                               div(id='my_log', materialSwitch(inputId = "log_linear", value = F,
                                                           status = "danger", label = "Log Scale"))
                           )
                       ),
                       tags$h3("Mean MFI Bridging data"),
                       plotlyOutput(outputId = "Mean_MFI_Bridging", height = PLOT_HEIGHT, width = PLOT_WIDTH),
                       tags$h3("Median MFI Bridging data"),
                       plotlyOutput(outputId = "Median_MFI_Bridging", height = PLOT_HEIGHT, width = PLOT_WIDTH),
              ),
              tabPanel("Counts",
                       tags$h2("Box plots of Sample and Bridging Data"),
                       fluidRow(
                           column(4, airDatepickerInput("date_boxplot", "Select individual dates:", multiple = T, inline = T)),
                           column(4, downloadButton("download_box_count_bridge", "Download Count Bridge"),
                                     tags$div(tags$p()),
                                     downloadButton("download_box_count_sample", "Download Count Sample")
                           ),
                       ),
                       tags$h3("Mean Count Bridging"),
                       plotlyOutput(outputId = "Mean_Count_Bridging", height = PLOT_HEIGHT, width = PLOT_WIDTH),
                       tags$h3("Mean Count Sample"),
                       plotlyOutput(outputId = "Mean_Count_Sample", height = PLOT_HEIGHT, width = PLOT_WIDTH),

              ),
              tabPanel("Blank values",
                       tags$h2("Blank Values"),
                       fluidRow(
                          column(4, downloadButton("download_blank_sample", "Download Sample")),
                          column(4, downloadButton("download_blank_bridge", "Download Bridge"))
                       ),
                       tags$h3("Sample Data"),
                       plotlyOutput(outputId = "Blank_Sample", height = PLOT_HEIGHT, width = PLOT_WIDTH),
                       tags$h3("Bridging Data"),
                       plotlyOutput(outputId = "Blank_Bridging", height = PLOT_HEIGHT, width = PLOT_WIDTH),
              ),
              tabPanel("Temperature",
                       tags$h2("Temperature delta for Sample and Bridging Data"),
                       downloadButton("download_deltaT", "Download"),
                       tags$h3("Combined Data"),
                       plotlyOutput(outputId = "DeltaT_Combined", height = PLOT_HEIGHT, width = PLOT_WIDTH),
              ),
              tabPanel("MFI Sample",
                       tags$h2("Line plots for MFI per plate"),
                       fluidRow(
                           column(2, align="right",tags$h3("Display")),
                           column(4, align="left",
                              radioButtons("perplate_display","", c("daywise" = "Date", "weekwise" = "Week","per plate" = "Plate_daywise"), inline=T)
                           ),
                           column(3, downloadButton("download_MFI_perplate_mean", "Download Mean")),
                           column(3, downloadButton("download_MFI_perplate_median", "Download Median")),
                       ),
                       tags$h3("Mean MFI"),
                       plotlyOutput(outputId = "Mean_MFI_perplate", height = PLOT_HEIGHT, width = PLOT_WIDTH),
                       tags$h3("Median MFI"),
                       plotlyOutput(outputId = "Median_MFI_perplate", height = PLOT_HEIGHT, width = PLOT_WIDTH),
              ),
              tabPanel("KT3 Plot",
                       tags$h2("KT3"),
                       fluidRow(
                          column(4, downloadButton("download_KT3_sample", "Download Sample")),
                          column(4, downloadButton("download_KT3_bridge", "Download Bridge")),
                       ),
                       tags$h3("Sample Data"),
                       plotlyOutput(outputId = "KT3_Sample", height = PLOT_HEIGHT, width = PLOT_WIDTH),
                       tags$h3("Bridging Data"),
                       plotlyOutput(outputId = "KT3_Bridge", height = PLOT_HEIGHT, width = PLOT_WIDTH), 
              ),
              tabPanel("GST Plot",
                       tags$h2("GST"),
                       fluidRow(
                           column(4, downloadButton("download_GST_sample", "Download Sample")),
                           column(4, downloadButton("download_GST_bridge", "Download Bridge")),
                       ),
                       tags$h3("Sample Data"),
                       plotlyOutput(outputId = "GST_Sample", height = PLOT_HEIGHT, width = PLOT_WIDTH),
                       tags$h3("Bridging Data"),
                       plotlyOutput(outputId = "GST_Bridge", height = PLOT_HEIGHT, width = PLOT_WIDTH), 
              ),
              
  )
)

# Build server =================================================================
server <- function(input, output, session) {
    
  get_dates_to_load <- reactive({
        date_list <-input$datemultiple
        avaliable_dates <- get_avaliable_dates(SUMMARY_DIR)
        if (is.null(date_list)){
            # if no date is selected, the last two are loaded
            dates_to_load$dates <- tail(avaliable_dates,n=2)
        }
        else{
            monday_list <- cut(as.Date(date_list), "week")
            monday_list <- factor(format(as.Date(monday_list),"%Y%m%d"))
            # TODO: this assumes that the available dates are mondays, it might need change
            date_intersection <- factor(avaliable_dates,levels = levels(monday_list))
            date_intersection <- date_intersection[!is.na(date_intersection)]
            if (length(date_intersection) == 0){
                showModal(modalDialog("No data found for the dates selected, loading the two more recent",easyClose = T))
                date_intersection <- tail(avaliable_dates,n=2)
            }
            dates_to_load$dates <- sort(as.character(date_intersection))
        }
  })
  # Reactive containers ====
  loaded_files <- reactiveValues()
  dates_to_load <- reactiveValues()

  # Button observing functions ====
  observeEvent(input$load_button, {
    get_dates_to_load()
    load_data()
    update_calendar()
    })

  observeEvent(input$create_summaries,{
    # here we call the script to create summaries
    week_list <- list.files(RAW_DATA_DIR)
    sample_info <- read.xlsx(SAMPLE_INFO_FILE)
    bridge_info <- read.xlsx(BRIDGE_INFO_FILE)
    for (i in 1:length(week_list)){
        read_in_sample_data(paste0(RAW_DATA_DIR,"/",week_list[i]), sample_info)
        read_in_bridging_data(paste0(RAW_DATA_DIR,"/",week_list[i]), bridge_info)
    }
    get_dates_to_load()
    load_data()
    
  })
  
  output$files_to_load_text <- renderText({ 
      if (length(dates_to_load$dates)>0){
          paste0("The following weeks will be loaded:\n",
                 paste0(
                 dates_to_load$dates,
                 collapse = "\n"
                 ))}
      else{
          "No files found"
      }})
  
  load_data <- reactive({
    all_files_list <- list.files(SUMMARY_DIR)
    Sample_df <- data.frame()
    Bridge_df <- data.frame()
    for (i in 1:length(dates_to_load$dates)){
      selected_date <- dates_to_load$dates[i]
      sample_file <- all_files_list[str_detect(all_files_list, selected_date) & str_detect(all_files_list,"Sample")]
      if(! is_empty(sample_file)){
        myDF <-read.csv(paste0(SUMMARY_DIR,"/",sample_file), header = T)
        Sample_df <- rbind(Sample_df, myDF)
      }
      bridge_file <- all_files_list[str_detect(all_files_list, selected_date) & str_detect(all_files_list,"Bridging")]
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
  
  update_calendar <- reactive({
      # TODO: Connect the selection of this calender to the plot
      highlightedDates  <- as.Date(dates_to_load$dates,format="%Y%m%d")
      calendar_options <- data.frame(highlightedDates)
      
      updateAirDateInput(
          session = session,
          "date_boxplot",
          value = calendar_options$highlightedDates,
          options = calendar_options
      )
  })


  # Call_plotting functions ====
  
  ## TAB 1 ====
  output$Mean_MFI_Bridging  <- renderPlotly({
      remove_hover_duplicate(ggplotly(mean_median_lineplots(loaded_files$Bridge_mm,
                                         input$log_linear)[["Mean"]]))
  })
  output$download_MFI_bridge_mean <- downloadHandler(
      filename = "MFI_bridge_mean.html",
      content = function(file) {
          htmlwidgets::saveWidget(as_widget(remove_hover_duplicate(
              ggplotly(mean_median_lineplots(loaded_files$Bridge_mm,input$log_linear)[["Mean"]]))),file)
          
      }
  )

  output$Median_MFI_Bridging  <- renderPlotly({
      remove_hover_duplicate(ggplotly(mean_median_lineplots(loaded_files$Bridge_mm,
                                          input$log_linear)[["Median"]]))
  })
  
  output$download_MFI_bridge_median <- downloadHandler(
      filename = "MFI_bridge_median.html",
      content = function(file) {
          htmlwidgets::saveWidget(as_widget(remove_hover_duplicate(
              ggplotly(mean_median_lineplots(loaded_files$Bridge_mm,input$log_linear)[["Median"]]))),file)
          
      }
  )

  ## TAB 2 ====
  output$Mean_Count_Bridging  <- renderPlotly({
      ggplotly(mean_boxplots(loaded_files$Bridge_mm))
  })

  output$Mean_Count_Sample  <- renderPlotly({
      ggplotly(mean_boxplots(loaded_files$Sample_mm))
  })

  output$download_box_count_bridge <- downloadHandler(
      filename = "Counts_bridge.html",
      content = function(file) {
          htmlwidgets::saveWidget(as_widget(ggplotly(mean_boxplots(loaded_files$Bridge_mm))),file)
      }
  )
  
  output$download_box_count_sample <- downloadHandler(
      filename = "Counts_sample.html",
      content = function(file) {
          htmlwidgets::saveWidget(as_widget(ggplotly(mean_boxplots(loaded_files$Sample_mm))),file)
      }
  )
  
  ## TAB 3 ====
  
  output$Blank_Sample  <- renderPlotly({
      ggplotly(blank_bees(loaded_files$Sample_blanks))
  })

  output$Blank_Bridging  <- renderPlotly({
      ggplotly(blank_bees(loaded_files$Bridge_blanks))
  })
  
  output$download_blank_sample <- downloadHandler(
      filename = "Blanks_sample.html",
      content = function(file) {
          htmlwidgets::saveWidget(as_widget(ggplotly(blank_bees(loaded_files$Sample_blanks))),file)
      }
  )
  
  output$download_blank_bridge <- downloadHandler(
      filename = "Blanks_bridge.html",
      content = function(file) {
          htmlwidgets::saveWidget(as_widget(ggplotly(blank_bees(loaded_files$Bridge_blanks))),file)
      }
  )
  
  ## TAB 4 ====

  output$DeltaT_Combined  <- renderPlotly({
      remove_parenthesis_legend(
          ggplotly(delta_t_pointplot(loaded_files$Sample, loaded_files$Bridge)))
  })
  
  output$download_deltaT <- downloadHandler(
      filename = "DeltaT.html",
      content = function(file) {
          htmlwidgets::saveWidget(as_widget(remove_parenthesis_legend(
              ggplotly(delta_t_pointplot(loaded_files$Sample, loaded_files$Bridge)))),file)
      }
  )
  ## TAB 5 ====
  
  output$Mean_MFI_perplate  <- renderPlotly({
      remove_hover_duplicate(ggplotly(mm_per_plate_lineplots(loaded_files$Sample_mm_per_plate,input$perplate_display)[["Mean"]]))
  })

  output$Median_MFI_perplate  <- renderPlotly({
      remove_hover_duplicate(ggplotly(mm_per_plate_lineplots(loaded_files$Sample_mm_per_plate,input$perplate_display)[["Median"]]))
  })
  
  output$download_MFI_perplate_mean <- downloadHandler(
      filename = paste0("MFI",input$perplate_display,"_mean.html"),
      content = function(file) {
          htmlwidgets::saveWidget(as_widget(remove_hover_duplicate(
              ggplotly(mm_per_plate_lineplots(loaded_files$Sample_mm_per_plate,input$perplate_display)[["Mean"]]))),file)
      }
  )
  
  output$download_MFI_perplate_median <- downloadHandler(
      filename = paste0("MFI",input$perplate_display,"_median.html"),
      content = function(file) {
          htmlwidgets::saveWidget(as_widget(remove_hover_duplicate(
              ggplotly(mm_per_plate_lineplots(loaded_files$Sample_mm_per_plate,input$perplate_display)[["Median"]]))),file)
      }
  )
  
  ## TAB 6 ====
  
  output$KT3_Sample  <- renderPlotly({
      remove_hover_duplicate(ggplotly(KT3_lineplot(loaded_files$Sample_blanks)))
  })
  
  output$KT3_Bridge  <- renderPlotly({
      remove_hover_duplicate(ggplotly(KT3_lineplot(loaded_files$Bridge_blanks)))
  })
  
  output$download_KT3_sample <- downloadHandler(
      filename = "KT3_Sample.html",
      content = function(file) {
          htmlwidgets::saveWidget(as_widget(remove_hover_duplicate(ggplotly(KT3_lineplot(loaded_files$Sample_blanks)))),file)
      }
  )
  
  output$download_KT3_bridge <- downloadHandler(
      filename = "KT3_Bridge.html",
      content = function(file) {
          htmlwidgets::saveWidget(as_widget(remove_hover_duplicate(ggplotly(KT3_lineplot(loaded_files$Bridge_blanks)))),file)
      }
  )
  
  ## TAB 7 ====
  
  output$GST_Sample  <- renderPlotly({
      ggplotly(GST_bees(loaded_files$Sample))
  })
  
  output$GST_Bridge  <- renderPlotly({
      ggplotly(GST_bees(loaded_files$Bridge))
  })
  
  output$download_GST_sample <- downloadHandler(
      filename = "GST_Sample.html",
      content = function(file) {
          htmlwidgets::saveWidget(as_widget(ggplotly(GST_bees(loaded_files$Sample))),file)
      }
  )
  
  output$download_GST_bridge <- downloadHandler(
      filename = "GST_Bridge.html",
      content = function(file) {
          htmlwidgets::saveWidget(as_widget(ggplotly(GST_bees(loaded_files$Bridge))),file)
      }
  )
  
}


# Create Shiny app ----
shinyApp(ui = ui, server = server) # local
# shinyApp(ui = ui, server = server,options=list(host = "0.0.0.0", port=7201)) # dev.station
