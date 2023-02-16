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

SUMMARY_DIR = "./CKB/Combined_Output"
RAW_DATA_DIR = "./CKB/"
FUNCTIONS_DIR = "./R_functions"
BRIDGE_INFO_FILE = "./CKB/OrgaStudie/Plattenbelegungsplan_Bridging.xlsx"
SAMPLE_INFO_FILE = "./CKB/OrgaStudie/Plattenbelegungsplan_StudySamples.xlsx"

# Other Global variables ======================================================
# PLOT_HEIGHT = "300px" we leave the height in automatic for the moment
PLOT_WIDTH_LONG = "90%"
PLOT_WIDTH_SHORT = "60%"
source(paste0(FUNCTIONS_DIR,"/","Amuse_Functions.R"))

# Build UI =====================================================================

ui <- fluidPage(
  
  theme = shinytheme("superhero"),
  
  titlePanel("AMuSe"),
  tags$head(tags$style(HTML(css_style))),
  tabsetPanel(id= "TabPanel",type = "tabs",
              tabPanel("Load Files",
                       tags$div(tags$br(),tags$br()),
                       fluidRow(
                        column(8,actionButton("create_summaries", "Update files")),
                        column(4,actionButton('all_weeks_button', "Create all weeks summary", icon = icon("download")),)
                       ),
                       tags$div(tags$p()),
                       verbatimTextOutput("files_created_text"),
                       tags$div(tags$p()),
                       airDatepickerInput("datemultiple", "Select individual dates:", 
                                          multiple = T, inline = T,firstDay = 1, 
                                          highlightedDates = get_all_available_days(SUMMARY_DIR),
                                          maxDate = tail(get_all_available_days(SUMMARY_DIR),n=1)),
                       fluidRow(
                           column(1,actionButton("load_button", "Load"),),
                           column(4, align = "left",verbatimTextOutput("files_to_load_text")),
                       ),
                       
              ),
              tabPanel("Bead Counts",
                       tags$h2("Bead Counts"),
                       airDatepickerInput("date_boxplot", "Select individual dates:", multiple = T, inline = T,firstDay = 1),
                       tags$h3("Mean Count Bridging"),
                       actionButton('download_box_count_bridge', "Download Count Bridge", icon = icon("download")),
                       plotlyOutput(outputId = "Mean_Count_Bridging", width = PLOT_WIDTH_LONG),
                       tags$h3("Mean Count Sample"),
                       actionButton('download_box_count_sample', "Download Count Sample", icon = icon("download")),
                       plotlyOutput(outputId = "Mean_Count_Sample", width = PLOT_WIDTH_LONG),
                       downloadLink('download_box_count_bridge_html', 'Download Count Bridge HTML'),
                       downloadLink('download_box_count_sample_html', 'Download Count Sample HTML'),
                       
              ),
              tabPanel("Blank Values",
                       tags$h2("Blank Values"),
                       airDatepickerInput("blank_calendar", "Select dates:", multiple = T, inline = T,firstDay = 1),
                       tags$h3("Bridging Data"),
                       actionButton("download_blank_bridge", "Download Bridge", icon = icon("download")),
                       plotlyOutput(outputId = "Blank_Bridging", width = PLOT_WIDTH_LONG),
                       tags$h3("Sample Data"),
                       actionButton("download_blank_sample", "Download Sample", icon = icon("download")),
                       plotlyOutput(outputId = "Blank_Sample", width = PLOT_WIDTH_LONG),
                       downloadLink("download_blank_bridge_html", "Download Bridge HTML"),
                       downloadLink("download_blank_sample_html", "Download Sample HTML"),
              ),
              tabPanel("Temperature",
                       tags$h2("Temperature delta for Sample and Bridging Data"),
                       airDatepickerInput("temperature_calendar", "Select dates:", multiple = T, inline = T,firstDay = 1),
                       tags$h3("Combined Data"),
                       actionButton("download_deltaT", "Download", icon = icon("download")),
                       plotlyOutput(outputId = "DeltaT_Combined", width = PLOT_WIDTH_SHORT),
                       downloadLink("download_deltaT_html", "Download HTML"),
              ),
              tabPanel("Plates Controls",
                       tags$h2("Plates Controls"),
                       airDatepickerInput("control_calendar", "Select dates:", multiple = T, inline = T,firstDay = 1),
                       tags$h3("Bridging Data"),
                       fluidRow(
                           column(2, actionButton("download_control_1", "Download Control 1", icon = icon("download")),),
                           column(2, actionButton("download_control_2", "Download Control 2", icon = icon("download")),),
                           column(2, actionButton("download_control_3", "Download Control 3", icon = icon("download")),),
                           column(2, div(id='my_log', materialSwitch(inputId = "platecontrol_log", value = F,status = "danger", label = "Log Scale")),),
                       ),
                       tags$div(tags$br(),tags$br()),
                       plotlyOutput(outputId = "control_1", width = PLOT_WIDTH_LONG),
                       tags$div(tags$br(),tags$br()),
                       plotlyOutput(outputId = "control_2", width = PLOT_WIDTH_LONG),
                       tags$div(tags$br(),tags$br()),
                       plotlyOutput(outputId = "control_3", width = PLOT_WIDTH_LONG),
                       downloadLink("download_control_1_html", "Download Control 1 HTML"),
                       downloadLink("download_control_2_html", "Download Control 2 HTML"),
                       downloadLink("download_control_3_html", "Download Control 3 HTML"),
              ),
              tabPanel("KT3",
                       tags$h2("KT3"),
                       airDatepickerInput("KT3_calendar", "Select dates:", multiple = T, inline = T,firstDay = 1),
                       tags$h3("Bridging Data"),
                       fluidRow(
                        column(2, actionButton("download_KT3_bridge", "Download KT3", icon = icon("download")),),
                        column(2, div(id='my_log', materialSwitch(inputId = "KT3_log", value = F,status = "danger", label = "Log Scale")),),
                       ),
                       plotlyOutput(outputId = "KT3_Bridge", width = PLOT_WIDTH_SHORT),
                       downloadLink("download_KT3_bridge_html", "Download KT3 HTML"),
              ),
              
              tabPanel("GST",
                       tags$h2("GST"),
                       airDatepickerInput("GST_calendar", "Select dates:", multiple = T, inline = T,firstDay = 1),
                       tags$h3("Bridging Data"),
                       actionButton("download_GST_bridge", "Download Bridge", icon = icon("download")),
                       plotlyOutput(outputId = "GST_Bridge", width = PLOT_WIDTH_LONG), 
                       tags$h3("Sample Data"),
                       actionButton("download_GST_sample", "Download Sample", icon = icon("download")),
                       plotlyOutput(outputId = "GST_Sample", width = PLOT_WIDTH_LONG),
                       downloadLink("download_GST_bridge_html", "Download Bridge HTML"),
                       downloadLink("download_GST_sample_html", "Download Sample HTML"),
              ),
              tabPanel("MFI Bridging",
                       tags$h2("Line plots Bridging data"),
                       fluidRow(
                           column(4, align = "left",
                                  actionButton("download_MFI_bridge_mean", "Download Mean Plot", icon = icon("download")),
                           ),
                           column(2, align = "left",
                               div(id='my_log', materialSwitch(inputId = "log_linear", value = F,
                                                           status = "danger", label = "Log Scale"))
                           )
                       ),
                       tags$h3("Mean MFI Bridging data"),
                       plotlyOutput(outputId = "Mean_MFI_Bridging", width = PLOT_WIDTH_LONG),
                       tags$h3("Median MFI Bridging data"),
                       actionButton("download_MFI_bridge_median", "Download Median Plot", icon = icon("download")),
                       plotlyOutput(outputId = "Median_MFI_Bridging", width = PLOT_WIDTH_LONG),
                       downloadLink("download_MFI_bridge_mean_html", "Download Mean Plot HTML"),
                       downloadLink("download_MFI_bridge_median_html", "Download Median Plot HTML"),
              ),
              tabPanel("MFI Samples",
                       tags$h2("Line plots for MFI per plate"),
                       fluidRow(
                           column(4, airDatepickerInput("Samples_calendar", "Select dates:", multiple = T, inline = T,firstDay = 1),),
                           column(2,div(id='my_log', materialSwitch(inputId = "mm_log_toggle", value = F,
                                                                    status = "danger", label = "Log Scale"))),
                           column(2, align="right",tags$h3("Display")),
                           column(4, align="left",
                              radioButtons("perplate_display","", c("Per plate" = "Plate.id", "Daywise" = "Date", "Weekwise" = "Week"), inline=T)
                           ),
                       ),
                       tags$h3("Mean MFI"),
                       actionButton("download_MFI_perplate_mean", "Download Mean", icon = icon("download")),
                       plotlyOutput(outputId = "Mean_MFI_perplate", width = PLOT_WIDTH_LONG),
                       tags$h3("Median MFI"),
                       actionButton("download_MFI_perplate_median", "Download Median", icon = icon("download")),
                       plotlyOutput(outputId = "Median_MFI_perplate", width = PLOT_WIDTH_LONG),
                       downloadLink("download_MFI_perplate_mean_html", "Download Mean HTML"),
                       downloadLink("download_MFI_perplate_median_html", "Download Median HTML"),
              ),
              tags$script(src="scripts.js"),      
  )
)

# Build server =================================================================
server <- function(input, output, session) {
    
  get_dates_to_load <- reactive({
        date_list <-input$datemultiple
        avaliable_dates <- get_avaliable_dates(SUMMARY_DIR)
        files_to_load <- c()
        if (is.null(date_list)){
            # if no date is selected, the last two are loaded
            dates_to_load$dates <- tail(avaliable_dates,n=2)
        }
        else{
            for (date in avaliable_dates){ 
                date_edges <- str_split(date,"-")[[1]]
                date_edges_asdate <- as.Date(as.character(date_edges),format="%Y%m%d")
                all_days <- seq(date_edges_asdate[1], date_edges_asdate[2],by="days")
                for(selected_date in date_list){
                    if(selected_date %in% all_days){
                        files_to_load <- c(files_to_load, date_edges[1])
                    }
                }
            }
            dates_to_load$dates <- sort(unique(as.character(files_to_load)))
        }
        if (length(files_to_load) == 0){
            showModal(modalDialog("No data found for the selected dates, loading the most recent files",easyClose = T,style = "color: red;"))
            files_to_load <- tail(avaliable_dates,n=2)
            dates_to_load$dates <- sort(as.character(files_to_load))
        }
  })
  
  # Reactive containers ====
  loaded_files <- reactiveValues()
  dates_to_load <- reactiveValues()
  created_files <- reactiveValues()

  # Button observing functions ====
  observeEvent(input$load_button, {
    get_dates_to_load()
    load_data()
    update_calendar()
    })

  observeEvent(input$create_summaries,{
      withProgress(message = "Updating files ...",{
        sample_info <- read.xlsx(SAMPLE_INFO_FILE)
        incProgress(0.5)
        bridge_info <- read.xlsx(BRIDGE_INFO_FILE)
      })
    # run script to gather files
    updated_files <- c("Sample files:")
    updated_files <- c(updated_files, read_in_sample_data(paste0(RAW_DATA_DIR), sample_info))
    updated_files <- c(updated_files, "Bridging files:")
    updated_files <- c(updated_files, read_in_bridging_data(paste0(RAW_DATA_DIR), bridge_info))
    # update the text with the new files
    created_files$files <- updated_files
    # update the calendar with the new available dates 
    highlightedDates  <- get_all_available_days(SUMMARY_DIR)
    calendar_options <- data.frame(highlightedDates)
    calendar_options$maxDate <- tail(highlightedDates,n=1)
    
    updateAirDateInput(
        session = session,
        "datemultiple",
        options = calendar_options, 
        value =  tail(highlightedDates,n=1)
    )
    
  })
  
  observeEvent(input$all_weeks_button, {
      all_files_list <- list.files(SUMMARY_DIR)
      Sample_df <- data.frame()
      Bridge_df <- data.frame()
      withProgress(message = 'Gathering data...',{
          for (i in 1:length(all_files_list)){
              if(str_detect(all_files_list[i],"Sample_data")){
                  myDF <-read.csv(paste0(SUMMARY_DIR,"/",all_files_list[i]), header = T)
                  Sample_df <- rbind(Sample_df, myDF)
              }
              if(str_detect(all_files_list[i],"Bridging_data")){
                  myDF <-read.csv(paste0(SUMMARY_DIR,"/",all_files_list[i]), header = T)
                  Bridge_df <- rbind(Bridge_df, myDF)
              }
              incProgress(1/length(all_files_list))
          }
      })
      write_csv(x = Sample_df, file = paste0(SUMMARY_DIR,"/", gsub("-","",Sys.Date()), "_all_sample_data_combined.csv"))
      write_csv(x = Bridge_df, file = paste0(SUMMARY_DIR,"/",gsub("-","",Sys.Date()), "_all_bridging_data_combined.csv"))
      showModal(modalDialog(paste0("Files created: ",paste0(SUMMARY_DIR,"/", gsub("-","",Sys.Date()), "_all_sample_data_combined.csv"), "\n", 
                                   paste0(SUMMARY_DIR,"/",gsub("-","",Sys.Date()), "_all_bridging_data_combined.csv")),easyClose = T))
      
  })
  
  # Text rendering functions ====
  output$files_to_load_text <- renderText({ 
      if (length(dates_to_load$dates)>0){
          paste0("The following weeks will be loaded:\n",
                 paste0(
                 dates_to_load$dates,
                 collapse = "\n"
                 ))}
      else{
          "No files loaded"
      }})
  
  output$files_created_text <- renderText({ 
      if (length(created_files$files)>0){
                 paste0(
                     created_files$files,
                     collapse = "\n"
                 )}
      else{
          "  "
      }})
  
  load_data <- reactive({
    all_files_list <- list.files(SUMMARY_DIR)
    Sample_df <- data.frame()
    Bridge_df <- data.frame()
    withProgress(message = 'Loading data...',{
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
          incProgress(1/length(dates_to_load$dates))
        }
    })
    if(!is_empty(Bridge_df)){
        loaded_files$Bridge_mm <- get_mean_median(Bridge_df)
        loaded_files$Bridge_controls <- get_controls(Bridge_df) 
    }
    loaded_files$Bridge <- Bridge_df
    if(!is_empty(Sample_df)){
        loaded_files$Sample_mm <- get_mean_median(Sample_df)
        loaded_files$Sample_controls <- get_controls(Sample_df) 
    }
    loaded_files$Sample <- Sample_df

  })
  
  update_calendar <- reactive({
      highlightedDates  <- as.Date(as.character(
          sort(unique(c(loaded_files$Bridge$Date, loaded_files$Sample$Date)))),format="%Y%m%d")
      calendar_options <- data.frame(highlightedDates)
      
      updateAirDateInput(
          session = session,
          "date_boxplot",
          value = tail(calendar_options$highlightedDates,1),
          options = calendar_options
      )
      updateAirDateInput(
          session = session,
          "blank_calendar",
          value = calendar_options$highlightedDates,
          options = calendar_options
      )
      updateAirDateInput(
          session = session,
          "control_calendar",
          value = calendar_options$highlightedDates,
          options = calendar_options
      )
      updateAirDateInput(
          session = session,
          "KT3_calendar",
          value = calendar_options$highlightedDates,
          options = calendar_options
      )
      updateAirDateInput(
          session = session,
          "Samples_calendar",
          value = calendar_options$highlightedDates,
          options = calendar_options
      )
      updateAirDateInput(
          session = session,
          "GST_calendar",
          value = calendar_options$highlightedDates,
          options = calendar_options
      )
      updateAirDateInput(
          session = session,
          "temperature_calendar",
          value = calendar_options$highlightedDates,
          options = calendar_options
      )
  })


  # Call_plotting functions ====
  
  ## TAB 1 Mean Count box plots====
  output$Mean_Count_Bridging  <- renderPlotly({      
      date_as_number = as.numeric(str_remove_all(input$date_boxplot,"-"))
      if (any(loaded_files$Bridge_mm$Date==date_as_number)){
          mean_boxplots(loaded_files$Bridge_mm,date_as_number)
      }
  })
  output$download_box_count_bridge_html <- downloadHandler(
      filename = "bead_counts_bridge.html",
      content = function(file) {
          date_as_number = as.numeric(str_remove_all(input$date_boxplot,"-"))
          htmlwidgets::saveWidget(as_widget(
              ggplotly(mean_boxplots(loaded_files$Bridge_mm,date_as_number))),file)

      }
  )
  
  output$Mean_Count_Sample  <- renderPlotly({
      date_as_number = as.numeric(str_remove_all(input$date_boxplot,"-"))
      if (any(loaded_files$Sample_mm$Date==date_as_number)){
          mean_boxplots(loaded_files$Sample_mm,date_as_number)
      }
  })

  output$download_box_count_sample_html <- downloadHandler(
      filename = "bead_counts_sample.html",
      content = function(file) {
          date_as_number = as.numeric(str_remove_all(input$date_boxplot,"-"))
          htmlwidgets::saveWidget(as_widget(
              mean_boxplots(loaded_files$Sample_mm,date_as_number)),file)

      }
  )
  
  ## TAB 2 Blanks ====
  output$Blank_Sample  <- renderPlotly({
      date_as_number = as.numeric(str_remove_all(input$blank_calendar,"-"))
      blank_lines(loaded_files$Sample_controls, date_as_number)
  })

  output$download_blank_sample_html <- downloadHandler(
      filename = "blank_values_sample.html",
      content = function(file) {
          date_as_number = as.numeric(str_remove_all(input$blank_calendar,"-"))
          htmlwidgets::saveWidget(as_widget(
              blank_lines(loaded_files$Sample_controls, date_as_number)),file)

      }
  )
  
  output$Blank_Bridging  <- renderPlotly({
      date_as_number = as.numeric(str_remove_all(input$blank_calendar,"-"))
      blank_lines(loaded_files$Bridge_controls, date_as_number)
  })

  output$download_blank_bridge_html <- downloadHandler(
      filename = "blank_values_bridge.html",
      content = function(file) {
          date_as_number = as.numeric(str_remove_all(input$blank_calendar,"-"))
          htmlwidgets::saveWidget(as_widget(
              blank_lines(loaded_files$Bridge_controls, date_as_number)),file)

      }
  )

  ## TAB 3 Temperature====
  
  output$DeltaT_Combined  <- renderPlotly({
      date_as_number = as.numeric(str_remove_all(input$temperature_calendar,"-"))
      delta_t_pointplot(loaded_files$Sample, loaded_files$Bridge, date_as_number)
  })

  output$download_deltaT_html <- downloadHandler(
      filename = "temperature.html",
      content = function(file) {
          date_as_number = as.numeric(str_remove_all(input$temperature_calendar,"-"))
          htmlwidgets::saveWidget(as_widget(
              delta_t_pointplot(loaded_files$Sample, loaded_files$Bridge, date_as_number)),file)

      }
  )

  ## TAB 4 Control plates====
  
  output$control_1  <- renderPlotly({
      date_as_number = as.numeric(str_remove_all(input$control_calendar,"-"))
      ggplotly(plate_control_plots(loaded_files$Bridge_controls, loaded_files$Sample_controls, input$platecontrol_log, date_as_number)[[1]])
  })

  output$download_control_1_html <- downloadHandler(
      filename = "control_1.html",
      content = function(file) {
          date_as_number = as.numeric(str_remove_all(input$control_calendar,"-"))
          htmlwidgets::saveWidget(as_widget(
              ggplotly(plate_control_plots(loaded_files$Bridge_controls, loaded_files$Sample_controls, input$platecontrol_log, date_as_number)[[1]])),file)

      }
  )

  output$control_2  <- renderPlotly({
      date_as_number = as.numeric(str_remove_all(input$control_calendar,"-"))
      ggplotly(plate_control_plots(loaded_files$Bridge_controls, loaded_files$Sample_controls, input$platecontrol_log, date_as_number)[[2]])
  })

  output$download_control_2_html <- downloadHandler(
      filename = "control_2.html",
      content = function(file) {
          date_as_number = as.numeric(str_remove_all(input$control_calendar,"-"))
          htmlwidgets::saveWidget(as_widget(
              ggplotly(plate_control_plots(loaded_files$Bridge_controls, loaded_files$Sample_controls, input$platecontrol_log, date_as_number)[[2]])),file)

      }
  )
  
  output$control_3  <- renderPlotly({
      date_as_number = as.numeric(str_remove_all(input$control_calendar,"-"))
      ggplotly(plate_control_plots(loaded_files$Bridge_controls, loaded_files$Sample_controls, input$platecontrol_log, date_as_number)[[3]])
  })

  output$download_control_3_html <- downloadHandler(
      filename = "control_3.html",
      content = function(file) {
          date_as_number = as.numeric(str_remove_all(input$control_calendar,"-"))
          htmlwidgets::saveWidget(as_widget(
              ggplotly(plate_control_plots(loaded_files$Bridge_controls, loaded_files$Sample_controls, input$platecontrol_log, date_as_number)[[3]])),file)

      }
  )

  ## TAB 5 KT3 ====
  output$KT3_Bridge  <- renderPlotly({
      date_as_number = as.numeric(str_remove_all(input$KT3_calendar,"-"))
      KT3_lineplot(loaded_files$Bridge_controls, input$KT3_log, date_as_number)
  })

  output$download_KT3_bridge_html <- downloadHandler(
      filename = "KT3_bridge.html",
      content = function(file) {
          date_as_number = as.numeric(str_remove_all(input$KT3_calendar,"-"))
          htmlwidgets::saveWidget(as_widget(
              KT3_lineplot(loaded_files$Bridge_controls, input$KT3_log, date_as_number)),file)

      }
  )

  ## TAB 6 GST====
  output$GST_Sample  <- renderPlotly({
      date_as_number = as.numeric(str_remove_all(input$GST_calendar,"-"))
      GST_bees(loaded_files$Sample, date_as_number)
  })

  output$download_GST_sample_html <- downloadHandler(
      filename = "GST_sample.html",
      content = function(file) {
          date_as_number = as.numeric(str_remove_all(input$GST_calendar,"-"))
          htmlwidgets::saveWidget(as_widget(
              GST_bees(loaded_files$Sample, date_as_number)),file)

      }
  )
  
  output$GST_Bridge  <- renderPlotly({
      date_as_number = as.numeric(str_remove_all(input$GST_calendar,"-"))
      GST_bees(loaded_files$Bridge, date_as_number)
  })

  output$download_GST_bridge_html <- downloadHandler(
      filename = "GST_bridge.html",
      content = function(file) {
          date_as_number = as.numeric(str_remove_all(input$GST_calendar,"-"))
          htmlwidgets::saveWidget(as_widget(
              GST_bees(loaded_files$Bridge, date_as_number)),file)

      }
  )
  
  ## TAB 7 Bridging Data====
  output$Mean_MFI_Bridging  <- renderPlotly({
      fix_jpeg_download(
      ggplotly(mean_median_lineplots(loaded_files$Bridge_mm,
                                         input$log_linear)[["Mean"]]),"MFI_bridge_mean")
  })

  output$download_MFI_bridge_mean_html <- downloadHandler(
      filename = "mean_MFI_bridge.html",
      content = function(file) {
          htmlwidgets::saveWidget(as_widget(
              ggplotly(mean_median_lineplots(loaded_files$Bridge_mm, input$log_linear)[["Mean"]])),file)

      }
  )

  output$Median_MFI_Bridging  <- renderPlotly({
      fix_jpeg_download(
      ggplotly(mean_median_lineplots(loaded_files$Bridge_mm,
                                          input$log_linear)[["Median"]]),"MFI_bridge_median")
  })

  output$download_MFI_bridge_median_html <- downloadHandler(
      filename = "median_MFI_bridge.html",
      content = function(file) {
          htmlwidgets::saveWidget(as_widget(
              ggplotly(mean_median_lineplots(loaded_files$Bridge_mm, input$log_linear)[["Median"]])),file)

      }
  )

  ## TAB 8 Sample MM ====
  
  output$Mean_MFI_perplate  <- renderPlotly({
      date_as_number = as.numeric(str_remove_all(input$Samples_calendar,"-"))
      Sample_mm_per_plate <- get_mean_median_per_plate(loaded_files$Sample, input$perplate_display, date_as_number)
      ggplotly(mm_per_plate_lineplots(Sample_mm_per_plate, input$mm_log_toggle)[["Mean"]])
  })

  output$download_MFI_perplate_mean_html <- downloadHandler(
      filename = "mean_MFI_bridge_pp.html",
      content = function(file) {
          date_as_number = as.numeric(str_remove_all(input$Samples_calendar,"-"))
          Sample_mm_per_plate <- get_mean_median_per_plate(loaded_files$Sample, input$perplate_display, date_as_number)
          htmlwidgets::saveWidget(as_widget(
              ggplotly(mm_per_plate_lineplots(Sample_mm_per_plate, input$mm_log_toggle)[["Mean"]])),file)

      }
  )

  output$Median_MFI_perplate  <- renderPlotly({
      date_as_number = as.numeric(str_remove_all(input$Samples_calendar,"-"))
      Sample_mm_per_plate <- get_mean_median_per_plate(loaded_files$Sample, input$perplate_display, date_as_number)
      ggplotly(mm_per_plate_lineplots(Sample_mm_per_plate, input$mm_log_toggle)[["Median"]])
  })

  output$download_MFI_perplate_median_html <- downloadHandler(
      filename = "mean_MFI_bridge_pp.html",
      content = function(file) {
          date_as_number = as.numeric(str_remove_all(input$Samples_calendar,"-"))
          Sample_mm_per_plate <- get_mean_median_per_plate(loaded_files$Sample, input$perplate_display, date_as_number)
          htmlwidgets::saveWidget(as_widget(
              ggplotly(mm_per_plate_lineplots(Sample_mm_per_plate, input$mm_log_toggle)[["Median"]])),file)

      }
  )

}


# Create Shiny app ----
shinyApp(ui = ui, server = server) # local
# shinyApp(ui = ui, server = server,options=list(host = "0.0.0.0", port=7201)) # dev.station
