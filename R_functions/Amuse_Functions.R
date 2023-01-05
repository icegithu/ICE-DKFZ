# # # # functions for Amuse Shiny
# Author: GK
# Reviewer: EV
# Date: Dec 2022
# Version: 1.0

read_in_sample_data <- function(path_to_file = path, Sample_info_file = Sample_info_file){
    
    # TODO this regex pattern might need some more work
    (Sample_plates_raw <- list.files(path = paste0(path_to_file, "/Rohdaten/"), pattern = "FM Platte .*\\D\\D\\D\\d\\d\\d.csv", recursive = T))
    
    if (length(Sample_plates_raw) == 0){ return()}
    # make an empty container
    Sample_df <- data.frame()
    
    for (x in Sample_plates_raw) {
        
        # x <- Sample_plates_raw[1]
        
        temp_sample_data <- read.csv(file = paste(path_to_file, "Rohdaten", x, sep="/"), 
                                     header = F, row.names = NULL, col.names = paste0("V",1:100))
        
        colnames_start <- which(grepl("Location", temp_sample_data$V1))[1]
        colnames(temp_sample_data) <- temp_sample_data[colnames_start,]
        head(temp_sample_data)
        
        # find median table
        median_table_start <- which(grepl("median", temp_sample_data$Sample, ignore.case = T))
        median_table <- temp_sample_data[(median_table_start+2):(median_table_start+97),]
        # Remove extra columns if any (some weird machine issues)
        last_col <- which(colnames(median_table) == "Total Events")
        median_table <- median_table[,1:last_col]
        head(median_table)
        
        # add Data-type
        median_table$Data_Type <- "MFI"
        
        # find count table
        count_table_start <- which(grepl("^count$", temp_sample_data$Sample, ignore.case = T))
        count_table <- temp_sample_data[(count_table_start+2):(count_table_start+97),]
        # Remove extra columns if any (some weird machine issues)
        last_col <- which(colnames(count_table) == "Total Events")
        count_table <- count_table[,1:last_col]
        head(count_table)
        
        # add Data-type
        count_table$Data_Type <- "Counts"
        head(count_table, 3)
        
        # Put MFI and Count tables together
        med_count <- rbind(median_table, count_table)
        
        # Find the log file for the current plate: 
        plate_x <- gsub(".csv", "", gsub(".* ", "", x))
        plate_x_log <- list.files(path_to_file, pattern = plate_x, recursive = T)
        plate_x_log <- plate_x_log[grepl("Log_Messages", plate_x_log, ignore.case = T)]
        
        # add the date based on the log file
        med_count$Date <- gsub("_.*", "", gsub(".*Log_Messages_", "", plate_x_log))
        
        # add the week number
        med_count$Week <- gsub("Woche", "Week_", str_extract(x, "Woche\\d+"))
        
        # read in the log file and add the delta T
        plate_x_log_data <- readxl::read_xls(path = paste0(path_to_file, plate_x_log))
        delta_start <- which(grepl("Delta Calibration Temp", plate_x_log_data$Message, ignore.case = T))[1]
        delta_temp <- as.numeric(str_extract(gsub("Temp ", "", str_extract(plate_x_log_data$Message[delta_start], "Temp .*C \\(")), ".*\\d"))
        
        # add the Delta T
        med_count$Delta_T <- delta_temp
        head(med_count)
        
        # add the sample from first cell to all the rest of in the same column
        med_count$Sample <- med_count$Sample[1]
        
        # Rename for later
        med_count <- med_count %>% 
            select(-matches("NA")) %>% 
            rename(Plate.ID = Sample, position = Location)
        
        # add the well number (might be useful in the future)
        # med_count$well <- str_extract(med_count$position, "[A-H]\\d\\d?")
        
        # add the position on the plate (basically like a well number)
        med_count$position <- as.numeric(gsub("\\(.*", "", med_count$position))
        
        # save the output into the container
        Sample_df <- rbind(Sample_df, med_count)
        
        # Fix rownames
        rownames(Sample_df) <- NULL
    }
    
    # Merge raw data with Sample info file using plate ID,  
    head(Sample_info_file)
    Sample_all <- left_join(Sample_df, Sample_info_file, by = c("Plate.ID", "position"))
    colnames(Sample_all) <- str_to_title(colnames(Sample_all))
    Sample_all <- Sample_all %>% relocate(c("Plate.id", "Position", "Well", "Sample.id" , "Data_type", "Week", "Date", "Delta_t", "Fortnr", "Plate.number.intern", "Study", 
                                            "Plate_daywise", "Assay.day", "Assay.date", "Comment"))
    head(Sample_all)
    
    # save the data
    start_date <- min(unique(Sample_all$Date))
    end_date <- max(unique(Sample_all$Date))
    
    (filename <- paste0(path_to_file, "Combined_Output/Sample_data_", 
                        unique(Sample_all$Week), "_", start_date, "-", end_date, ".csv"))
    
    # Make the output dir if not there yet
    if (!dir.exists(paste0(path_to_file, "Combined_Output"))) {dir.create(paste0(path_to_file, "Combined_Output"), recursive = T)}
    
    write_csv(x = Sample_all, file = filename)
    
    print(paste("Sample data collected and saved under:", filename))
}


read_in_bridging_data <- function(path_to_file = path, Bridge_info_file = Bridge_info_file){
    
    # Read in bridging plates  
    # TODO this regex pattern might need some more work
    (Bridge_plates_raw <- list.files(path = paste0(path_to_file, "/Rohdaten/"), pattern = "FM Platte \\d\\d\\d\\d.*csv", recursive = T))
    
    if (length(Bridge_plates_raw) == 0){ return()}
    # make an empty container
    Bridge_df <- data.frame()
    
    for (x in Bridge_plates_raw) {
        
        # x <- Bridge_plates_raw[1]
        
        temp_bridge_data <- read.csv(file = paste(path_to_file, "Rohdaten", x, sep="/"), 
                                     header = F, row.names = NULL, col.names = paste0("V",1:100))
        head(temp_bridge_data)
        
        (colnames_start <- which(grepl("Location", temp_bridge_data$V1))[1])
        colnames(temp_bridge_data) <- temp_bridge_data[colnames_start,]
        head(temp_bridge_data)
        
        # median starts from 1st row
        (median_start <- which(grepl("median", temp_bridge_data$Sample, ignore.case = T)))
        bridge_median_df <- temp_bridge_data[(median_start+2):(median_start+97),]
        bridge_median_df$Data_Type <- "MFI"
        
        # count 
        count_start <- which(grepl('^count$', temp_bridge_data$Sample, ignore.case = T))
        bridge_count_table <- temp_bridge_data[(count_start+2):(count_start+97),]
        bridge_count_table$Data_Type <- "Counts"
        
        # Put those MFI and Count tables together
        Bridge_med_count <- rbind(bridge_median_df, bridge_count_table)
        
        #####################################
        # Find the log file for this plate: 
        bridge_x <- str_extract(x, "FM Platte \\d\\d\\d\\d")
        bridge_x_log <- list.files(path_to_file, pattern = bridge_x, recursive = T)
        (bridge_x_log <- bridge_x_log[grepl("Log_Messages", bridge_x_log, ignore.case = T)])
        
        # add the date based on the log file
        Bridge_med_count$Date <- gsub("_.*", "", gsub(".*Log_Messages_", "", bridge_x_log))
        
        # add the week number
        Bridge_med_count$Week <- gsub("Woche", "Week_", str_extract(bridge_x_log, "Woche\\d+"))
        
        # read in the log file and add the delta T
        Bridge_plate_x_log_data <- readxl::read_xls(path = paste0(path_to_file, bridge_x_log))
        delta_start <- which(grepl("Delta Calibration Temp", Bridge_plate_x_log_data$Message, ignore.case = T))[1]
        delta_temp <- as.numeric(str_extract(gsub("Temp ", "", str_extract(Bridge_plate_x_log_data$Message[delta_start], "Temp .*C \\(")), ".*\\d"))
        
        # add the Delta T
        Bridge_med_count$Delta_T <- delta_temp
        
        # add the sample from first cell to all the rest of in the same column
        Bridge_med_count$Sample <- Bridge_med_count$Sample[1]
        
        # Rename for later
        Bridge_med_count <- Bridge_med_count %>% 
            select(-matches("NA")) %>% 
            rename(Plate.ID = Sample, position = Location)
        
        # add the well number (might be useful in the future)
        # med_count$well <- str_extract(med_count$position, "[A-H]\\d\\d?")
        
        # add the position on the plate (basically like a well number)
        Bridge_med_count$position <- as.numeric(gsub("\\(.*", "", Bridge_med_count$position))
        head(Bridge_med_count)
        
        # Save data into one container
        Bridge_df <- rbind(Bridge_df, Bridge_med_count)
        
        rownames(Bridge_df) <- NULL
        
    }
    
    Bridge_all <- left_join(Bridge_df, Bridge_info_file, by = c("Plate.ID", "position"))
    colnames(Bridge_all) <- str_to_title(colnames(Bridge_all))
    
    # Merge raw data with Sample info file using plate ID,  
    head(Bridge_all)
    Bridge_all <- Bridge_all %>% relocate(c("Plate.id", "Position", "Well", "Sample.id" , "Data_type", 
                                            "Week", "Date", "Delta_t", "Fortnr", "Plate.number.intern", 
                                            "Study", "Plate", "Assay.day", "Assay.date", "Comment"))
    head(Bridge_all,2)
    
    Bridge_all <- Bridge_all %>% relocate(contains("Analyte"), .after = last_col())
    
    # save the data
    (start_date <- min(unique(Bridge_all$Date)))
    (end_date <- max(unique(Bridge_all$Date)))
    
    (filename <- paste0(path_to_file, "Combined_Output/Bridging_data_", 
                        unique(Bridge_all$Week), "_", start_date, "-", end_date, ".csv"))
    
    # Make the output dir if not there yet
    if (!dir.exists(paste0(path_to_file, "Combined_Output"))) {dir.create(paste0(path_to_file, "Combined_Output"), recursive = T)}
    
    write_csv(x = Bridge_all, file = filename)
    
    print(paste("Bridging data collected and saved under:", filename))
}

# Figure 1 – Mean/Median MFI Lineplots =========================================
# Bridging data only + Log/linear toggle

# get mean and median. Works on both Sample and Bridging data
get_mean_median <- function(df){
    
    # df <- bridge_data # debug
    # head(df)
    
    start <- which(colnames(df) == "Comment")
    analyte_cols <- colnames(df)[(start + 1) : ncol(df)]
    analyte_cols
    
    final_df <- 
        df %>% 
        select(Plate.id, Date, Sample.id, Data_type, matches(analyte_cols)) %>%
        pivot_longer(cols = matches(analyte_cols), names_to = "Analyte", values_to = "Value") %>%
        group_by(Plate.id, Date, Analyte, Data_type) %>% 
        summarise(Mean = mean(Value, na.rm = T),
                  Median = median(Value,  na.rm = T)) %>% 
        pivot_wider(names_from = Data_type, values_from = c(Mean, Median))
    
    return(final_df)
    
}

# AESTHETICS ===================================================================
theme_set(
    theme_classic() +
        theme(plot.title = element_text(hjust = 0.5, size = rel(1.5)),
              axis.text.x = element_text(angle = 45, hjust = 1))
)


# Draw the mean median MFI lineplots
mean_median_lineplots <- function(df, log_toggle){
    
    # df <- bridge_df_mm #debug
    head(df)
    
    df$Date <- factor(df$Date)
    df$Analyte <- factor(df$Analyte)
    df$Analyte <- fct_relevel(df$Analyte, c("Gst.Tag", "Total.Events"), after = Inf)
    
    out_list <- list()
    # Draw median MFI lineplot
    out_list[["Mean"]] <-
        ggplot(df, aes(x = Analyte, y = Mean_MFI, color = Date, group = Date)) + 
        geom_line(linewidth = 1) + geom_point() + labs(x = "", y = "Mean MFI")
    
    # Draw median MFI lineplot
    out_list[["Median"]] <-
        ggplot(df, aes(x = Analyte, y = Median_MFI, color = Date, group = Date)) + 
        geom_line(linewidth = 1) + geom_point() + labs(x = "", y = "Median MFI")
    
    if (log_toggle) {
        
        out_list[["Mean"]] <- out_list[["Mean"]] + 
            scale_y_log10("Log10(Mean MFI)",
                          breaks = scales::trans_breaks("log10", function(x) 10^x),
                          labels = scales::trans_format("log10", math_format(10^.x))) +
            annotation_logticks(sides = "l")
        
        
        out_list[["Median"]] <- out_list[["Median"]] + 
            scale_y_log10("Log10(Median MFI)",
                          breaks = scales::trans_breaks("log10", function(x) 10^x),
                          labels = scales::trans_format("log10", math_format(10^.x))) +
            annotation_logticks(sides = "l")
    }
    
    return(out_list)
    
}

# Figure 2 – Mean/Median Counts Boxplots =======================================
# Bridging and Sample data

# Draw the mean median boxplots function
mean_boxplots <- function(df, selected_date = ""){
    
    # df <- sample_df_mm #debug
    # selected_date <- unique(df$Date)[1] #debug
    
    # TODO we might wanna use if missing so that if no date is specified we select all of them
    # if (selected_date == "") {}
    
    df <- df %>% filter(Analyte != "Total.Events" & Date == selected_date)
    
    # Draw Mean Boxes
    Mean_box_plot <-
        ggplot(df, aes(x = Analyte, y = Mean_Counts)) + 
        geom_boxplot() + labs(x = "", y = "Mean Counts")
    
    plotly_box <- ggplotly(Mean_box_plot) %>% 
        layout(annotations = list(list(showarrow = FALSE, yref = "paper", xref = "paper", y = 1, x = 1, 
                                       text = paste("Showing data from", selected_date))))
    
    return(fix_jpeg_download(plotly_box),"Counts")
}

# Figure 3 – Blank MFI Boxplots =================================================
# Bridging and Sample data

# get blank and KT-3 samples only. Works on both Sample and Bridging data
get_blanks_kt <- function(df){
    
    # df <- sample_data # debug
    
    start <- which(colnames(df) == "Comment")
    analyte_cols <- colnames(df)[(start + 1) : ncol(df)]
    analyte_cols
    
    final_df <- 
        df %>% filter(grepl("blank|KT3", Sample.id) & Data_type == "MFI") %>% 
        select(Plate.id, Date, Sample.id, matches(analyte_cols)) %>%
        pivot_longer(cols = matches(analyte_cols), names_to = "Analyte", values_to = "MFI") %>% 
        filter(Analyte != "Total.Events")
    
    return(final_df)
    
}

# Draw the blank boxplots function
blank_bees <- function(df){
    
    # df <- sample_blanks_kt # debug
    df <- df %>% filter(Sample.id == "blank")
    df$Plate.id <- as.character(df$Plate.id)
    df$Analyte <- factor(df$Analyte)
    df$Analyte <- fct_relevel(df$Analyte, c("Gst.Tag"), after = Inf)
    
    plot <-
        ggplot(df, aes(x = Analyte, y = MFI, color = Plate.id)) + 
        geom_beeswarm() + labs(color = "Plate.ID", x = "")
    
    return(fix_jpeg_download(ggplotly(plot),"blanks"))
    
}    

# Figure 4 – Delta-T Dotplots ==================================================
# Bridging and Sample data

# draw the Delta-T point plots function
delta_t_pointplot <- function(df1 = sample_data, df2 = bridge_data){
    
    # df1 <- sample_data #debug
    # df2 <- bridge_data #debug
    
    # Subset data
    delta_df1 <- df1 %>% 
        dplyr::select(Plate.id, Date, Delta_t) %>% 
        distinct()
    
    # Subset data    
    delta_df2 <- df2 %>% 
        dplyr::select(Plate.id, Date, Delta_t) %>% 
        distinct()
    
    # Add type
    delta_df1$Type <- "Sample Plates"
    delta_df2$Type <- "Bridging Plate"
    
    # Combine
    combo_df <- rbind(delta_df1, delta_df2)
    
    plot <-
        ggplot(combo_df, aes(x = Date, y = Delta_t, shape = Type, color = Plate.id)) +
        geom_beeswarm(size = 1) + 
        coord_cartesian(ylim = c(-2.5, 2.5))+
        geom_hline(yintercept = 0, linetype = "longdash")+
        labs(x = "", y = "Delta T (\u00B0C)", shape = "Plate Type", color = "Plate ID")
    
    return(fix_jpeg_download(ggplotly(plot),"Temperature_plot"))
}

# Figure 5 – Mean and Median MFI per plate Lineplots ===========================
# get mean and median per plate function.

get_mean_median_per_plate <- function(df){
    
    # df <- sample_data #debug
    # head(df)
    
    start <- which(colnames(df) == "Comment")
    analyte_cols <- colnames(df)[(start + 1) : ncol(df)]
    analyte_cols
    
    final_df <- df %>% 
        filter(Data_type == "MFI") %>% 
        select(Plate.id, Date, Week, Plate_daywise, matches(analyte_cols)) %>%
        pivot_longer(cols = matches(analyte_cols), names_to = "Analyte", values_to = "MFI") %>%
        group_by(Plate.id, Date, Week, Plate_daywise, Analyte) %>% 
        summarise(Mean_MFI = mean(MFI),
                  Median_MFI = median(MFI))
    
    return(final_df)
    
}

# draw the Delta-T point plots function
mm_per_plate_lineplots <- function(df, x_axis = x_axis){
    
    # df <- sample_df_mm_per_plate # Debug
    
    df <- df %>% ungroup() %>% filter(Analyte != "Total.Events") %>% 
        mutate(across(c(Date, Plate_daywise), factor))
    
    out_list <- list()
    
    # Draw Lineplot Mean platewise 
    out_list[["Mean"]] <-
        ggplot(df, aes(x = get(x_axis), y = Mean_MFI, color = Analyte, group = Analyte)) + 
        geom_line(linewidth = 1) + geom_point() + 
        scale_x_discrete(expand = expansion(mult = c(0.05, 0.05))) +
        labs(x = "", y = "Mean MFI")
    
    # Draw Lineplot Mean platewise 
    out_list[["Median"]] <-
        ggplot(df, aes(x = get(x_axis), y = Median_MFI, color = Analyte, group = Analyte)) + 
        geom_line(linewidth = 1) + geom_point() + 
        scale_x_discrete(expand = expansion(mult = c(0.05, 0.05))) +
        labs(x = "", y = "Median MFI")
    
    return(out_list)
    
}

# Figure 6 – KT-3 Lineplots ====================================================
# TODO Will need a Mean over plate.id/date probably

# Draw the KT-3 pointplots function
KT3_lineplot <- function(df){
    
    # df <- sample_blanks_kt # debug
    
    df <- df %>% filter(Sample.id == "KT3")# %>%
    df$Date <- factor(df$Date)
    df$Analyte <- factor(df$Analyte)
    df$Analyte <- fct_relevel(df$Analyte, "Gst.Tag", after = Inf)
    
    plot <-
        ggplot(df, aes(x = Date, y = MFI, group = Analyte, color = Analyte)) + 
        geom_line(linewidth = 1) + geom_point() +
        scale_x_discrete(expand = expansion(mult = c(0.05, 0.05))) +
        labs(x = "", y = "KT-3 MFI", color = "Analyte") 
    
    return(fix_jpeg_download(remove_hover_duplicate(ggplotly(plot)),"KT3_Plot"))
    
}    

# Figure 7 – GST Beeswarm ======================================================
# Draw the GST bees function
GST_bees <- function(df){
    
    # df <- bridge_data # debug
    
    df <- df %>% filter(Data_type == "MFI")
    df$Plate.id <- as.character(df$Plate.id)
    df$Date <- factor(df$Date)
    
    plot <-
        ggplot(df, aes(x = Date, y = Gst.Tag, color = Plate.id)) + 
        geom_beeswarm(size = 2) + labs(x = "", y = "GST Tag", color = "Plate ID") 
    
    return(fix_jpeg_download(ggplotly(plot),"GST_Bridge"))
    
}    

GST_violins <- function(df){
    
    # df <- sample_data # debug
    
    df <- df %>% filter(Data_type == "MFI")
    df$Plate.id <- as.character(df$Plate.id)
    df$Plate_daywise <- as.character(df$Plate_daywise)
    df$Date <- factor(df$Date)
    head(df)
    
    plot <-
        ggplot(df, aes(x = Plate_daywise, y = Gst.Tag, fill = Plate_daywise)) + 
        geom_violin(width=0.7, alpha = 0.5) + 
        geom_boxplot(width=0.2, show.legend = F) +
        labs(x = "", y = "GST Tag", fill = "Plate No. Daywise") + 
        facet_grid(~ Date) +
        theme(strip.background = element_blank(), 
              strip.text.x = element_text(size = rel(1.3)))
    
    return(fix_jpeg_download(ggplotly(plot),"GST_Sample"))
    
}    


get_avaliable_dates <- function(summary_dir){
    # get all files available
    files_list <- list.files(summary_dir)
    # get dates from those files
    # date_list <- unique(str_extract(files_list, "\\d\\d\\.\\d\\d?\\.\\d\\d\\d\\d")) # old file date format
    # new date format
    date_list <- unique(str_extract(files_list, "\\d\\d\\d\\d\\d\\d\\d\\d"))
    date_list <- date_list[!is.na(date_list)]
    return(date_list)
}

remove_hover_duplicate <- function(p){
    for (i in 1:length(p[["x"]][["data"]])){
        points_in_line <- p[["x"]][["data"]][[i]][["text"]]
        for (j in 1:length(points_in_line)){
            initial_text <- p[["x"]][["data"]][[i]][["text"]][[j]] 
            text_as_list <- str_split(initial_text, "<br")
            if (length(text_as_list)>0){
                p[["x"]][["data"]][[i]][["text"]][[j]] <- paste(unique(text_as_list[[1]]),collapse = "<br")
            }
        }
    }
    return(p)
}

remove_parenthesis_legend <- function(p){
    for (i in 1:length(p[["x"]][["data"]])){
        name <- p[["x"]][["data"]][[i]][["name"]]
        p[["x"]][["data"]][[i]][["name"]] <- str_remove_all(str_remove_all(name,"\\("),"\\)")
    }
    return(p)
}

fix_jpeg_download <- function(p, filename){
    p <- p%>% config(toImageButtonOptions = list(format= 'jpeg', 
                                                 filename= filename,
                                                 height= 800,
                                                 width= 1100,
                                                 scale= 1 ))
    return(p)
}
