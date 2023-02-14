# # # # functions for Amuse Shiny
# Author: GK
# Reviewer: EV
# Date: Dec 2022
# Version: 1.5

read_in_sample_data <- function(path_to_file = path, Sample_info_file = Sample_info_file){
    
    # Get mod date and filenames of already available summary data
    sample_summaries <- list.files(paste0(path_to_file, "/Combined_Output/"))
    sample_summaries <- sample_summaries[grepl("Sample", sample_summaries)]
    mod_times_summaries <- file.mtime(paste0(path_to_file, "/Combined_Output/", sample_summaries))
    
    if (length(sample_summaries) == 0) {
        sample_summaries <- "empty"
        mod_times_summaries <- as.POSIXct("1900-01-01 10:30:00 CET")
    }
    
    # Make a Df out of it
    summarized_data <- data.frame(Filenames = sample_summaries, modification = mod_times_summaries, week = str_extract(sample_summaries, "Woche\\d\\d?"))
    
    # get raw data files location
    (current_path <- paste0(path_to_file, "/Rohdaten/"))
    # read in only sample files
    (Sample_plates_raw <- list.files(path = current_path, pattern = "FM Platte .*\\D\\D\\D\\d\\d\\d.csv", recursive = T))
    mod_times_files <- file.mtime(paste0(current_path, Sample_plates_raw))
    # Make a Df out of it
    (raw_data <- data.frame(Filenames = Sample_plates_raw, mod.time = mod_times_files, week = str_extract(Sample_plates_raw, "Woche\\d\\d?")))
    
    # match modifications date from summarized data to raw data based on week number
    raw_data$available_data_date <- summarized_data$modification[match(raw_data$week, summarized_data$week)]
    
    # Check if the available data is older
    # True value means that the datafile needs to be read in
    raw_data$needs_updating <- raw_data$mod.time > raw_data$available_data_date
    # If no data is available then NA, so convert them to True as well
    raw_data$needs_updating[is.na(raw_data$needs_updating)] <- T
    # Get the list of weeks to read in
    (to_read_in_weeks <- unique(raw_data$week[raw_data$needs_updating]))
    
    if (length(to_read_in_weeks) == 0) {
        return("Nothing to update")
    }
    
    files_updated_text <- c()
    
    for (week in seq_along(to_read_in_weeks)) {
        
        # week <- 1 # debug
        
        (current_path_samples <- list.dirs(paste0(paste0(path_to_file, "/Rohdaten/"))))
        (current_path_samples <- current_path_samples[grepl(to_read_in_weeks[week], current_path_samples)])
        
        # TODO this regex pattern might need some more work
        (Sample_plates_raw <- list.files(path = current_path_samples, pattern = "FM Platte .*\\D\\D\\D\\d\\d\\d.csv", recursive = T))
        
        if (length(Sample_plates_raw) == 0){ return()}
        
        # make an empty container
        Sample_df <- data.frame()
        
        for (x in Sample_plates_raw) {
            
            # x <- Sample_plates_raw[1]
            
            temp_sample_data <- read.csv(file = paste(current_path_samples, x, sep="/"), 
                                         header = F, row.names = NULL, col.names = paste0("V",1:100))
            
            colnames_start <- which(grepl("Location", temp_sample_data$V1))[1]
            colnames(temp_sample_data) <- temp_sample_data[colnames_start,]
            head(temp_sample_data,2)
            
            # find median table
            median_table_start <- which(grepl("median", temp_sample_data$Sample, ignore.case = T))
            median_table <- temp_sample_data[(median_table_start+2):(median_table_start+97),]
            # Remove extra columns if any (some weird machine issues)
            last_col <- which(colnames(median_table) == "Total Events")
            median_table <- median_table[,1:last_col]
            head(median_table)
            
            # add Data-type
            median_table$Data.Type <- "MFI"
            
            # find count table
            count_table_start <- which(grepl("^count$", temp_sample_data$Sample, ignore.case = T))
            count_table <- temp_sample_data[(count_table_start+2):(count_table_start+97),]
            # Remove extra columns if any (some weird machine issues)
            last_col <- which(colnames(count_table) == "Total Events")
            count_table <- count_table[,1:last_col]
            head(count_table)
            
            # add Data-type
            count_table$Data.Type <- "Counts"
            head(count_table, 3)
            
            # Put MFI and Count tables together
            med_count <- rbind(median_table, count_table)
            
            # Find the log file for the current plate: 
            plate_x <- gsub(".csv", "", gsub(".* ", "", x))
            
            plate_x_log <- list.files(current_path_samples, pattern = plate_x, recursive = T)
            plate_x_log <- plate_x_log[grepl("Log_Messages", plate_x_log, ignore.case = T)]
            
            # add the date based on the log file
            med_count$Date <- gsub("_.*", "", gsub(".*Log_Messages_", "", plate_x_log))
            
            # add the week number
            # med_count$Week <- gsub("Woche", "Week_", str_extract(x, "Woche\\d+"))
            med_count$Week <- str_extract(current_path_samples, "Woche\\d+")
            
            # read in the log file and add the delta T
            plate_x_log_data <- readxl::read_xls(path = paste(current_path_samples, plate_x_log, sep = "/"))
            delta_start <- which(grepl("Delta Calibration Temp", plate_x_log_data$Message, ignore.case = T))[1]
            delta_temp <- as.numeric(str_extract(gsub("Temp ", "", str_extract(plate_x_log_data$Message[delta_start], "Temp .*C \\(")), ".*\\d"))
            
            # add the Delta T
            med_count$Delta.T <- delta_temp
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
        
        dim(Sample_df)
        
        # Merge raw data with Sample info file using plate ID,  
        head(Sample_info_file)
        Sample_all <- left_join(Sample_df, Sample_info_file, by = c("Plate.ID", "position"))
        # Fix Col names
        colnames(Sample_all) <- gsub(" |_", ".", colnames(Sample_all))
        substr(colnames(Sample_all), 1, 1) <- toupper(substr(colnames(Sample_all), 1, 1))
        colnames(Sample_all)
        
        Sample_all <- Sample_all %>% relocate(c("Plate.ID", "Position", "Well", "Sample.ID" , "Data.Type", "Week", "Date", "Delta.T", "FortNr", "Plate.number.intern", "Study", 
                                                "Plate.daywise", "Assay.day", "Assay.date", "Comment"))
        head(Sample_all)
        
        # save the data
        start_date <- min(unique(Sample_all$Date))
        end_date <- max(unique(Sample_all$Date))
        
        (filename <- paste0(path_to_file, "/Combined_Output/Sample_data_", 
                            unique(Sample_all$Week), "_", start_date, "-", end_date, ".csv"))
        
        # Make the output dir if not there yet
        if (!dir.exists(paste(path_to_file, "Combined_Output", sep = "/"))) {dir.create(paste(path_to_file, "Combined_Output", sep = "/"), recursive = T)}
        
        write_csv(x = Sample_all, file = filename)
        
        files_updated_text<- c(files_updated_text,(paste(to_read_in_weeks[week], "- Sample data collected and saved under:", filename)))
        
    }
    return(files_updated_text)
}

read_in_bridging_data <- function(path_to_file = path, Bridge_info_file = Bridge_info_file){
    
    # Get mod date and filenames of already available summary data
    bridging_summaries <- list.files(paste0(path_to_file, "/Combined_Output/"))
    bridging_summaries <- bridging_summaries[grepl("Bridging", bridging_summaries)]
    mod_times_summaries <- file.mtime(paste0(path_to_file, "/Combined_Output/", bridging_summaries))
    
    if (length(bridging_summaries) == 0) {
        bridging_summaries <- "empty"
        mod_times_summaries <- as.POSIXct("1900-01-01 10:30:00 CET")
    }
    
    # Make a Df out of it
    summarized_data <- data.frame(Filenames = bridging_summaries, modification = mod_times_summaries, week = str_extract(bridging_summaries, "Woche\\d\\d?"))
    
    # get raw data files location
    (current_path <- paste0(path_to_file, "/Rohdaten/"))
    # read in only sample files
    (Bridging_plates_raw <- list.files(path = current_path, pattern = "FM Platte \\d\\d\\d\\d.*csv", recursive = T))
    mod_times_files <- file.mtime(paste0(current_path, Bridging_plates_raw))
    # Make a Df out of it
    (raw_data <- data.frame(Filenames = Bridging_plates_raw, mod.time = mod_times_files, week = str_extract(Bridging_plates_raw, "Woche\\d\\d?")))
    
    # match modifications date from summarized data to raw data based on week number
    raw_data$available_data_date <- summarized_data$modification[match(raw_data$week, summarized_data$week)]
    
    # Check if the available data is older
    # True value means that the datafile needs to be read in
    raw_data$needs_updating <- raw_data$mod.time > raw_data$available_data_date
    # If no data is available then NA, so convert them to True as well
    raw_data$needs_updating[is.na(raw_data$needs_updating)] <- T
    # Get the list of weeks to read in
    (to_read_in_weeks <- unique(raw_data$week[raw_data$needs_updating]))
    
    if (length(to_read_in_weeks) == 0) {
        return("Nothing to update")
    }
    files_updated_text <- c()
    
    for (week in seq_along(to_read_in_weeks)) {
        
        # week <- 1 # debug
        
        (current_path <- list.dirs(paste0(paste0(path_to_file, "/Rohdaten/"))))
        (current_path <- current_path[grepl(to_read_in_weeks[week], current_path)])
        
        # TODO this regex pattern might need some more work
        (Bridge_plates_raw <- list.files(path = current_path, pattern = "FM Platte \\d\\d\\d\\d.*csv", recursive = T))
        if (length(Bridge_plates_raw) == 0){ return()}
        
        # make an empty container
        Bridge_df <- data.frame()
        
        # Read in bridging plates  
        
        for (x in Bridge_plates_raw) {
            
            # x <- Bridge_plates_raw[1] #debug
            
            temp_bridge_data <- read.csv(file = paste(current_path, x, sep="/"), 
                                         header = F, row.names = NULL, col.names = paste0("V",1:100))
            head(temp_bridge_data)
            
            (colnames_start <- which(grepl("Location", temp_bridge_data$V1))[1])
            colnames(temp_bridge_data) <- temp_bridge_data[colnames_start,]
            head(temp_bridge_data)
            
            # median starts from 1st row
            (median_start <- which(grepl("median", temp_bridge_data$Sample, ignore.case = T)))
            bridge_median_df <- temp_bridge_data[(median_start+2):(median_start+97),]
            bridge_median_df$Data.Type <- "MFI"
            
            # count 
            count_start <- which(grepl('^count$', temp_bridge_data$Sample, ignore.case = T))
            bridge_count_table <- temp_bridge_data[(count_start+2):(count_start+97),]
            bridge_count_table$Data.Type <- "Counts"
            
            # Put those MFI and Count tables together
            Bridge_med_count <- rbind(bridge_median_df, bridge_count_table)
            
            # Find the log file for this plate: 
            bridge_x <- str_extract(x, "FM Platte \\d\\d\\d\\d")
            bridge_x_log <- list.files(current_path, pattern = bridge_x, recursive = T)
            (bridge_x_log <- bridge_x_log[grepl("Log_Messages", bridge_x_log, ignore.case = T)])
            
            # add the date based on the log file
            Bridge_med_count$Date <- gsub("_.*", "", gsub(".*Log_Messages_", "", bridge_x_log))
            
            # add the week number
            Bridge_med_count$Week <- str_extract(current_path, "Woche\\d+")
            
            # read in the log file and add the delta T
            Bridge_plate_x_log_data <- readxl::read_xls(path = paste(current_path, bridge_x_log, sep = "/"))
            delta_start <- which(grepl("Delta Calibration Temp", Bridge_plate_x_log_data$Message, ignore.case = T))[1]
            delta_temp <- as.numeric(str_extract(gsub("Temp ", "", str_extract(Bridge_plate_x_log_data$Message[delta_start], "Temp .*C \\(")), ".*\\d"))
            
            # add the Delta T
            Bridge_med_count$Delta.T <- delta_temp
            
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
        
        # Merge raw data with Bridge info file using plate ID and position (redundant)
        Bridge_all <- left_join(Bridge_df, Bridge_info_file, by = c("Plate.ID", "position"))
        # Fix Col names
        colnames(Bridge_all) <- gsub(" |_", ".", colnames(Bridge_all))
        substr(colnames(Bridge_all), 1, 1) <- toupper(substr(colnames(Bridge_all), 1, 1))
        colnames(Bridge_all)
        # add Plate.daywise col for later. It's just the plate.ID repeated for bridging data
        Bridge_all <- Bridge_all %>% rename(Plate.daywise = Plate) %>%
            mutate(Plate.daywise = as.numeric(Plate.ID)) %>%
            relocate(c("Plate.ID", "Position", "Well", "Sample.ID" , "Data.Type", "Week", "Date", "Delta.T", 
                       "FortNr", "Plate.number.intern", "Study", 
                       "Plate.daywise", "Assay.day", "Assay.date", "Comment"))
        
        # Change NAs to Empty for ggplot reasons
        Bridge_all$Sample.ID[is.na(Bridge_all$Sample.ID)] <- "empty"
        head(Bridge_all, 2)
        
        # save the data
        (start_date <- min(unique(Bridge_all$Date)))
        (end_date <- max(unique(Bridge_all$Date)))
        
        (filename <- paste0(path_to_file, "/Combined_Output/Bridging_data_", 
                            unique(Bridge_all$Week), "_", start_date, "-", end_date, ".csv"))
        
        # Make the output dir if not there yet
        if (!dir.exists(paste(path_to_file, "Combined_Output", sep = "/"))) {dir.create(paste0(path_to_file, "Combined_Output", sep = "/"), recursive = T)}
        
        write_csv(x = Bridge_all, file = filename)
        
        files_updated_text <- c(files_updated_text,(paste(to_read_in_weeks[week], "- Bridging data collected and saved under:", filename)))
    }
    return(files_updated_text)
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
        select(Plate.ID, Date, Sample.ID, Data.Type, matches(analyte_cols)) %>%
        pivot_longer(cols = matches(analyte_cols), names_to = "Analyte", values_to = "Value") %>%
        group_by(Plate.ID, Date, Analyte, Data.Type) %>% 
        summarise(Mean = mean(Value, na.rm = T),
                  Median = median(Value,  na.rm = T)) %>% 
        pivot_wider(names_from = Data.Type, values_from = c(Mean, Median))
    
    final_df$Analyte <- factor(final_df$Analyte, levels = analyte_cols)
    
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
    
    df <- df %>% filter(!grepl("Total.Events", Analyte, ignore.case = T)) %>% 
        ungroup() %>% 
        mutate(across(c(Date), factor))
    
    # df$Analyte <- fct_relevel(df$Analyte, c("Gst Tag"), after = Inf)
    
    out_list <- list()
    # Draw median MFI lineplot
    out_list[["Mean"]] <-
        ggplot(df, aes(x = Analyte, y = Mean_MFI, color = Date)) + 
        geom_line(linewidth = 1, aes(group = Date)) + geom_point() + labs(x = "", y = "Mean MFI")
    
    # Draw median MFI lineplot
    out_list[["Median"]] <-
        ggplot(df, aes(x = Analyte, y = Median_MFI, color = Date)) + 
        geom_line(linewidth = 1, aes(group = Date)) + geom_point() + labs(x = "", y = "Median MFI")
    
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
    
    df <- df %>% filter(!grepl("Total.Events", Analyte, ignore.case = T) & 
                            grepl(paste(selected_date, collapse = "|"), Date)) %>%
        ungroup() %>%
        mutate(across(c(Date, Analyte), factor))
    
    # Draw Mean Boxes
    Mean_box_plot <-
        ggplot(df, aes(x = Analyte, y = Mean_Counts)) + 
        geom_boxplot(aes(fill = Date)) + 
        labs(x = "", y = "Mean Counts") +
        geom_hline(yintercept = 80, linetype = "longdash", color = "red") +
        geom_hline(yintercept = 100, linetype = "longdash", color = "orange")
    
    plotly_box <- ggplotly(Mean_box_plot) %>% 
        layout(boxmode = "group")
    
    return(fix_jpeg_download(plotly_box,"Counts"))
}

# Figure 3 – Blank MFI Boxplots =================================================
# Bridging and Sample data

# get blank and KT-3 samples only. Works on both Sample and Bridging data
get_controls <- function(df){
    
    # df <- sample_data # debug
    
    start <- which(colnames(df) == "Comment")
    analyte_cols <- colnames(df)[(start + 1) : ncol(df)]
    analyte_cols
    head(df)
    
    final_df <- 
        df %>% filter(grepl("blank|KT3|Plattenkontrolle", Sample.ID) & Data.Type == "MFI") %>% 
        select(Plate.ID, Date, Well, Sample.ID, Plate.number.intern, matches(analyte_cols)) %>%
        pivot_longer(cols = matches(analyte_cols), names_to = "Analyte", values_to = "MFI") %>%
        ungroup() %>%
        mutate(across(c(Date, Analyte, Plate.ID), factor))
    
    final_df$Analyte <- factor(final_df$Analyte, levels = analyte_cols)
    
    return(final_df)
    
}

blank_lines <- function(df, selected_date = ""){
    
    # df <- bridge_controls # debug
    # df <- sample_controls # debug
    # selected_date <- unique(df$Date)[1] #debug
    # head(df)
    
    df <- df %>% filter(Sample.ID == "blank" & !grepl("Total.Events", Analyte, ignore.case = T) &
                            grepl(paste(selected_date, collapse = "|"), Date))
    
    plot <-
        ggplot(df, aes(x = Analyte, y = MFI, color = Plate.ID, label = Plate.number.intern)) + 
        geom_line(linewidth = 1, aes(group = Plate.ID), position = position_dodge(rel(0.5))) + 
        geom_point(position = position_dodge(rel(0.5))) + 
        labs(color = "Plate.ID", x = "")
    
    return(fix_jpeg_download(ggplotly(plot), "blanks"))
    
}    

blank_bees <- function(df, selected_date = ""){
    
    # df <- sample_controls # debug
    # selected_date <- unique(df$Date)[1:4] #debug
    # head(df)
    
    df <- df %>% filter(Sample.ID == "blank" & !grepl("Total.Events", Analyte, ignore.case = T) &
                            grepl(paste(selected_date, collapse = "|"), Date))
    
    df_median <- df %>% group_by(Analyte) %>%
        summarise(MFI = median(MFI, na.rm = T))
    
    plot <-
        ggplot(df, aes(x = Analyte, y = MFI)) + 
        # stat_summary(fun = mean, geom = "crossbar", width = 0.5, color = "black") # this would be so much better but ggplotly doesn't like it :'( 
        geom_beeswarm(cex = rel(0.5), aes(color = Plate.ID, label = Date)) +
        geom_crossbar(data = df_median, width = rel(0.5), size = rel(0.5), aes(ymin = MFI, ymax = MFI), show.legend = F, color = "black")+
        labs(color = "Plate.ID", x = "")
    
    
    return(fix_jpeg_download(ggplotly(plot), "blanks"))
    
}    

# Figure 4 – Delta-T Dotplots ==================================================
# Bridging and Sample data

# draw the Delta-T point plots function
delta_t_pointplot <- function(df1 = sample_data, df2 = bridge_data, selected_date = ""){
    
    # df1 <- sample_data #debug
    # df2 <- bridge_data #debug
    # selected_date = unique(sort(c(sample_df_mm$Date, bridge_data$Date)))[1:3] #debug
    
    # Subset data
    delta_df1 <- df1 %>% 
        select(Plate.ID, Date, Delta.T) %>% 
        distinct()
    
    # Subset data    
    delta_df2 <- df2 %>% 
        select(Plate.ID, Date, Delta.T) %>% 
        distinct()
    
    # Add type
    delta_df1$Type <- "Sample Plates"
    delta_df2$Type <- "Bridging Plates"
    
    # Combine
    combo_df <- rbind(delta_df1, delta_df2) %>% 
        filter(Date %in% selected_date) %>%
        mutate(across(c(Date), factor))
    
    plot <-
        ggplot(combo_df, aes(x = Date, y = Delta.T, label = Plate.ID, color = Type)) +
        geom_beeswarm(size = rel(1.5)) + 
        coord_cartesian(ylim = c(-2.5, 2.5))+
        geom_hline(yintercept = 0, linetype = "longdash")+
        labs(x = "", y = "Delta T (\u00B0C)", shape = "Plate Type", color = "Plate Type")
    
    return(fix_jpeg_download(ggplotly(plot),"Temperature_plot","short"))
}

# Figure 5 – Plate control line plots ==========================================

plate_control_plots <- function(df1 = bridge_controls, df2 = sample_controls, log_toggle = F, selected_date = ""){
    
    # df1 <- bridge_controls #debug
    # df2 <- sample_controls # debug
    # (selected_date <- unique(sort(c(bridge_controls$Date, sample_controls$Date)))[1:4])
    
    df <- rbind(df1, df2)
    
    df <- df %>% filter(grepl("Plattenkontrolle", Sample.ID) & 
                            !grepl("Total.Events", Analyte, ignore.case = T)
                        & Date %in% selected_date) %>%
        mutate(across(Plate.number.intern, factor)) %>%
        arrange(Plate.number.intern)
    
    df$Plate.ID <- factor(df$Plate.ID, levels = unique(df$Plate.ID))
    
    out_list <- list()
    
    for (i in unique(df$Sample.ID)) {
        # i <- unique(df$Sample.ID)[1]
        
        temp_df <- df %>% filter(Sample.ID == i)
        
        out_list[[i]] <-
            ggplot(temp_df, aes(x = Plate.ID, y = MFI,
                                color = Analyte, label = Date, text = paste("Plate # intern.", Plate.number.intern))) +
            geom_line(linewidth = 1, aes(group = Analyte)) + geom_point() + 
            scale_x_discrete(expand = expansion(mult = c(0.05, 0.05))) +
            labs(x = "Plate No.", y = "MFI", title = unique(temp_df$Sample.ID)) 
        
        if (log_toggle) {
            
            out_list[[i]] <- out_list[[i]] + 
                scale_y_log10("Log10(MFI)",
                              breaks = scales::trans_breaks("log10", function(x) 10^x),
                              labels = scales::trans_format("log10", math_format(10^.x))) +
                annotation_logticks(sides = "l")
        }
    }
    
    return(out_list)
}

# Figure 6 – Mean and Median MFI per plate Lineplots ===========================
# get mean and median per plate function.

get_mean_median_per_plate <- function(df, x_axis_selection, selected_date = ""){
    
    # df <- sample_data #debug
    # selected_date <- unique(sample_data$Date)
    # x_axis_selection <- "Date"
    # x_axis_selection <- "Week"
    # x_axis_selection <- "Plate.id"
    # head(df)
    
    start <- which(colnames(df) == "Comment")
    analyte_cols <- colnames(df)[(start + 1) : (ncol(df)-1)] # -1 from the end to remove total events
    analyte_cols 
    
    final_df <- df %>% 
        filter(Data.Type == "MFI" & Date %in% selected_date) %>% 
        select(Plate.ID, Date, Week, Plate.daywise, Plate.number.intern, matches(analyte_cols)) %>%
        pivot_longer(cols = matches(analyte_cols), names_to = "Analyte", values_to = "MFI") %>%
        mutate(across(c(Date, Plate.daywise, Week), factor)) 
    
    final_df$Analyte <- factor(final_df$Analyte, levels = analyte_cols)
    
    if (x_axis_selection == "Date") {
        
        final_df <- final_df %>% 
            group_by(Date, Analyte) %>% 
            summarise(Mean_MFI = mean(MFI),
                      Median_MFI = median(MFI)) %>% 
            rename(X_axis = Date) %>%
            mutate(Plate.number.intern = NA)
        
    } else if (x_axis_selection == "Week") {
        
        final_df <- final_df %>% 
            group_by(Week, Analyte) %>% 
            summarise(Mean_MFI = mean(MFI),
                      Median_MFI = median(MFI)) %>% 
            rename(X_axis = Week) %>%
            mutate(Plate.number.intern = NA)
        
    } else {
        
        final_df <- final_df %>% 
            group_by(Plate.ID, Plate.number.intern, Analyte) %>%
            summarise(Mean_MFI = mean(MFI),
                      Median_MFI = median(MFI)) %>%
            rename(X_axis = Plate.ID) %>%
            arrange(Plate.number.intern)
        
        final_df$X_axis <- factor(final_df$X_axis, unique(final_df$X_axis))
    }
    
    return(final_df)
    
}

# draw the Delta-T point plots function
mm_per_plate_lineplots <- function(df, log_toggle = F){
    
    # df <- sample_df_mm_per_plate # Debug
    # head(df)
    
    out_list <- list()
    
    # Draw Lineplot Mean
    out_list[["Mean"]] <-
        ggplot(df, aes(x = X_axis, y = Mean_MFI, color = Analyte, label = Plate.number.intern)) + 
        geom_line(linewidth = 1, aes(group = Analyte)) + geom_point() + 
        scale_x_discrete(expand = expansion(mult = c(0.05, 0.05))) +
        labs(x = "", y = "Mean MFI")
    
    # Draw Lineplot Mean 
    out_list[["Median"]] <-
        ggplot(df, aes(x = X_axis, y = Median_MFI, color = Analyte, label = Plate.number.intern)) + 
        geom_line(linewidth = 1, aes(group = Analyte)) + geom_point() + 
        scale_x_discrete(expand = expansion(mult = c(0.05, 0.05))) +
        labs(x = "", y = "Median MFI")
    
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

# Figure 7 – KT-3 Lineplots ====================================================

# Draw the KT-3 pointplots function
KT3_lineplot <- function(df, log_toggle = F, selected_date = ""){
    
    # df <- bridge_controls #debug
    # selected_date <- unique(bridge_controls$Date) #debug
    
    df <- df %>% filter(Sample.ID == "KT3" & Date %in% selected_date)# %>%
    head(df)
    
    plot <-
        ggplot(df, aes(x = Date, y = MFI, color = Analyte, label = Plate.ID)) + 
        geom_line(linewidth = 1, aes(group = Analyte)) + geom_point() +
        scale_x_discrete(expand = expansion(mult = c(0.05, 0.05))) +
        labs(x = "", y = "KT-3 MFI", color = "Analyte") 
    
    
    if (log_toggle) {
        
        plot <- plot + 
            scale_y_log10("KT-3 Log10(MFI)",
                          breaks = scales::trans_breaks("log10", function(x) 10^x),
                          labels = scales::trans_format("log10", math_format(10^.x))) +
            annotation_logticks(sides = "l")
    }
    
    return(fix_jpeg_download(ggplotly(plot),"KT3_Plot","short"))
    
}    

GST_bees <- function(df, selected_date = ""){
    
    # selected_date <- unique(sort(c(bridge_data$Date, sample_data$Date))) #debug
    # df <- sample_data #debug
    # df <- bridge_data #debug
    
    df <-
        df %>% filter(Data.Type == "MFI" & Date %in% selected_date) %>% 
        mutate(across(c(Plate.ID, Plate.daywise, Date), factor))
    
    # head(df)
    df_median <- 
        df %>% group_by(Date) %>%
        summarise(GST.tag = median(GST.tag, na.rm = T))
    
    plot <-
        ggplot(df, aes(x = Date, y = GST.tag)) + 
        geom_beeswarm(cex = rel(0.5), alpha = 0.7,
                      aes(color = Plate.daywise, text = paste("Plate.ID", Plate.ID), label = Well)) +
        geom_crossbar(data = df_median, size = rel(0.4), aes(ymin = GST.tag, ymax = GST.tag), 
                      show.legend = F, color = "black", width = rel(0.5)) + 
        # stat_summary(fun = mean, geom = "crossbar", width = 0.5, color = "black") # this would be so much better but ggplotly doesn't like it :'( 
        labs(x = "", y = "GST Tag", color = "Plate No. Daywise") 
    
    return(fix_jpeg_download(ggplotly(plot), "GST_Sample"))
    
}

get_all_available_days <-function(summary_dir){
    # Gets all days in between the filenames in the summary folder
    # the dates are returned in a vector to be used with the calendar input
    
    # All dates are grabbed independetly if they contain only summary or only 
    # bridge data 
    
    files_list <- list.files(summary_dir)
    date_list <- sort(unique(str_extract(files_list, "\\d\\d\\d\\d\\d\\d\\d\\d-\\d\\d\\d\\d\\d\\d\\d\\d")))
    all_available_days <- as.Date(c())
    
    for(date in date_list){
        date_edges <- str_split(date,"-")[[1]]
        date_edges <- as.Date(as.character(date_edges),format="%Y%m%d")
        all_available_days <- c(all_available_days,seq(date_edges[1], date_edges[2],by="days"))
    }
    return(all_available_days)
}

get_avaliable_dates <- function(summary_dir){
    # get all files available
    files_list <- list.files(summary_dir)
    # get dates from those files
    date_list <- unique(str_extract(files_list, "\\d\\d\\d\\d\\d\\d\\d\\d-\\d\\d\\d\\d\\d\\d\\d\\d"))
    date_list <- date_list[!is.na(date_list)]
    return(date_list)
}


fix_jpeg_download <- function(p, filename, size = "long"){
    if (size=="short"){
        p <- p%>% config(toImageButtonOptions = list(format= 'jpeg', 
                                                     filename= filename,
                                                     height= 800,
                                                     width= 800,
                                                     scale= 1 ))
    }
    else{
        p <- p%>% config(toImageButtonOptions = list(format= 'jpeg', 
                                                     filename= filename,
                                                     height= 500,
                                                     width= 1000,
                                                     scale= 1 ))
    }
    return(p)
}

css_style <- ".shiny-notification {
                  height: 100px;
                  width: 800px;
                  position:fixed;
                  top: calc(50% - 50px);;
                  left: calc(50% - 400px);;
                }
                pre.shiny-text-output {
                    word-wrap: normal;
                    background-color: #0000;
                    color: white;
                }
                .airdatepicker-highlighted {
                    font-weight: bold;
                    background: #ddd;
                }
                .dp-note {
                    background: #ddd;
                }
                .-selected- .dp-note {
                    opacity: 0;
                }"
