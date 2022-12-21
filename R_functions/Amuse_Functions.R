# # # # functions for Amuse Shiny
# Author: GK
# Reviewer: EV
# Date: Dec 2022
# Version: 1.0

read_in_sample_data <- function(path_to_file = path, Sample_info_file = Sample_info_file){
    
    Sample_plates_raw <- list.files(path = path_to_file, pattern = "FM Platte", recursive = T)
    (Sample_plates_raw <- Sample_plates_raw[!grepl(" \\d+ ", Sample_plates_raw)])
    
    Sample_df <- data.frame()
    
    for (x in Sample_plates_raw) {
        
        # x <- Sample_plates_raw[2]
        
        temp_Sample_data_info <- read.csv(file = x, header = F)
        head(temp_Sample_data_info)
        
        # Skip 50 lines to get a correct column detection 
        temp_Sample_data <- read.csv(file = x, header = F, skip = 51)
        colnames(temp_Sample_data) <- temp_Sample_data[2,]
        
        # find median table
        median_table_start <- which(grepl("median", temp_Sample_data$Sample, ignore.case = T))
        median_table <- temp_Sample_data[(median_table_start+2):(median_table_start+97),]
        # Remove extra columns if any (some weird machine issues)
        last_col <- which(colnames(median_table) == "Total Events")
        median_table <- median_table[,1:last_col]
        head(median_table)
        
        # add type
        median_table$Data_Type <- "MFI"
        
        # find count table
        count_table_start <- which(grepl("^count$", temp_Sample_data$Sample, ignore.case = T))
        count_table <- temp_Sample_data[(count_table_start+2):(count_table_start+97),]
        # Remove extra columns if any (some weird machine issues)
        last_col <- which(colnames(count_table) == "Total Events")
        count_table <- count_table[,1:last_col]
        head(count_table)
        
        # add type
        count_table$Data_Type <- "Counts"
        head(count_table, 3)
        
        # Find the error log for this plate: 
        plate_x <- gsub(".csv", "", gsub(".* ", "", x))
        plate_x_error <- list.files(pattern = plate_x, recursive = T)
        plate_x_error <- plate_x_error[grepl("error", plate_x_error, ignore.case = T)]
        plate_x_error_data <- read.csv(plate_x_error)
        delta_temp <- as.numeric(str_extract(gsub("Temp ", "", str_extract(plate_x_error_data$Message[26], "Temp .*C \\(")), ".*\\d"))
        
        # Put those 2 tables together
        med_count <- rbind(median_table, count_table)
        med_count$Date <- gsub(" .*", "", gsub("^\\.", "", gsub("[^A-Za-z0-9 ]", ".", temp_Sample_data_info$V2[18])))
        med_count$Week <- gsub("Woche", "Week_", str_extract(x, "Woche\\d+"))
        med_count$Delta_T <- delta_temp
        head(med_count)
        
        # TODO 
        # Probably wont need for "real" data the next line
        med_count$Sample <- gsub(".csv", "", gsub(".* ", "", x))
        # #####
        
        # Need some renaming:
        med_count <- med_count %>% rename(Plate.ID = Sample,
                                          position = Location)
        
        med_count$position <- as.numeric(gsub("\\(.*", "", med_count$position))
        head(med_count)
        dim(med_count)
        colnames(med_count)
        
        Sample_df <- rbind(Sample_df, med_count)
        dim(Sample_df)
        colnames(Sample_df)
        
        rownames(Sample_df) <- NULL
    }
    
    # Merge raw data with Sample info file using plate ID,  
    Sample_all <- left_join(Sample_df, Sample_info_file, by = c("Plate.ID", "position"))
    colnames(Sample_all) <- str_to_title(colnames(Sample_all))
    Sample_all <- Sample_all %>% relocate(contains("Analyte"), .after = last_col())
    
    # save data
    # TODO at the moment I select the first date occurance. 
    (filename <- paste0(path_to_file, "Combined_Output/Sample_data_", 
                        unique(Sample_all$Week), "_", unique(Sample_all$Date)[1], ".csv"))
    write_csv(x = Sample_all, file = filename)
    print(paste("Sample data collected and saved under:", filename))
}


read_in_bridging_data <- function(path_to_file = path, Bridge_info_file = Bridge_info_file){
    
    # Read in bridging plates  
    (Bridge_plates_raw <- list.files(path = path_to_file, pattern = "FM Platte \\d+", recursive = T))
    
    # make an empty container
    Bridge_df <- data.frame()
    
    for (x in Bridge_plates_raw) {
        
        # x <- Bridge_plates_raw[2]
        
        temp_Bridge_df <- read.csv(x, header = F, skip = 36)   
        colnames(temp_Bridge_df) <- temp_Bridge_df[4,]
        
        # median starts from 1st row
        Bridge_median_start <- which(grepl("median", temp_Bridge_df$Sample, ignore.case = T))
        Bridge_median_df <- temp_Bridge_df[(Bridge_median_start+2):(Bridge_median_start+97),]
        Bridge_median_df$Data_Type <- "MFI"
        
        # count 
        Bridge_count_start <- which(grepl('^count$', temp_Bridge_df$Sample, ignore.case = T))
        Bridge_count_table <- temp_Bridge_df[(Bridge_count_start+2):(Bridge_count_start+97),]
        Bridge_count_table$Data_Type <- "Counts"
        
        # Find the error log for this plate: 
        bridge_x <- str_extract(x, "\\d\\d\\d\\d\\d\\d")
        bridge_x_error <- list.files(pattern = bridge_x, recursive = T)
        bridge_x_error <- bridge_x_error[grepl("error", bridge_x_error, ignore.case = T)]
        bridge_x_error_data <- read.csv(bridge_x_error)
        bridge_delta_temp <- as.numeric(str_extract(gsub("Temp ", "", str_extract(bridge_x_error_data$Message[26], "Temp .*C \\(")), ".*\\d"))
        
        # Put those 2 tables together
        Bridge_med_count <- rbind(Bridge_median_df, Bridge_count_table)
        
        # TODO 
        # Probably wont need for "real" data the next line: in real data, Sample should be 000001 etc.
        Bridge_med_count$Sample <- str_extract(x, "\\d\\d\\d\\d\\d\\d")
        ### 
        
        Bridge_med_count$Date <- str_extract(x, "\\d\\d\\.\\d\\d\\.\\d\\d\\d\\d")
        Bridge_med_count$Week <- gsub("Woche", "Week_", str_extract(x, "Woche\\d+"))
        Bridge_med_count$Delta_T <- bridge_delta_temp
        
        # Need some renaming:
        Bridge_med_count <- Bridge_med_count %>% rename(Plate.ID = Sample,
                                                        position = Location)
        
        Bridge_med_count$position <- as.numeric(gsub("\\(.*", "", Bridge_med_count$position))
        
        # Save data into one container
        Bridge_df <- rbind(Bridge_df, Bridge_med_count)
        
        rownames(Bridge_df) <- NULL
        
    }
    
    Bridge_all <- left_join(Bridge_df, Bridge_info_file, by = c("Plate.ID", "position"))
    colnames(Bridge_all) <- str_to_title(colnames(Bridge_all))
    Bridge_all <- Bridge_all %>% relocate(contains("Analyte"), .after = last_col())
    
    # save data
    # TODO at the moment I select the first date occurance. 
    (filename <- paste0(path_to_file, "Combined_Output/Bridging_data_", 
                        unique(Bridge_all$Week), "_", unique(Bridge_all$Date)[1], ".csv"))
    write_csv(x = Bridge_all, file = filename)
    print(paste("Bridging data collected and saved under:", filename))
}

# Figure 1 – Mean/Median MFI Lineplots =========================================
# Bridging data only + Log/linear toggle

# get mean and median. Works on both Sample and Bridging data
get_mean_median <- function(df){
    
    final_df <- 
        df %>% 
        select(Plate.id, Date, Sample.id, Data_type, contains("Analyte")) %>%
        pivot_longer(cols = contains("Analyte"), names_to = "Analyte", values_to = "Value") %>%
        group_by(Plate.id, Date, Analyte, Data_type) %>% 
        summarise(Mean = mean(Value, na.rm = T),
                  Median = median(Value,  na.rm = T)) %>% 
        pivot_wider(names_from = Data_type, values_from = c(Mean, Median))
    
    return(final_df)
    
}

# Draw the mean median MFI lineplots
mean_median_lineplots <- function(df){
    
    # df <- bridge_df_mm ¤debug
    out_list <- list()
    # Draw median MFI lineplot
    out_list[["Mean"]] <-
        ggplot(df, aes(x = Analyte, y = Mean_MFI, color = Date, group = Date)) + 
        geom_line(linewidth = 1) + geom_point() +
        theme_classic() + theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
    
    # Draw median MFI lineplot
    out_list[["Median"]] <-
        ggplot(df, aes(x = Analyte, y = Median_MFI, color = Date, group = Date)) + 
        geom_line(linewidth = 1) + geom_point() +
        theme_classic() + theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
    
    return(out_list)
    
}

# Figure 2 – Mean/Median Counts Boxplots =======================================
# Bridging and Sample data

# Draw the mean median boxplots function
mean_median_boxplots <- function(df){
    
    # df <- sample_df_mm #debug
    
    # Draw Mean Boxes
    Mean_box_plot <-
        ggplot(df, aes(x = Analyte, y = Mean_Counts)) + 
        geom_boxplot() +
        theme_classic() + theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
    
    return(Mean_box_plot)
    
}

# Figure 3 – Blank MFI Boxplots =================================================
# Bridging and Sample data

# get blank and KT-3 samples only. Works on both Sample and Bridging data
get_blanks_kt <- function(df){
    
    # df <- sample_data # debug
    
    final_df <- 
        df %>% filter(grepl("blank|KT-3", Sample.id) & Data_type == "MFI") %>% 
        select(Plate.id, Date, Sample.id, contains("Analyte")) %>%
        pivot_longer(cols = contains("Analyte"), names_to = "Analyte", values_to = "MFI")
    
    return(final_df)
    
}

# Draw the blank boxplots function
blank_bees <- function(df){
    
    # df <- sample_blanks # debug
    df <- df %>% filter(Sample.id == "blank")
    
    plot <-
        ggplot(df, aes(x = Analyte, y = MFI, color = as.character(Plate.id))) + 
        geom_beeswarm() + labs(color = "Plate.ID") + 
        theme_classic() + theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
    
    return(plot)
    
}    

# Figure 4 – Delta-T Dotplots ==================================================
# Bridging and Sample data

# draw the Delta-T point plots function
delta_t_pointplot <- function(sample_data = df1, bridge_data = df2){
    
    # df1 <- sample_data #debug
    # df2 <- bridge_data #debug
    
    # Subset data
    delta_df1 <- df1 %>% 
        select(Plate.id, Date, Delta_t) %>% 
        distinct()
    # Subset data    
    delta_df2 <- df2 %>% 
        select(Plate.id, Date, Delta_t) %>% 
        distinct()
    
    # Add type
    delta_df1$Type <- "Sample Plates"
    delta_df2$Type <- "Bridging Plate"
    
    # Combine
    combo_df <- rbind(delta_df1, delta_df2)
    
    plot <-
        ggplot(combo_df, aes(x = Date, y = Delta_t, shape = Type, color = Plate.id)) + 
        geom_point(position = position_dodge(0.3), size = 2) +
        theme_classic() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
    
    return(plot)
}

# Figure 5 – Mean and Median MFI per plate Lineplots ===========================
# get mean and median per plate function.

get_mean_median_per_plate <- function(df){
    
    # df <- sample_data #debug
    # head(df)
    
    final_df <- df %>% 
        filter(Data_type == "MFI") %>% 
        select(Plate.id, Date, Week, Plate_daywise, contains("Analyte")) %>%
        pivot_longer(cols = contains("Analyte"), names_to = "Analyte", values_to = "MFI") %>%
        group_by(Plate.id, Date, Week, Plate_daywise, Analyte) %>% 
        summarise(Mean_MFI = mean(MFI),
                  Median_MFI = median(MFI))
    
    return(final_df)
    
}

# draw the Delta-T point plots function
mm_per_plate_lineplots <- function(df, x_axis = x_axis){
    
    # df <- sample_df_mm_per_plate # Debug
    
    out_list <- list()
    
    # Draw Lineplot Mean platewise 
    out_list[["Mean"]] <-
        ggplot(df, aes(x = get(x_axis), y = Mean_MFI, color = Analyte, group = Analyte)) + 
        geom_line(linewidth = 1) + xlab(x_axis) +
        theme_classic() + theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
    
    # Draw Lineplot Mean platewise 
    out_list[["Median"]] <-
        ggplot(df, aes(x = get(x_axis), y = Median_MFI, color = Analyte, group = Analyte)) + 
        geom_line(linewidth = 1) + xlab(x_axis) +
        theme_classic() + theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
    
    return(out_list)
    
}


# Figure 6 – KT-3 Lineplots ====================================================
# TODO Will need a Mean over plate.id/date probably

# Draw the KT-3 pointplots function
KT3_lineplot <- function(df){
    
    # df <- sample_blanks_kt # debug
    df <- df %>% filter(Sample.id == "KT-3")# %>% 
    # TODO this needs summarising over something at one point when we have more plates    
    # group_by(Plate.id, Date) %>% 
    # summarise(Mean_MFI = mean(MFI))
    
    plot <-
        ggplot(df, aes(x = Date, y = MFI, group = Analyte, color = Analyte)) + 
        geom_line(linewidth = 1) + labs(color = "Analyte") + 
        theme_classic() + theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
    
    return(plot)
    
}    

# Figure 7 – GST Beeswarm ======================================================

# Draw the GST bees function
GST_bees <- function(df){
    
    # df <- sample_data # debug
    
    df <- df %>% filter(Data_type == "MFI")
    
    plot <-
        ggplot(df, aes(x = Date, y = GST_tag, color = as.character(Plate.id))) + 
        geom_beeswarm(size = 2) + labs(color = "Plate.ID") +   
        theme_classic() + theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
    
    return(plot)
    
}    
