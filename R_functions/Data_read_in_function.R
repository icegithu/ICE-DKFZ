# # # # Serology data import for Shiny
# Author: GK
# Reviewer: EV
# Date: Dec 2022
# Version: 1.0

# Set-up the Environment =======================================================
rm(list = ls(all.names = TRUE)) # will clear all objects including hidden objects

# Libraries
library("tidyverse")
library("openxlsx")

# Path to the input data
path <- "C:/DKFZ Project Amuse//CKB/"
setwd(path)

list.files(recursive = T)

# Read in the Sample file - always the same, never updated
Sample_info_file_loc <- "Orga Studie/Plattenbelegungsplan_StudySamples.xlsx"
Sample_info_file <- read.xlsx(Sample_info_file_loc)
head(Sample_info_file,3)

# Read in the briding file - always the same, never updated
Bridge_info_file_loc <- "Orga Studie/Plattenbelegungsplan_Bridging.xlsx"
Bridge_info_file <- read.xlsx(Bridge_info_file_loc)
head(Bridge_info_file)

# Read in Sample plates
list.files("Rohdaten/", recursive = T)
Sample_plates_raw <- list.files(pattern = "FM Platte", recursive = T)
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

# one week's worth of data in one df
head(Sample_df)

# Read in bridging plates  
list.files("Rohdaten/", recursive = T)
(Bridge_plates_raw <- list.files(pattern = "FM Platte \\d+", recursive = T))

# make an empty container
Bridge_df <- data.frame()

for (x in Bridge_plates_raw) {
    
    # x <- Bridge_plates_raw[1]
    
    temp_Bridge_df <- read.csv(x, header = F, skip = 36)   
    colnames(temp_Bridge_df) <- temp_Bridge_df[3,]
    
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

# one week's worth of bridging data
head(Bridge_df)

# Finalize Sample DF ===========================================================

### Now we have all the data. need to merge it
head(Sample_info_file, 3)
head(Sample_df, 3)
dim(Sample_df) # in the beginning
dim(Sample_info_file) # this should be very long (all the 40k Samples)

# Merge raw data with Sample info file using plate ID,  
Sample_all <- left_join(Sample_df, Sample_info_file, by = c("Plate.ID", "position"))
colnames(Sample_all) <- str_to_title(colnames(Sample_all))
Sample_all <- Sample_all %>% relocate(contains("Analyte"), .after = last_col())
head(Sample_all, 2)
dim(Sample_all)
# Write the data out
# TODO at the moment I select the first date occurance. 
(filename <- paste("Combined_Output/Sample_data", unique(Sample_all$Week), unique(Sample_all$Date)[1], sep = "_"))
write_csv(x = Sample_all, file = paste0(filename,".csv"))

# Finalize Bridge DF ===========================================================
head(Bridge_info_file, 3)
head(Bridge_df, 2)
dim(Bridge_info_file)
dim(Bridge_df)

Bridge_all <- left_join(Bridge_df, Bridge_info_file, by = c("Plate.ID", "position"))
colnames(Bridge_all) <- str_to_title(colnames(Bridge_all))
Bridge_all <- Bridge_all %>% relocate(contains("Analyte"), .after = last_col())
head(Bridge_all, 2)
dim(Bridge_all)

# Write the data out: 
# TODO at the moment I select the first date occurance. 
(filename <- paste("Combined_Output/Bridge_data", unique(Bridge_all$Week), unique(Bridge_all$Date)[1], sep = "_"))
write_csv(x = Bridge_all, file = paste0(filename,".csv"))

