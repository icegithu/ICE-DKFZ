# # # # Serology data import for Shiny
# Author: GK
# Reviewer: EV
# Date: Dec 2022
# Version: 1.0

# Set-up the Environment =======================================================
rm(list = ls(all.names = TRUE)) # will clear all objects including hidden objects

# Libraries
library("ggbeeswarm")
library("cowplot")
library("openxlsx")
library("tidyverse")

# Path to the input data for
path <- "C:/DKFZ Project Amuse/CKB/"
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

# Source amuse functions
source('C:/Users/GK/Documents/R_Projects/ICE-DKFZ/R_functions/Amuse_Functions.R')

# update input data
# read_in_sample_data(path_to_file = path, Sample_info_file = Sample_info_file)
# read_in_bridging_data(path_to_file = path, Bridge_info_file = Bridge_info_file)

# Read in sample data for plotting
list.files(paste0(path,"Combined_Output"))
sample_data <- read.csv(paste0(path,"Combined_Output/Sample_data_Week_1_11.4.2022.csv"))
sum(is.na(sample_data$Sample.id))
head(sample_data)

# Read in bridging data for plotting
bridge_data <- read.csv(paste0("Combined_Output/Bridging_data_Week_1_20.05.2021.csv"))
bridge_data$Sample.id[is.na(bridge_data$Sample.id)] <- "empty"
head(bridge_data)

################################################################################
### THIS IS ONLY UNTIL WE GET NEW GOOD Dummy data
################################################################################
# Add KT-3 info (for now) to make those plots later
bridge_data$Sample.id[bridge_data$Sample.id == "AB_1"] <- "KT-3"
bridge_data %>% filter(Sample.id == "KT-3")
sample_data$Sample.id[sample_data$Sample.id == "ABC123"| sample_data$Sample.id == "DEF123"] <- "KT-3"
sample_data %>% filter(Sample.id == "KT-3")
# Rename one analyte to GST tag
colnames(bridge_data)[ncol(bridge_data)] <- "GST_tag"
bridge_data$GST_tag <- rnorm(n = nrow(bridge_data), mean = 90, sd = 5)

colnames(sample_data)[ncol(sample_data)] <- "GST_tag"
sample_data$GST_tag <- rnorm(n = nrow(sample_data), mean = 85, sd = 8)

################################################################################
################################################################################

# Get mean and median dataframes
bridge_df_mm <- get_mean_median(bridge_data)
sample_df_mm <- get_mean_median(sample_data)

# Figure 1 – Mean/Median MFI Lineplots =========================================
# Bridging data only + Log/linear toggle
Bridge_MM_MFI_Plots <- mean_median_lineplots(bridge_df_mm)
do.call(plot_grid, c(Bridge_MM_MFI_Plots, ncol = 1, align = "hv"))

# Figure 2 – Mean/Median Counts Boxplots =======================================
# Bridging and Sample data
Mean_Boxplots <- list()
Mean_Boxplots[["Sample"]] <- mean_median_boxplots(sample_df_mm) + ggtitle("Sample data")
Mean_Boxplots[["Bridge"]] <- mean_median_boxplots(bridge_df_mm) + ggtitle("Bridging data")

do.call(plot_grid, c(Mean_Boxplots, ncol = 1, align = "hv"))

# get blank-sample data frames 
sample_blanks_kt <- get_blanks_kt(sample_data)

############### ONLY for dummy data!!!! 
sample_blanks_kt$MFI[sample_blanks_kt$Plate.id == "jub826" & sample_blanks_kt$Sample.id == "blank"] <- sample(10:20, size = sum(sample_blanks_kt$Plate.id == "jub826" & sample_blanks_kt$Sample.id == "blank"), replace = T)
################################################################################

bridge_blanks_kt <- get_blanks_kt(bridge_data)

# Figure 3 – Blank MFI Boxplots =================================================
# Bridging and Sample data
blank_bees(sample_blanks_kt)
blank_bees(bridge_blanks_kt)

# Figure 4 – Delta-T Dotplots ==================================================
# Bridging and Sample data
delta_t_pointplot(sample_data = sample_data, bridge_data = bridge_data)

# Get mean median per plate
sample_df_mm_per_plate <- get_mean_median_per_plate(sample_data)
head(sample_df_mm_per_plate)

# Figure 5 – Mean and Median MFI per plate Lineplots ===========================
# Sample data
x_axis <- "Plate.id"
x_axis <- "Date"
x_axis <- "Week"
x_axis <- "Plate_daywise"

Sample_MM_per_plate <- mm_per_plate_lineplots(sample_df_mm_per_plate, x_axis = x_axis)
do.call(plot_grid, c(Sample_MM_per_plate, ncol = 1, align = "hv"))

# Figure 6 – KT-3 dotplots =====================================================
# Sample and bridging data
KT3_lineplot(sample_blanks_kt)
KT3_lineplot(bridge_blanks_kt)

# Figure 7 – GST Beeswarm plot =================================================
# Sample and bridging data

GST_bees(sample_data)
GST_bees(bridge_data)
