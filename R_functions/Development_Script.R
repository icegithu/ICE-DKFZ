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
library("scales")
library("tidyverse")
library("plotly")

# Path to the input data for
path <- "C:/DKFZ Project Amuse/CKB/"
# setwd(path)
# list.files(recursive = T)

# Read in the Sample file - always the same, never updated
Sample_info_file_loc <- paste0(path, "OrgaStudie/Plattenbelegungsplan_StudySamples.xlsx")
Sample_info_file <- read.xlsx(Sample_info_file_loc)
head(Sample_info_file, 3)

# Read in the briding file - always the same, never updated
Bridge_info_file_loc <- paste0(path, "OrgaStudie/Plattenbelegungsplan_Bridging.xlsx")
Bridge_info_file <- read.xlsx(Bridge_info_file_loc)
head(Bridge_info_file)

# Source amuse functions
source('C:/Users/GK/Documents/R_Projects/ICE-DKFZ/R_functions/Amuse_Functions.R')

# update input data
# read_in_sample_data(path_to_file = path, Sample_info_file = Sample_info_file)
# read_in_bridging_data(path_to_file = path, Bridge_info_file = Bridge_info_file)

# Read in sample data for plotting
list.files(paste0(path,"Combined_Output"))

sample_data <- read.csv(paste0(path,"Combined_Output/Sample_data_Week_1_20210520-20210523.csv"))
sum(is.na(sample_data$Sample.id))
head(sample_data)

# Read in bridging data for plotting
bridge_data <- read.csv(paste0(path, "Combined_Output/Bridging_data_Week_1_20210520-20210523.csv"))
# Fix nas
bridge_data$Sample.id[is.na(bridge_data$Sample.id)] <- "empty"
head(bridge_data)

################################################################################

# Figure 1 – Mean/Median MFI Lineplots =========================================
# Get mean and median dataframes
bridge_df_mm <- get_mean_median(bridge_data)
sample_df_mm <- get_mean_median(sample_data)

# Bridging data only + Log/linear toggle
log_toggle <- T
Bridge_MM_MFI_Plots <- mean_median_lineplots(bridge_df_mm, log_toggle)
do.call(plot_grid, c(Bridge_MM_MFI_Plots, ncol = 1, align = "hv"))

# Figure 2 – Mean Counts in Boxplots ===========================================
# Bridging and Sample data

selected_date <- unique(sample_df_mm$Date)[1]
mean_boxplots(sample_df_mm, selected_date)

selected_date <- unique(bridge_df_mm$Date)[1]
mean_boxplots(bridge_df_mm, selected_date)

# Figure 3 – Blank MFI Boxplots =================================================
# get blank-sample data frames 
sample_blanks_kt <- get_blanks_kt(sample_data)
bridge_blanks_kt <- get_blanks_kt(bridge_data)

# Bridging and Sample data
blank_bees(sample_blanks_kt)
blank_bees(bridge_blanks_kt)

# Figure 4 – Delta-T Dotplots ==================================================
# Bridging and Sample data
p <- delta_t_pointplot(df1 = sample_data, df2 = bridge_data)
remove_parenthesis_legend(p)

# Figure 5 – Mean and Median MFI per plate Lineplots ===========================
# Get mean median per plate
sample_df_mm_per_plate <- get_mean_median_per_plate(sample_data)

# Sample data
x_axis <- "Date"
x_axis <- "Week"
x_axis <- "Plate_daywise"
x_axis <- "Plate.id"

Sample_MM_per_plate <- mm_per_plate_lineplots(sample_df_mm_per_plate, x_axis = x_axis)
do.call(plot_grid, c(Sample_MM_per_plate, ncol = 1, align = "hv"))

# Figure 6 – KT-3 dotplots =====================================================
# Bridging data only
KT3_lineplot(bridge_blanks_kt)

# Figure 7 – GST Beeswarm plot =================================================
# Sample data
GST_violins(sample_data)

# Bridging data
# TODO Filtering is only for now, because of bad dummy data
bridge_data %>% 
    filter(Gst.Tag<750) %>% 
    GST_bees()
