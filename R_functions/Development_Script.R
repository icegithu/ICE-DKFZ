# # # # Serology data import for Shiny
# Author: GK
# Reviewer: EV
# Date: Dec 2022
# Version: 1.0

# Set-up the Environment =======================================================
rm(list = ls(all.names = TRUE)) # will clear all objects including hidden objects

# Libraries
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
read_in_sample_data(path_to_file = path, Sample_info_file = Sample_info_file)
read_in_bridging_data(path_to_file = path, Bridge_info_file = Bridge_info_file)

# Read in sample data for plotting
sample_data <- read.csv(paste0(path,"Combined_Output/Sample_data_Week_1_11.4.2022.csv"))
head(sample_data)

# Read in bridging data for plotting
bridge_data <- read.csv(paste0("Combined_Output/Bridging_data_Week_1_20.05.2021.csv"))
head(bridge_data)

# Get mean and median dataframes
bridge_df_mm <- get_mean_median(bridge_data)
sample_df_mm <- get_mean_median(sample_data)

# Figure 1 – Mean/Median MFI Lineplots =========================================
# Bridging data only + Log/linear toggle
Bridge_MM_MFI_Plots <- mean_median_lineplots(bridge_df_mm)
do.call(plot_grid, c(Bridge_MM_MFI_Plots, ncol = 1, align = "hv"))

# Figure 2 – Mean/Median Counts Boxplots =======================================
# Bridging and Sample data
Sample_MM_Boxplots <- mean_median_boxplots(sample_df_mm)
do.call(plot_grid, c(Sample_MM_Boxplots, ncol = 1, align = "hv"))

Bridge_MM_Boxplots <- mean_median_boxplots(bridge_df_mm)
do.call(plot_grid, c(Bridge_MM_Boxplots, ncol = 1, align = "hv"))

# get blank-sample data frames 
sample_blanks <- get_blanks(sample_data)
bridge_blanks <- get_blanks(bridge_data)

# Figure 3 – Blank MFI Boxplots =================================================
# Bridging and Sample data
blank_boxplots(sample_blanks)
blank_boxplots(bridge_blanks)

# Figure 4 – Delta-T Dotplots ==================================================
# Bridging and Sample data
delta_t_pointplots(sample_data)
delta_t_pointplots(bridge_data)

# Get mean median per plate
sample_df_mm_per_plate <- get_mean_median_per_plate(sample_data)

# Figure 5 – Mean and Median MFI per plate Lineplots ===========================
# Sample data
Sample_MM_per_plate <- mm_per_plate_lineplots(sample_df_mm_per_plate)
do.call(plot_grid, c(Sample_MM_per_plate, ncol = 1, align = "hv"))
