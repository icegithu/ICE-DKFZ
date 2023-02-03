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
path <- "C:/DKFZ Project Amuse/Test CKB/"
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
read_in_sample_data(path_to_file = path, Sample_info_file = Sample_info_file)
read_in_bridging_data(path_to_file = path, Bridge_info_file = Bridge_info_file)

# Read in sample data for plotting
list.files(paste0(path,"Combined_Output"))
(selected_samples_files <- list.files(path = paste0(path,"Combined_Output"), pattern = "Sample", full.names = T))
selected_samples_files <- selected_samples_files[2] # for now for better compatibility with dummy data

# Read in and bind rows
sample_data <- selected_samples_files %>% 
    lapply(read_csv) %>% 
    bind_rows %>%
    rename_all(recode, "Gst Tag" = "Gst.Tag")

# Read in bridging data for plotting
list.files(paste0(path,"Combined_Output"))
(selected_bridge_files <- list.files(path = paste0(path,"Combined_Output"), pattern = "Bridging", full.names = T))
selected_bridge_files <- selected_bridge_files[2] # for now for better compatibility with dummy data

bridge_data <- selected_bridge_files %>% 
    lapply(read_csv) %>% 
    bind_rows %>% 
    rename_all(recode, "Gst Tag" = "Gst.Tag") %>%
    mutate(Plate_daywise = as.numeric(Plate.id)) %>%
    relocate(Plate_daywise, .after = Plate)

# Fix nas
bridge_data$Sample.id[is.na(bridge_data$Sample.id)] <- "empty"
head(bridge_data)

################################################################################

# Figure 1 – Mean/Median MFI Lineplots =========================================
# Get mean and median dataframes
bridge_df_mm <- get_mean_median(bridge_data)
sample_df_mm <- get_mean_median(sample_data)

# Bridging data only + Log/linear toggle
log_toggle <- F
Bridge_MM_MFI_Plots <- mean_median_lineplots(bridge_df_mm, log_toggle)
Bridge_MM_MFI_Plots[[1]] %>% ggplotly()
# do.call(plot_grid, c(Bridge_MM_MFI_Plots, ncol = 1, align = "hv"))
# ggsave("C:/Users/GK/Desktop/dkfz/Figure_1_Bridging_data_only.jpg", scale = 2.5)

# Figure 2 – Mean Counts in Boxplots ===========================================
# Bridging and Sample data

selected_date <- unique(bridge_df_mm$Date)[3]
mean_boxplots(bridge_df_mm, selected_date)
# ggsave("C:/Users/GK/Desktop/dkfz/Figure_2_Bridging.jpg", scale = 2.5)

selected_date <- unique(sample_df_mm$Date)[1:4]
mean_boxplots(sample_df_mm, selected_date)
# ggsave("C:/Users/GK/Desktop/dkfz/Figure_2_Sample.jpg", scale = 2.5)

# Figure 3 – Blank MFI Boxplots =================================================
# get blank-sample data frames 
sample_controls <- get_controls(sample_data)
bridge_controls <- get_controls(bridge_data)

# Bridging and Sample data
blank_lines(bridge_controls)
# ggsave("C:/Users/GK/Desktop/dkfz/Figure_3_Bridging_lines.jpg", scale = 2.5)

(selected_date <- as.character(unique(sample_controls$Date)[1:4]))
blank_bees(sample_controls, selected_date)
# ggsave("C:/Users/GK/Desktop/dkfz/Figure_3_Sample_bees.jpg", scale = 2.5)

# blank_violins(sample_controls)
# ggsave("C:/Users/GK/Desktop/dkfz/Figure_3_Sample_violins.jpg", scale = 2.5)

# Figure 4 – Delta-T Dotplots ==================================================
# Bridging and Sample data
delta_t_pointplot(df1 = sample_data, df2 = bridge_data) 
# %>% remove_parenthesis_legend()
# ggsave("C:/Users/GK/Desktop/dkfz/Figure_4_Bridging_and_sample.jpg", scale = 2.5)

# Figure 5 – Plate control line plots ==========================================
# Bridging and Sample data

log_toggle <- T
all_controls <- plate_control_plots(bridge_controls, sample_controls, log_toggle)
# do.call(plot_grid, c(plate_control_plots, ncol = 1, align = "hv"))
all_controls[[1]] %>% ggplotly()
all_controls[[2]] %>% ggplotly()
all_controls[[3]] %>% ggplotly()
# ggsave("C:/Users/GK/Desktop/dkfz/Figure_5_Bridge_controls.jpg", scale = 2.5)


# Figure 6 – Mean and Median MFI per plate Lineplots ===========================
# Get mean median per selection. Either date, week or plate.id
# Sample data
x_axis_selection <- "Date"
x_axis_selection <- "Week"
x_axis_selection <- "Plate.id"

sample_df_mm_per_plate <- get_mean_median_per_plate(sample_data, x_axis_selection)


log_toggle <- T

Sample_MM_per_plate <- mm_per_plate_lineplots(sample_df_mm_per_plate, log_toggle)
Sample_MM_per_plate[[1]] %>% ggplotly()
# do.call(plot_grid, c(Sample_MM_per_plate, ncol = 1, align = "hv"))
# ggsave("C:/Users/GK/Desktop/dkfz/Figure_6_Sample_mm.jpg", scale = 2.5)

# Figure 7 – KT-3 dotplots =====================================================
# Bridging data only

log_toggle <- T
KT3_lineplot(bridge_controls, log_toggle)

# ggsave("C:/Users/GK/Desktop/dkfz/Figure_7_Bridging_KT-3.jpg", scale = 2.5)

# Figure 8 – GST Beeswarm plot =================================================

# Bridging data
# TODO Filtering is only for now, because of bad dummy data
bridge_data %>% 
    filter(Gst.Tag < 750) %>% 
    GST_bees()
# ggsave("C:/Users/GK/Desktop/dkfz/Figure_8_Bridging_Bees.jpg", scale = 2.5)

# Sample data
GST_bees(sample_data)
# ggsave("C:/Users/GK/Desktop/dkfz/Figure_8_Sample_violins.jpg", scale = 2.5)
