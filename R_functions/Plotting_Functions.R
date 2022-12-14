# # # # Serology plotting functions for shiny
# Author: GK
# Reviewer: EV
# Date: Dec 2022
# Version: 1.0

# Set-up the Environment =======================================================
rm(list = ls(all.names = TRUE)) # will clear all objects including hidden objects

# Libraries
library("tidyverse")
library("cowplot")

# Path to the input data
path <- "C:/DKFZ Project Amuse/CKB/Combined_Output/"
setwd(path)
list.files(recursive = T)

# Read in sample data
sample_data <- read.csv("Sample_data_Week_1_11.4.2022.csv")
head(sample_data)

# Read in bridging data
bridge_data <- read.csv("Bridge_data_Week_1_20.05.2021.csv")
head(bridge_data)

# Figure 1 – Mean/Median MFI Lineplots =========================================
# Bridging data only + Log/linear toggle

# get mean and median function. Works on both Sample and Bridging data
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

bridge_df_mm <- get_mean_median(bridge_data)

# Draw the mean median MFI lineplots function
mean_median_lineplots <- function(df){
    
    out_list <- list()
    # Draw median MFI lineplot
    out_list[["Mean"]] <- ggplot(df, aes(x = Analyte, y = Mean_MFI, color = Date, group = Date)) + 
        geom_line(linewidth = 1) +
        theme_classic() + theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
    
    # Draw median MFI lineplot
    out_list[["Median"]] <- ggplot(df, aes(x = Analyte, y = Median_MFI, color = Date, group = Date)) + 
        geom_line(linewidth = 1) +
        theme_classic() + theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
    
    return(out_list)
    
}

Bridge_MM_MFI_Plots <- mean_median_lineplots(bridge_df_mm)
do.call(plot_grid, c(Bridge_MM_MFI_Plots, ncol = 1, align = "hv"))

# Figure 2 – Mean/Median Counts Boxplots =======================================
# Bridging and Sample data

# Draw the mean median boxplots function
mean_median_boxplots <- function(df){
    
    out_list <- list()
    
    # Draw Mean Boxes
    out_list[["Mean"]] <- ggplot(df, aes(x = Analyte, y = Mean_Counts)) + 
        geom_boxplot() +
        theme_classic() + theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
    
    # Draw Median Boxes
    out_list[["Median"]] <- ggplot(df, aes(x = Analyte, y = Median_Counts)) + 
        geom_boxplot() +
        theme_classic() + theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
    
    return(out_list)
    
}

sample_df_mm <- get_mean_median(sample_data)
bridge_df_mm

Sample_MM_Boxplots <- mean_median_boxplots(sample_df_mm)
do.call(plot_grid, c(Sample_MM_Boxplots, ncol = 1, align = "hv"))

Bridge_MM_Boxplots <- mean_median_boxplots(bridge_df_mm)
do.call(plot_grid, c(Bridge_MM_Boxplots, ncol = 1, align = "hv"))

# Figure 3 – Blank MFI Boxplots =================================================
# Bridging and Sample data

# get mean and median function. Works on both Sample and Bridging data
get_blanks <- function(df){
    
    final_df <- 
        df %>% filter(grepl("blank", Sample.id) & Data_type == "MFI") %>% 
        select(Plate.id, Date, Sample.id, contains("Analyte")) %>%
        pivot_longer(cols = contains("Analyte"), names_to = "Analyte", values_to = "MFI")
    
    return(final_df)
    
}

sample_blanks <- get_blanks(sample_data)
bridge_blanks <- get_blanks(bridge_data)

# Draw the blank boxplots function
blank_boxplots <- function(df){
    
    plot <- ggplot(df, aes(x = Date, y = MFI)) + 
        geom_boxplot() +
        theme_classic() + theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
    
    return(plot)
    
}    

blank_boxplots(sample_blanks)
blank_boxplots(bridge_blanks)

# Figure 4 – Delta-T Dotplots ==================================================
# Bridging and Sample data

# draw the Delta-T point plots function
delta_t_pointplots <- function(df){
    
    delta_df <- df %>% 
        select(Plate.id, Date, Delta_t) %>% 
        distinct()
    
    plot <- ggplot(delta_df, aes(x = Date, y = Delta_t)) + 
        geom_point(position = position_dodge(0.3)) +
        theme_classic() + 
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
    
    return(plot)
}

delta_t_pointplots(sample_data)
delta_t_pointplots(bridge_data)

# Figure 5 – Mean and Median MFI per plate Lineplots ===========================
# Sample data

# get mean and median per plate function.
get_mean_median_per_plate <- function(df){
    
    final_df <- df %>% 
        filter(Data_type == "MFI") %>% 
        select(Plate.id, Date, contains("Analyte")) %>%
        pivot_longer(cols = contains("Analyte"), names_to = "Analyte", values_to = "MFI") %>%
        group_by(Plate.id, Date, Analyte) %>% 
        summarise(Mean_MFI = mean(MFI),
                  Median_MFI = median(MFI))
    
    return(final_df)
    
}

sample_df_mm_per_plate <- get_mean_median_per_plate(sample_data)

# draw the Delta-T point plots function
delta_t_pointplots <- function(df){
    
    out_list <- list()
    
    # Draw Lineplot Mean platewise 
    out_list[["Mean"]] <- ggplot(df, aes(x = Plate.id, y = Mean_MFI, color = Analyte, group = Analyte)) + 
    geom_line(linewidth = 1) +
    theme_classic() + theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

    # Draw Lineplot Mean platewise 
    out_list[["Median"]] <- ggplot(df, aes(x = Plate.id, y = Median_MFI, color = Analyte, group = Analyte)) + 
    geom_line(linewidth = 1) +
    theme_classic() + theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

return(out_list)

}

Sample_MM_per_plate <- delta_t_pointplots(sample_df_mm_per_plate)
do.call(plot_grid, c(Sample_MM_per_plate, ncol = 1, align = "hv"))
