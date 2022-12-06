# # # # Serology plotting functions for shiny
# Author: GK
# Reviewer: EV
# Date: Dec 2022
# Version: 1.0

# Set-up the Environment =======================================================
rm(list = ls(all.names = TRUE)) # will clear all objects including hidden objects

# Libraries
library("tidyverse")
# library("openxlsx")

# Path to the input data
path <- "C:/DKFZ structure/CKB/Combined_Output/"
setwd(path)
list.files(recursive = T)

sample_data <- read.csv("Sample_data_Week_1_11.4.2022.csv")
head(sample_data)

bridge_data <- read.csv("Bridge_data_Week_1_20.05.2021.csv")
head(bridge_data)


# Mean MFI plot: MFI is the "median" already So it's double averaging. 
# First select the data:
# Calculate mean
sample_data <- sample_data %>% 
    mutate(Mean_MFI = rowMeans(select(sample_data, (contains("Analyte")))))
sample_data <- sample_data %>%
    rowwise() %>%
    mutate(Median_MFI = median(c_across(contains("Analyte"))))

# Only keep the selection
plot_df <- sample_data %>% filter(Data_type == "Median") %>% select(Plate.id, Date, Week, Sample.id, Mean_MFI, Median_MFI)
head(plot_df)

# For now until we have a better idea lets remove some sample names:
plot_df <- plot_df %>% filter(!grepl("DEF", Sample.id))
####

# Lineplots: Median and mean
ggplot(plot_df, aes(x = Sample.id, y = Mean_MFI, color = Date, group = Date)) + 
    geom_line(linewidth = 1) +
    theme_classic() + theme(axis.text.x = element_text(angle = 45)) 
#
ggplot(plot_df, aes(x = Sample.id, y = Median_MFI, color = Date, group = Date)) + 
    geom_line(linewidth = 1) +
    theme_classic() + theme(axis.text.x = element_text(angle = 45)) 

# Box plots
# Now keep the counts instad of "medians"
# Only keep the selection
box_plot_df <- sample_data %>% filter(Data_type == "Count") %>% select(Plate.id, Date, Week, Sample.id, Mean_MFI, Median_MFI)
head(box_plot_df)
# For now until we have a better idea lets remove some sample names:
box_plot_df <- box_plot_df %>% filter(!grepl("DEF", Sample.id))
####
# Boxes: Median and mean
ggplot(box_plot_df, aes(x = Sample.id, y = Mean_MFI)) + 
    geom_boxplot() +
    theme_classic() + theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

#
# Boxes: Median and mean
ggplot(box_plot_df, aes(x = Sample.id, y = Median_MFI)) + 
    geom_boxplot() +
    theme_classic() + theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

# TODO
# 3 Blanks per date MFI 
#
# 4 system delta temperature per date 
# Only for sample plates. X-Axis would be the plates, y-axis the MFI values. One line per analyte. 

#
#
