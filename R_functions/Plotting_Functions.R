# # # # Serology plotting functions for shiny
# Author: GK
# Reviewer: EV
# Date: Dec 2022
# Version: 1.0

# Set-up the Environment =======================================================
rm(list = ls(all.names = TRUE)) # will clear all objects including hidden objects

# Libraries
library("tidyverse")

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

# Calculate mean and median per sample (over analytes) and add them to new columns
sample_data <- sample_data %>% 
    mutate(Mean = rowMeans(select(sample_data, (contains("Analyte")))))

sample_data <- sample_data %>%
    rowwise() %>%
    mutate(Median = median(c_across(contains("Analyte"))))

# 1) MFI plot =============================================================
# THis was apparently wrong. they dont care about samples but analytes
# Mean MFI plot: MFI is median already So it's kind of double averaging. 
# plot_df <- sample_data %>% 
#     filter(Data_type == "Median") %>% 
#     select(Plate.id, Date, Week, Sample.id, Mean, Median)
head(bridge_data)

plot_df <-
    bridge_data %>% 
    filter(Data_type == "Median") %>% 
    select(Plate.id, Date, Week, Sample.id, contains("Analyte")) %>%
    pivot_longer(cols = contains("Analyte"), names_to = "Analyte", values_to = "MFI") %>%
    group_by(Plate.id, Date, Analyte) %>% 
    summarise(Mean_MFI = mean(MFI, na.rm = T),
              Median_MFI = median(MFI,  na.rm = T))
head(plot_df)

# Lineplot Mean MFI
p1 <-
    ggplot(plot_df, aes(x = Analyte, y = Mean_MFI, color = Date, group = Date)) + 
    geom_line(linewidth = 1) +
    theme_classic() + theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

# Lineplot Median MFI
p2 <- ggplot(plot_df, aes(x = Analyte, y = Median_MFI, color = Date, group = Date)) + 
    geom_line(linewidth = 1) +
    theme_classic() + theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

cowplot::plot_grid(p1, p2, ncol = 1, align = "hv")

# 2) Counts box plots ==========================================================
# Now keep the counts instaed of median in data type col.
# box_plot_df <- sample_data %>% 
#     filter(Data_type == "Count") %>% 
#     select(Plate.id, Date, Week, Sample.id, Mean, Median)

box_plot_df <-
    sample_data %>% 
    filter(Data_type == "Count") %>% 
    select(Plate.id, Date, Week, Sample.id, contains("Analyte")) %>%
    pivot_longer(cols = contains("Analyte"), names_to = "Analyte", values_to = "Count") %>%
    group_by(Plate.id, Date, Analyte) %>% 
    summarise(Mean_Counts = mean(Count),
              Median_Counts = median(Count))

head(box_plot_df)
# Boxes Mean
p3 <- ggplot(box_plot_df, aes(x = Analyte, y = Mean_Counts)) + 
    geom_boxplot() +
    theme_classic() + theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

# Boxes Median
p4 <- ggplot(box_plot_df, aes(x = Analyte, y = Median_Counts)) + 
    geom_boxplot() +
    theme_classic() + theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

cowplot::plot_grid(p3, p4, ncol = 1, align = "hv")

head(bridge_data)
box_plot_bridge_df <-
    bridge_data %>% 
    filter(Data_type == "Count") %>% 
    select(Plate.id, Date, Week, Sample.id, contains("Analyte")) %>%
    pivot_longer(cols = contains("Analyte"), names_to = "Analyte", values_to = "Count") %>%
    group_by(Plate.id, Date, Analyte) %>% 
    summarise(Mean_Counts = mean(Count),
              Median_Counts = median(Count))

head(box_plot_bridge_df)
# Boxes Mean
p5 <- ggplot(box_plot_bridge_df, aes(x = Analyte, y = Mean_Counts)) + 
    geom_boxplot() +
    theme_classic() + theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

# Boxes Median
p6 <- ggplot(box_plot_bridge_df, aes(x = Analyte, y = Median_Counts)) + 
    geom_boxplot() +
    theme_classic() + theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

cowplot::plot_grid(p5, p6, ncol = 1, align = "hv")

# 3) Blank plots ===============================================================
# Blanks per date ~ MFI 
blank_data <- sample_data %>% 
    filter(grepl("blank", Sample.id) & Data_type == "Median") %>% 
    select(Plate.id, Date, Sample.id, contains("Analyte")) %>%
    pivot_longer(cols = contains("Analyte"), names_to = "Analyte", values_to = "MFI")

head(blank_data, 2)

# TODO Remove the log! Used it cos variation is way too high. Probably just an issue with the dummy data
ggplot(blank_data, aes(x = Date, y = log(MFI))) + 
    geom_boxplot() +
    theme_classic() + theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

# 4) Delta T plots =============================================================
# system's delta temperature per date 
delta_temp_data <- sample_data %>% 
    select(Plate.id, Date, Delta_t) 
# Remove duplicated rows as all plates have the delta temp only once per plate. 
delta_temp_data <- delta_temp_data[!duplicated(delta_temp_data),]
head(delta_temp_data)

# Delta T point plot
ggplot(delta_temp_data, aes(x = Date, y = Delta_t)) + 
    geom_point() +
    theme_classic() + theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

# Also for bridging data
# system's delta temperature per date 
delta_temp_bridge <- bridge_data %>% 
    select(Plate.id, Date, Delta_t) 
# Remove duplicated rows as all plates have the delta temp only once per plate. 
delta_temp_bridge <- delta_temp_bridge[!duplicated(delta_temp_bridge),]
head(delta_temp_bridge)

# Delta T point plot
ggplot(delta_temp_bridge, aes(x = Date, y = Delta_t)) + 
    geom_point() +
    theme_classic() + theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

# 4) Average per plate
# X-Axis would be the plates, y-axis the MFI values. One line (color) per analyte. 
head(sample_data)
plate_plot_df <- sample_data %>% 
    filter(Data_type == "Median") %>% 
    select(Plate.id, Date, contains("Analyte")) %>%
    pivot_longer(cols = contains("Analyte"), names_to = "Analyte", values_to = "MFI") %>%
    group_by(Plate.id, Date, Analyte) %>% 
    summarise(Mean_MFI = mean(MFI),
              Median_MFI = median(MFI))
head(plate_plot_df)

# Lineplot Mean platewise 
p5 <- ggplot(plate_plot_df, aes(x = Plate.id, y = Mean_MFI, color = Analyte, group = Analyte)) + 
    geom_line(linewidth = 1) +
    theme_classic() + theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

# Lineplot Mean platewise 
p6 <- ggplot(plate_plot_df, aes(x = Plate.id, y = Median_MFI, color = Analyte, group = Analyte)) + 
    geom_line(linewidth = 1) +
    theme_classic() + theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

cowplot::plot_grid(p5, p6, ncol = 1, align = "hv")
