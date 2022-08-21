# -------------------------------------------------------------------------------------------
# This is part of the work used for the publication Neyret et al. 2022. Landscape management for multifunctionality and Equity. Nature Sustainability.
# by Margot Neyret

# In this script, raw data of acoustic diversity and NDSI is formatted into a usable form.

# Input: raw acoustic data from forest and grasslands.
# Output: Acoustic_diversity_by_plot datatable with all acoustic data correctly formatted.
# -------------------------------------------------------------------------------------------

# Load pakages
library(lubridate)
library(hms)

setwd('~/Desktop/Research/Senckenberg/Project_Sophie_P4/Landscape_composition/Code')
set.seed(101)


# Load data
SCH_grasslands = fread("Raw_data/Data_to_load/24692_Acoustic indices based on environmental sound recordings on all grassland EPs, SCH, 2016_2/24692.txt")
ALB_grasslands = fread("Raw_data/Data_to_load/24666_Acoustic indices based on environmental sound recordings on all grassland EPs, Alb, 2016_4/24666.txt")
HAI_grasslands = fread("Raw_data/Data_to_load/24691_Acoustic indices based on environmental sound recordings on all grassland EPs, HAI, 2016_2/24691.txt")
SCH_forest = fread("Raw_data/Data_to_load/27570_Acoustic_diversity_index_based_on_environmental_sound_recordings_on_all_forest_EPs_SCH_2016_1.1.0/27570.txt")
ALB_forest = fread("Raw_data/Data_to_load/27569_Acoustic_diversity_index_based_on_environmental_sound_recordings_on_all_forest_EPs_Alb_2016_1.1.0/27569.txt")
HAI_forest = fread("Raw_data/Data_to_load/27568_Acoustic_diversity_index_based_on_environmental_sound_recordings_on_all_forest_EPs_HAI_2016_4.1.5/27568.txt")

# Reformat time and date
SCH_forest[, time := sapply(time, function(x){unlist(strsplit(x, ' '))[2]})]
ALB_forest[, time := sapply(time, function(x){unlist(strsplit(x, ' '))[2]})]
HAI_forest[, time := sapply(time, function(x){unlist(strsplit(x, ' '))[2]})]

SCH_forest[, date := mdy(sapply(date, function(x){unlist(strsplit(x, ' '))[1]}))]
ALB_forest[, date := mdy(sapply(date, function(x){unlist(strsplit(x, ' '))[1]}))]
HAI_forest[, date := mdy(sapply(date, function(x){unlist(strsplit(x, ' '))[1]}))]
HAI_grasslands[, time := sapply(time, function(x){unlist(strsplit(x, ' '))[2]})]

SCH_grasslands[, date := dmy(date)]
ALB_grasslands[, date := dmy(date)]
HAI_grasslands[, date := dmy(date)]

# No NDSI data in forests
ALB_forest$NDSI = NA
HAI_forest$NDSI = NA
SCH_forest$NDSI = NA

# Put all data together
All_data = rbindlist(list(ALB_grasslands[!is.na(ADI), .SD, .SDcols = c('date', 'time', 'PlotID', 'ADI', 'NDSI')], 
                          SCH_grasslands[!is.na(ADI), .SD, .SDcols = c('date', 'time', 'PlotID', 'ADI', 'NDSI')], 
                          HAI_grasslands[!is.na(ADI), .SD, .SDcols = c('date', 'time', 'PlotID', 'ADI', 'NDSI')],
                          ALB_forest[!is.na(ADI), .SD, .SDcols = c('date', 'time', 'PlotID', 'ADI', 'NDSI')], 
                          SCH_forest[!is.na(ADI), .SD, .SDcols = c('date', 'time', 'PlotID', 'ADI', 'NDSI')], 
                          HAI_forest[!is.na(ADI), .SD, .SDcols = c('date', 'time', 'PlotID', 'ADI', 'NDSI')]))
All_data[, c('time', 'date_time') := list(as_hms(time),
                                          ymd_hms(paste(gsub('\\.', '_', date), time, sep = ' ')))]
All_data[, Region := substr(PlotID, 1, 1)]

# Filter over active 2-months period
Filtered_data = All_data[date >= dmy('01/04/2016') & date <= dmy('31/05/2016') &
                           time  >= as_hms('07:00:00') & time  <= as_hms('19:00:00'), ]
Filtered_data_by_day = Filtered_data[, list(ADI = mean(ADI), NDSI = mean(NDSI)), by = c('date', 'Region', 'PlotID')]
Acoustic_diversity_by_plot = Filtered_data_by_day[, list(ADI = mean(ADI), NDSI = mean(NDSI)), by = c('Region', 'PlotID')]
Acoustic_diversity_by_plot[, Plot := PlotID]

