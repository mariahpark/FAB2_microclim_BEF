# Get microclimate stats and combine microclimate data into one dataframe

rm(list=ls())
#libraries
library(openair)
library(lubridate)
library(ggplot2)
library(dplyr)
library(plyr)
library(data.table)
library(purrr)
library(RColorBrewer)
library(gridExtra)
library(ggiraphExtra)
library(grid)
library(tidyverse)
library(tidyr)

# Description of openair package
# https://davidcarslaw.github.io/openair/reference/aqStats.html

# Read metadata
setwd("c:/Users/maria/Desktop/Research/2024/microclimate/meta_dat")
meta_dat <- read.csv("exp_design.csv")

# Get multiple microclimate files
setwd("c:/Users/maria/Desktop/Research/2024/microclimate")

# Assuming your working directory is the folder with the CSVs
myfiles <- dir(pattern = ".(csv|CSV)$", full.names = TRUE) # get filenames and paths
dflist <- lapply(myfiles, data.table::fread) # read data from files, quickly
head(dflist[[1]]) # validate file reading
names(dflist) <- myfiles # assign names to list items

# Note: sensor 56 was mislabeled as the first P76
names(dflist) = c("C3", "10", "101",
                  "102", "103", "106", "108","112", "113", "117", "118",
                  "124", "125", "126", "128", "129", "13", "130", "132",
                  "133", "134", "137", "139", "141", "143", "144", "145", "146",
                  "18", "19", "2", "21", "22", "25", "29", "34", "38",
                  "39", "4", "42", "43", "45", "47", "51", "52", "53", "54",
                  "58", "59", "60", "61", "63", "67", "68",
                  "69", "70", "71", "73", "56", "76", "77", "8", "80", "82",
                  "84", "85", "89", "9", "91", "92", "96", "97", "98", "99")
dflist  

# Add name of plot to dataframe
for(i in 1:length(names(dflist))) {
  dflist[[i]]$plot_name <- names(dflist)[i]
}

# Make sure diversity metrics are numeric
meta_dat$FD_faith.no.root <- as.numeric(meta_dat$FD_faith.no.root)
meta_dat$PD_faith.no.root <- as.numeric(meta_dat$PD_faith.no.root)
meta_dat$FD_PSV <- as.numeric(meta_dat$FD_PSV)
meta_dat$PSV <- as.numeric(meta_dat$PSV)

#-------------------------------------------------------------------------------

# Rename columns (FIRST FUNCTION!!!)
rename_cols <- function(dat){
  colnames(dat) <- c('date', 'temp_c', 'rel_humidity', 'dewpoint_c', 'vpd_kPa', 'plot')
  return(dat)
}

#iterate over .x and do .f
renamed_df <- map(.x = dflist, .f = rename_cols)
#check first one
renamed_df[[1]]

# MAKE SURE WE'RE IN CENTRAL TIME ZONE
# REPLACE VS ADJUST
timezoned <- function(dat){
  dat$date <- force_tz(dat$date, tz = "America/Chicago")
  #dat$date <- with_tz(dat$date, "America/Chicago")
  return(dat)
}

tz.dat <- map(.x = renamed_df, .f = timezoned)

# From all plots - remove 5/29 (wasp nest spray date)

filter_dat <- function(dat){
  #make date into characters in order to filter
  dat$date <- as.character(dat$date)
  filtered.dat <- dat %>% filter(!(date >= "2024-05-29 00:00:00" & date <= "2024-05-30 00:00:00"))
  return(filtered.dat)
}

filtered_dat <- map(.x = tz.dat, .f = filter_dat)

# Filter out early dates (not all sensors installed yet)
late_dat <- function(dat){
  late_dat <- dat %>% filter(date >= "2024-05-02 00:00:00")
  return(late_dat)
}

# Iterate and check that it worked!
summer_dat <- map(.x = filtered_dat, .f = late_dat)
summer_dat[[20]]

summer_dat_2 <- summer_dat

# Remove p117 for sampling week average - sensor not active
summer_dat_2 <- summer_dat_2[-10]


# Filter summer dat 2 for the sampling week
filter_dat2 <- function(dat){
  filtered_dat <- dat %>% filter(date >= "2024-07-16 00:00:00" & date <= "2024-07-20 00:00:00")
  return(filtered_dat)
}

week_dat <- map(.x = summer_dat_2, .f = filter_dat2)

# Account for central time zone
# timezoned.2 <- function(dat){
#   dat$date <- as.POSIXct(dat$date, tz = "America/Chicago")
#   return(dat)
# }

#week_dat <- map(.x = week_dat, .f = timezoned.2)


#save sampling week data as a R object
save(week_dat, file = "sampling_wk_microclim.RData")


#merge files to combine diversity x microclim data
#make function to merge files

micro_stats_wk <- function(dat){
  week_dat <- aqStats(selectByDate(dat, month = 07),pollutant = c("vpd_kPa", "temp_c", "rel_humidity"), percentile = c(25,75,95))

    return(week_dat)
}

stat_sample_wk2 <- map(.x = week_dat, .f = micro_stats_wk)


# Add in plot names
for(i in 1:length(names(stat_sample_wk2))) {
  stat_sample_wk2[[i]]$plot <- names(summer_dat_2)[i]
}


#######
# Add in diversity data

merge_dat <- function(dat){
  merged <- merge(dat, meta_dat, by="plot")
  return(merged)
}

merged_df <- map(.x = stat_sample_wk2, .f = merge_dat)
merged_df[[10]]

# Bind and export data
bind_df<- bind_rows(merged_df, .id = "plot")

fwrite(bind_df,"c:/Users/maria/Desktop/Research/2024/microclimate/processed_data/merged_sample_wk_df_5_1_25.csv")
