# Clean soil moisture data in each plot

rm(list=ls())
library(ggplot2)
library(dplyr)
library(purrr)
library(tidyverse)
library(data.table)
library(anytime)

# Read data
setwd("c:/Users/maria/Desktop/Research/2024/soil_moisture/soil_moisture_2024_organized/exported_dataframes/")
dat <- read.csv("p141_new.csv")

# Find min battery value:
min(dat$Battery_V)
min_values <- dat[dat$Battery_V < 3.4, "vwc"]
min_values

# Filter to remove dying battery effect of extremely high VWC readings
# Retain data trends
filtered_dat <- subset(dat, dat$Battery_V > 3.4)

# Graph the trends
ggplot(filtered_dat,aes(x=Time_UTC,y=vwc))+
  geom_point(size=1)

# Delete unnecessary columns if they exist
#colnames(filtered_dat)
#filtered_dat <-
#  subset(filtered_dat, select = -c(SN...e00fce683a06675f90555594, V2, V3, V4))

#-------------------------------------------------------------------------------
# Convert the datetime column to Date

# Put date time into POSIXct format
filtered_dat$date<- as.POSIXct(filtered_dat$Time_UTC, format = "%Y/%m/%d %H:%M:%S")

# NOTE: Plot 8,10 have this format
#  filtered_dat$date<- as.POSIXct(filtered_dat$Time_UTC, format = "%m/%d/%Y %H:%M")

filtered_dat$date <- as.POSIXct(filtered_dat$date, format = "%Y-%m-%d %H:%M:%S")

# MAKE SURE WE'RE IN CENTRAL TIME ZONE
filtered_dat$date <- force_tz(filtered_dat$date, tz = "America/Chicago")

# Put in character format to preserve format
filtered_dat$date <- format(filtered_dat$date, "%Y-%m-%d %H:%M:%S")

# Make vwc into %
filtered_dat$vwc_percent <- filtered_dat$vwc * 100

# Export data
fwrite(filtered_dat,"c:/Users/maria/Desktop/Research/2024/soil_moisture/jul25_onwards/exported_dataframes/clean_df/p141_clean.csv")
