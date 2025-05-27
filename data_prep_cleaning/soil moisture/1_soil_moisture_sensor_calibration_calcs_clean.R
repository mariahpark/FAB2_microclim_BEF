# Run through this script for each plot with a soil moisture sensor

rm(list=ls())
library(ggplot2)
library(dplyr)
library(purrr)
library(tidyverse)
library(data.table)

setwd("c:/Users/maria/Desktop/Research/2023/soil_moisture")
dat <- read.csv("soil_moisture_sensor_calibrations_2023.csv")

# VWC Sensor Calibration Models:
# lm(y ~ x, data=dataframe)

# Teros 10 Sensor
# VWC = -0.41514 + 0.39213(teros10_voltage)
teros_mod <- lm(theta_Vw_over_Vt ~ Voltage_teros10, data=dat)
summary(teros_mod)

# Visualize
ggplot(dat,aes(x=Voltage_teros10,y=theta_Vw_over_Vt))+
  geom_point(size=3)+
  geom_line(aes(y=fitted(teros_mod)),col="red",linetype=2)

# GS1 Sensor
# VWC = -0.40056 + 0.38346(gs1_voltage)
gs_mod <- lm(theta_Vw_over_Vt ~ Voltage_GS1, data=dat)
summary(gs_mod)

# Visualize
ggplot(dat,aes(x=Voltage_GS1,y=theta_Vw_over_Vt))+
  geom_point(size=3)+
  geom_line(aes(y=fitted(gs_mod)),col="red",linetype=2)

#-------------------------------------------------------------------------------
# If needed: extra script to merge data entries from same plot
## (due to datalogger issues / swapped sd cards midseason)
setwd("c:/Users/maria/Desktop/Research/2024/soil_moisture/soil_moisture_2024_organized/exported_dataframes")
dat1 <- read.csv("p8_new.csv")
dat2 <- read.csv("p8_new2.csv")

dat3 <- rbind(dat1, dat2)

# Export the data
fwrite(dat3,"c:/Users/maria/Desktop/Research/2024/soil_moisture/soil_moisture_2024_organized/exported_dataframes/p8_combo.csv")

#-------------------------------------------------------------------------------
# Load VWC readings for a plot
setwd("c:/Users/maria/Desktop/Research/2024/soil_moisture/soil_moisture_2024_organized/p60")

# Assuming working directory is the folder with the CSVs
myfiles <- dir(pattern = ".(txt)$", full.names = TRUE) # get filenames and paths
dflist <- lapply(myfiles, data.table::fread) # read data from files, quickly
head(dflist[[1]]) # validate file reading
names(dflist) <- myfiles # assign names to list items

# Merge dflist
# Make new column
## MAKE SURE Plot.No MATCHES EACH PLOT
new_col <- function(dat){
  dat %>%
    mutate(Plot.No="60")
}


dflist_1 <- map(.x = dflist, .f = new_col)
dflist_1[[1]]

# Stack rows from a list into a dataframe
result <- bind_rows(dflist_1)

#-------------------------------------------------------------------------------
# Do the VWC calculations
# NOTE WHICH SENSOR WAS USED FOR EACH PLOT: USE APPROPRIATE FUNCTION

# VWC calculation for Teros 10 sensors
vwc_teros10_1 <- function(dat){
  vwced <- -0.41514 + 0.39213*dat[,3]
  return(vwced)
}

vwc_df_1 <- vwc_teros10_1(result)
result$vwc <- vwc_df_1

# VWC calculations for GS1 sensors
###2024: plots 13, 103 ,137
###2023: plots C2, 84, 141
vwc_gs <- function(dat){
  vwced <- -0.40056 + 0.38346*dat[,3]
  return(vwced)
}

vwc_gs_1 <- vwc_gs(result)
result$vwc <- vwc_gs_1

#-------------------------------------------------------------------------------
# Rename columns
names(result)[1:5] <- c("Time_UTC", "Battery_V", "Soil_Top_V", "Soil_Mid_V", "plot")

# Filter out negatives (noise)
result_filtered <- result %>%
  filter(vwc >= 0)

# Export the data
fwrite(result_filtered,"c:/Users/maria/Desktop/Research/2024/soil_moisture/soil_moisture_2024_organized/exported_dataframes/p60_new.csv")

