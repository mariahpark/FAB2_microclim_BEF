rm(list=ls())
library(ggplot2)
library(tidyr)
library(dplyr)
library(purrr)
library(ggplot2)
library(tidyverse)
library(data.table)
library(tidyr)
library(xts)
library(broom)
library(ggpmisc)
library(lubridate)
library(gridExtra)

setwd("c:/Users/maria/Desktop/Research/2024/soil_moisture/soil_moisture_2024_organized/clean_exported_dataframes")

# assuming working directory is the folder with the CSVs
myfiles <- dir(pattern = ".(csv|CSV)$", full.names = TRUE) # get filenames and paths
dflist <- lapply(myfiles, data.table::fread) # read data from files, quickly
head(dflist[[1]]) # validate file reading
names(dflist) <- myfiles # assign names to list items

names(dflist) = c("C3","10","102", "103","125","13","130","137","139","141",
                  "2","25","38","39","53","60","70","71","8","84","85","9","92","98")

#-------------------------------------------------------------------------------
# Make plot is a character and date into POSIXct form
character <- function(dat){
  dat$plot <- as.character(dat$plot)
  return(dat)
}

dflist <- map(.x = dflist, .f = character)

# Make sure we're in Central Time Zone
timezoned <- function(dat){
  dat$date <- force_tz(dat$date, tz = "America/Chicago")
  return(dat)
}

dflist.2 <- map(.x = dflist, .f = timezoned)


# Function to filter VWC for July **make time zones match

jul <- function(dat){
  filtered_data_range <- dat %>%
    filter(date >= as.POSIXct("2024-07-01 00:00:00", tz = "America/Chicago")
           & date < as.POSIXct("2024-08-01 00:00:00", tz = "America/Chicago"))
}

jul.dat <- map(.x = dflist.2, .f = jul)

# Get stats for July
range.fn <- function(dat){
  ###vwc
  dat$vwc_max <- max(dat$vwc)
  dat$vwc_q95 <- quantile(dat$vwc, probs = 0.95)
  dat$vwc_mean <- mean(dat$vwc)
  dat$vwc_med <- median(dat$vwc)
  dat$vwc_min <- min(dat$vwc)
  #range
  dat$vwc_range <- (dat$vwc_max - dat$vwc_min)
  return(dat)
}

jul.stats <- map(.x = jul.dat, .f = range.fn)

# Merge into dataframe
jul <- bind_rows(jul.stats)

# Add in month column
jul$month <- "7"

# Keep one row for each plot
distinct.dat <- distinct(jul, plot, month, .keep_all = TRUE)

#save the data
fwrite(jul,"c:/Users/maria/Desktop/Research/2024/processed_df/total.vwc.july.2024.stats.5.9.25.csv")
fwrite(distinct.dat,"c:/Users/maria/Desktop/Research/2024/processed_df/vwc.july.2024.stats.5.9.25.csv" )
