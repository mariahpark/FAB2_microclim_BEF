# Calculate amplitudes of microclimate metrics for sampling week

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


# Load sampling week microclimate data into here
setwd("c:/Users/maria/Desktop/Research/2024/microclimate/processed_data")
load("sampling_wk_microclim.RData")

# Remove p128 for sampling week average - sensor not active
week_dat <- week_dat[-14]

#-------------------------------------------------------------------------------
# Make function to extract predicted amplitudes of sampling week data

# Temperature amplitude function
fit_sine_to_list <- function(df_list) {
  # Initialize an empty list to store results
  results <- list()
  
  # Loop through each data frame in the list
  for (i in seq_along(df_list)) {
    dat <- df_list[[i]]  # Extract the current data frame
    
    # Ensure the 'date' and 'temp_c' columns exist
    if (!("date" %in% names(dat)) || !("temp_c" %in% names(dat))) {
      warning(paste("Data frame", i, "does not contain required columns. Skipping..."))
      next
    }
    
    # Create a 'time' variable (number of 30-minute intervals from the start)
    dat$time <- as.numeric(difftime(dat$date, min(dat$date), units = "mins")) / 30
    
    # Fit the sine function to the 30-minute data
    fit <- tryCatch({
      nls(temp_c ~ A * sin(B * time + C) + D, 
          data = dat,
          start = list(A = 10, B = 2 * pi / (24 * 2), C = 0, D = mean(dat$temp_c)))
    }, error = function(e) {
      message(paste("Model fitting failed for data frame", i, ":", e$message))
      return(NULL)  # Return NULL if fitting fails
    })
    
    # Check if the fit was successful
    if (!is.null(fit)) {
      # Extract amplitude
      temp_amplitude <- coef(fit)["A"]
      dat$temp_amp <- temp_amplitude  # Add amplitude to the data frame
    } else {
      dat$temp_amp <- NA  # Assign NA if fitting failed
    }
    
    # Store the modified data frame in the results list
    results[[i]] <- dat
  }
  
  return(results)  # Return the list of modified data frames
}


results_list <- fit_sine_to_list(week_dat)


# Relative humidity amplitude function
fit_sine_to_list_rh <- function(df_list) {
  # Initialize an empty list to store results
  results <- list()
  
  # Loop through each data frame in the list
  for (i in seq_along(df_list)) {
    dat <- df_list[[i]]  # Extract the current data frame
    
    # Ensure the 'date' and 'rel_humidity' columns exist
    if (!("date" %in% names(dat)) || !("rel_humidity" %in% names(dat))) {
      warning(paste("Data frame", i, "does not contain required columns. Skipping..."))
      next
    }
    
    # Create a 'time' variable (number of 30-minute intervals from the start)
    dat$time <- as.numeric(difftime(dat$date, min(dat$date), units = "mins")) / 30
    
    # Fit the sine function to the 30-minute data
    fit <- tryCatch({
      nls(rel_humidity ~ A * sin(B * time + C) + D, 
          data = dat,
          start = list(A = 10, B = 2 * pi / (24 * 2), C = 0, D = mean(dat$rel_humidity)))
    }, error = function(e) {
      message(paste("Model fitting failed for data frame", i, ":", e$message))
      return(NULL)  # Return NULL if fitting fails
    })
    
    # Check if the fit was successful
    if (!is.null(fit)) {
      # Extract amplitude
      rh_amp <- coef(fit)["A"]
      dat$rh_amp <- rh_amp  # Add amplitude to the data frame
    } else {
      dat$rh_amp <- NA  # Assign NA if fitting failed
    }
    
    # Store the modified data frame in the results list
    results[[i]] <- dat
  }
  
  return(results)  # Return the list of modified data frames
}


results_list <- fit_sine_to_list_rh(results_list)


# Vapor pressure deficit amplitude function
fit_sine_to_list_vpd <- function(df_list) {
  # Initialize an empty list to store results
  results <- list()
  
  # Loop through each data frame in the list
  for (i in seq_along(df_list)) {
    dat <- df_list[[i]]  # Extract the current data frame
    
    # Ensure the 'date' and 'vpd_kPa' columns exist
    if (!("date" %in% names(dat)) || !("vpd_kPa" %in% names(dat))) {
      warning(paste("Data frame", i, "does not contain required columns. Skipping..."))
      next
    }
    
    # Create a 'time' variable (number of 30-minute intervals from the start)
    dat$time <- as.numeric(difftime(dat$date, min(dat$date), units = "mins")) / 30
    
    # Fit the sine function to the 30-minute data
    fit <- tryCatch({
      nls(vpd_kPa ~ A * sin(B * time + C) + D, 
          data = dat,
          start = list(A = 10, B = 2 * pi / (24 * 2), C = 0, D = mean(dat$vpd_kPa)))
    }, error = function(e) {
      message(paste("Model fitting failed for data frame", i, ":", e$message))
      return(NULL)  # Return NULL if fitting fails
    })
    
    # Check if the fit was successful
    if (!is.null(fit)) {
      # Extract amplitude
      vpd_amp <- coef(fit)["A"]
      dat$vpd_amp <- vpd_amp  # Add amplitude to the data frame
    } else {
      dat$vpd_amp <- NA  # Assign NA if fitting failed
    }
    
    # Store the modified data frame in the results list
    results[[i]] <- dat
  }
  
  return(results)  # Return the list of modified data frames
}

results_list <- fit_sine_to_list_vpd(results_list)


add_col <- function(dat){
  #make another plot column for binding purposes
  dat$plot_2 <- dat$plot
  return(dat)
}


#iterate over .x and do .f
results_list <- map(.x = results_list, .f = add_col)
#check first one
results_list[[10]]


# Bind dataframes together
bind_df<- bind_rows(results_list, .id = "plot_2")

# Convert dates to character strings before saving to retain formatting
bind_df$date <- format(bind_df$date, "%Y-%m-%d %H:%M:%S")

# Have one entry per plot
df_unique <- bind_df[!duplicated(bind_df$plot), ]

# Make sure amplitudes are positive
cols <- c("temp_amp", "rh_amp", "vpd_amp")

df_unique[, (cols) := lapply(.SD, abs), .SDcols = cols]

fwrite(df_unique,"c:/Users/maria/Desktop/Research/2024/microclimate/processed_data/merged_sample_wk_amplitude_df_5_5_25.csv")

