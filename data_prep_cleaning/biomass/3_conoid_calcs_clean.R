rm(list=ls())

library(dplyr)
library(ggplot2)
library(ggpp)
library(ggpmisc)
library(tidyverse)
library(tidyr)
library(xts)
library(data.table)
library(purrr)
library(conflicted)
conflicts_prefer(dplyr::filter)

setwd("c:/Users/maria/Desktop/Research/2024/biomass/")
df <- read.csv("fab2_growth_dbh_infill_4_22_25.csv")
#------------------------------------------------------------------------------- 
# Select measurement dates 2023 - 2024
# Put date time into POSIXct format
df$measurement_date<- as.POSIXct(df$measurement_date, format = "%m/%d/%Y")

# Select trees measured in 2023 and 2024 
df.recent <- filter(df, measurement_date > "2023-01-01")

# Function to calculate tree volume!
calculate_volume <- function(df) {
  # Ensure the dataframe has the necessary columns
  if (!all(c("H", "DRC", "DBH") %in% colnames(df))) {
    stop("Dataframe must contain columns: H, DRC, and DBH (DBH can be NA).")
  }
  
  # Function for conoid volume: V = (1/3) * pi * r^2 * h
  conoid_volume <- function(DRC, H) {
    return((H * pi * DRC^2) / 12)
  }
  
  # Function for base conoid volume
  base_volume <- function(DRC, Hdbh, DBH) {
    return(Hdbh * ((DRC^2) + (DBH * DRC) + (DBH^2)) * (pi / 12))
  }
  
  # Function for top conoid volume
  top_volume <- function(H2, DBH) {
    return((H2 * pi * DBH^2) / 12)
  }
  
  # Initialize volume column
  df$volume <- NA
  
  # Filter out rows with NA in H or DRC
  valid_rows <- !is.na(df$H) & !is.na(df$DRC)
  
  # Calculate volumes for height < 130
  df$volume[valid_rows & df$H < 130] <- conoid_volume(
    df$DRC[valid_rows & df$H < 130], 
    df$H[valid_rows & df$H < 130]
  )
  
  # Calculate volumes for height >= 130
  if (any(valid_rows & df$H >= 130, na.rm = TRUE)) {
    Hdbh <- 130
    H2 <- df$H[valid_rows & df$H >= 130] - 130
    df$volume[valid_rows & df$H >= 130] <- base_volume(
      df$DRC[valid_rows & df$H >= 130], 
      Hdbh, 
      df$DBH[valid_rows & df$H >= 130]
    ) +
      top_volume(H2, df$DBH[valid_rows & df$H >= 130])
  }
  
  return(df)
}

volume <- calculate_volume(df)

# Export dataframe
fwrite(volume, "c:/Users/maria/Desktop/Research/2024/processed_df/biomass.volume.all.years.4.22.25.csv")
