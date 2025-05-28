# Develop coefficients to estimate log(DBH) from log(DRC) and log(Height)
# Will use predicted DBH to estimate biomass in 2024 (no DBH measurements)

rm(list=ls())

library(dplyr)
library(ggplot2)
library(ggpp)
library(ggpmisc)
library(tidyverse)
library(tidyr)
library(xts)
library(data.table)

# Read data and format variables

setwd("c:/Users/maria/Desktop/Research/2024/processed_df/clean.data/MARIA_data_submission")
df <- read.csv("fab2_growth_and_reproduction_R.csv")

# Height (H), diameter at root collar (DRC), diameter at breast height (DBH)

colnames(df)[30] <- "H"
colnames(df)[31] <- "drc_mm"
colnames(df)[32] <- "dbh_mm"

df$DRC <- NA
df$DBH <- NA

# mm -> cm
df$DRC <- df$drc_mm / 10
df$DBH <- df$dbh_mm / 10

# log10 for regressions
df$logdrc <- NA
df$logdbh <- NA
df$logh <- NA

df$logdrc <- log10(df$DRC)
df$logdbh <- log10(df$DBH)
df$logh <- log10(df$H)

#-------------------------------------------------------------------------------
# Filter data to make species-specific predictive models

filter_valid_rows <- function(df, columns) {
  # Keep rows where all specified columns have finite values
  df[apply(df[columns], 1, function(row) all(is.finite(row))), ]
}

# Specify columns to check
columns_to_check <- c("logdrc", "logdbh", "logh")

# Filter rows
filtered_df <- filter_valid_rows(df, columns_to_check)

# Select measurement dates 2023 - 2024
# Put date time into POSIXct format
filtered_df$measurement_date<- as.POSIXct(filtered_df$measurement_date, format = "%m/%d/%Y")

df.dbh <- filtered_df %>% filter(measurement_date > "2022-01-01" & measurement_date < "2024-01-01")

# Filter by species
tiam <- df.dbh %>% filter(species_code == "TIAM")
acru <- df.dbh %>% filter(species_code == "ACRU")
qual <- df.dbh %>% filter(species_code == "QUAL")
quru <- df.dbh %>% filter(species_code == "QURU")

acne <- df.dbh %>% filter(species_code == "ACNE")
bepa <- df.dbh %>% filter(species_code == "BEPA")
quel <- df.dbh %>% filter(species_code == "QUEL")
quma <- df.dbh %>% filter(species_code == "QUMA")
pist <- df.dbh %>% filter(species_code == "PIST")
pire <- df.dbh %>% filter(species_code == "PIRE")
piba <- df.dbh %>% filter(species_code == "PIBA")
juvi <- df.dbh %>% filter(species_code == "JUVI")

# Logically filter out the outliers (not included in making models)
tiam.filtered <- tiam %>% filter(DBH < DRC & DRC < 10)
acru.filtered <- acru %>% filter(DBH < DRC & DRC < 10)
quru.filtered <- quru %>% filter(DBH < DRC & DRC < 10)
qual.filtered <- qual %>% filter(DBH < DRC & DRC < 10 & H < 2000 & H > DRC)

quel.filtered <- quel %>% filter(DBH < DRC & DRC < 10)
quma.filtered <- quma %>% filter(DBH < DRC & DRC < 10 & DBH < 7.5)
bepa.filtered <- bepa %>% filter(DBH < DRC)
acne.filtered <- acne %>% filter(DBH < DRC & DRC < 10)

pist.filtered <- pist %>% filter(DBH < DRC & DRC < 20)
pire.filtered <- pire %>% filter(DBH < DRC & DRC < 20)
piba.filtered <- piba %>% filter(DBH < DRC & DRC < 20)
juvi.filtered <- juvi %>% filter(DBH < DRC & DRC < 20)


df_list <- list(tiam.filtered,acru.filtered,qual.filtered,quru.filtered,
                acne.filtered,bepa.filtered,quel.filtered,quma.filtered,
                pist.filtered,pire.filtered,piba.filtered,juvi.filtered)

# Rename the list based on the column
rename_dataframes <- function(df_list, col_name) {
  names(df_list) <- sapply(df_list, function(df) {
    unique_name <- as.character(df[[col_name]][1])  # Use the first value of the column
    if (is.na(unique_name) || unique_name == "") {
      paste0("Unnamed_", seq_along(df)) # Fallback for empty or NA names
    } else {
      unique_name
    }
  })
  
  return(df_list)
}

df_list <- rename_dataframes(df_list, "species_code")

#-------------------------------------------------------------------------------
# Function to get model coefficients to ultimately predict DBH
# Loop through dataframes, run regressions, and save results

run_regressions <- function(df_list, response_var, predictor1, predictor2) {
  # Create an empty dataframe to store results
  results <- data.frame(
    dataframe_name = character(),
    intercept = numeric(),
    coef_drc = numeric(),
    coef_H = numeric(),
    r_squared = numeric(),
    stringsAsFactors = FALSE
  )
  
  # Loop through each dataframe in the list
  for (i in seq_along(df_list)) {
    # Get the name of the dataframe (if names exist in the list)
    df_name <- ifelse(!is.null(names(df_list)), names(df_list)[i], paste("df", i, sep = "_"))
    
    # Access the current dataframe
    df <- df_list[[i]]
    
    # Run the linear regression model
    model <- lm(as.formula(paste(response_var, "~", predictor1, "+", predictor2)), data = df)
    
    # Extract coefficients and R-squared
    intercept <- coef(model)[1]
    coef1 <- coef(model)[2]
    coef2 <- coef(model)[3]
    r_squared <- summary(model)$r.squared
    
    # Append results to the dataframe
    results <- rbind(
      results,
      data.frame(
        dataframe_name = df_name,
        intercept = intercept,
        coef_drc = coef1,
        coef_H = coef2,
        r_squared = r_squared,
        stringsAsFactors = FALSE
      )
    )
  }
  
  return(results)
}

# Call the function
result_df <- run_regressions(df_list, response_var = "logdbh", predictor1 = "logdrc", predictor2 = "logh")

print(result_df)

# Export data
fwrite(result_df, "dbh.coefficients.csv")





