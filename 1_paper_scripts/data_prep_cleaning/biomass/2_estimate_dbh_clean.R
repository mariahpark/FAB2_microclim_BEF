# Estimate DBH of 2024 growth inventory trees using predicted coefficients

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

setwd("c:/Users/maria/Desktop/Research/2024/biomass/")
df <- read.csv("fab2_growth_clean_4_22_25.csv")
coeff <- read.csv("dbh.coefficients.csv")

#-------------------------------------------------------------------------------
# Log the height and drc

colnames(df)[20] <- "H"
colnames(df)[21] <- "drc_mm"
colnames(df)[22] <- "dbh_mm"

# Make sure numeric
df$H <- as.numeric(df$H)
df$drc_mm <- as.numeric(df$drc_mm)
df$dbh_mm <- as.numeric(df$dbh_mm)


df$DRC <- NA
df$DBH <- NA

df$DRC <- df$drc_mm / 10
df$DBH <- df$dbh_mm / 10

# log10 for regressions
df$logdrc <- NA
df$logdbh <- NA
df$logh <- NA

df$logdrc <- log10(df$DRC)
df$logh <- log10(df$H)

#filter 2022-2024
df <- filter(df, measurement_year >= "2022")

#merge coefficients with df
colnames(coeff)[1] <- "species_code"

df <- merge(df, coeff, by = "species_code")

# Filter by species
tiam <- df %>% filter(species_code == "TIAM")
acru <- df %>% filter(species_code == "ACRU")
qual <- df %>% filter(species_code == "QUAL")
quru <- df %>% filter(species_code == "QURU")

acne <- df %>% filter(species_code == "ACNE")
bepa <- df %>% filter(species_code == "BEPA")
quel <- df %>% filter(species_code == "QUEL")
quma <- df %>% filter(species_code == "QUMA")
pist <- df %>% filter(species_code == "PIST")
pire <- df %>% filter(species_code == "PIRE")
piba <- df %>% filter(species_code == "PIBA")
juvi <- df %>% filter(species_code == "JUVI")

df_list <- list(tiam,acru,qual,quru,
                acne,bepa,quel,quma,
                pist,pire,piba,juvi)

#-------------------------------------------------------------------------------
# Function to predict log10 dbh

dbh.fn <- function(df){
  df$dbh <- NA
  log10dbh <- df$intercept + df$coef_drc*df$logdrc + df$coef_H*df$logh
  df$dbh <- 10^log10dbh
  return(df)
  
}

df_dbh <- map(.x = df_list, .f = dbh.fn)

# Make into one dataframe again
dbh_combo <- bind_rows(df_dbh)

# Apply DBH function only to trees > 130cm height
height.filter <- function(df){
  ifelse(df$H < 130, NA, df$dbh)
  return(df)
}

dbh_combo$dbh <- ifelse(dbh_combo$H < 130, NA, dbh_combo$dbh)

# Replace NA in DBH with values from predicted dbh
dbh_combo$DBH <- ifelse(is.na(dbh_combo$DBH) |dbh_combo$DBH == "0" & !is.na(dbh_combo$dbh), dbh_combo$dbh, dbh_combo$DBH)

# Export dataframe 
fwrite(dbh_combo, "fab2_growth_dbh_infill_4_22_25.csv")


