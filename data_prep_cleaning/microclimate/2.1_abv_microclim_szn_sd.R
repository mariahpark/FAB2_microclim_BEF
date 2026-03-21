# Get microclimate stats and combine microclimate data into one dataframe

rm(list=ls())
#libraries
library(openair)
library(lubridate)
library(ggplot2)
library(dplyr)
#detach("package:plyr", unload = TRUE)
library(data.table)
library(purrr)
library(RColorBrewer)
library(gridExtra)
library(ggiraphExtra)
library(grid)
library(tidyverse)
library(tidyr)
library(conflicted)
conflict_prefer("summarise", "dplyr")
conflicts_prefer(dplyr::filter)
conflicts_prefer(lubridate::month)

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

# Filter Plot 128 dates when sensor was stalled
tz.dat$`128` <- tz.dat$`128` %>%
  mutate(datetime = ymd_hms(date)) %>%
  filter(!(datetime >= ymd("2024-06-08") & datetime <= ymd("2024-07-21")))

#-------------------------------------------------------------------------------
# Filtering of all datasets
# From all plots - remove 5/29 (wasp nest spray date)

filter_dat <- function(dat){
  
  dat %>%
    mutate(datetime = ymd_hms(date)) %>%
    filter(as.Date(datetime) != as.Date("2024-05-29"))
  
}

filtered_dat <- map(.x = tz.dat, .f = filter_dat)

# Filter out early dates (not all sensors installed yet) and 9/3 (no sd calculated)
late_dat <- function(dat){
  dat %>%
    filter(as.Date(datetime) >= as.Date("2024-05-02") &
                                          as.Date(datetime) < as.Date("2024-09-03"))
}

# Iterate and check that it worked!
summer_dat <- map(.x = filtered_dat, .f = late_dat)
summer_dat[[20]]

summer_dat_2 <- summer_dat

# Remove p117 for sampling week average - sensor not active
#summer_dat_2 <- summer_dat_2[-10]

#-------------------------------------------------------------------------------
# Trim top 5% of data and calculate daily stats

trim_mean <- function(x){
  q <- quantile(x, 0.95, na.rm = TRUE)
  mean(x[x <= q], na.rm = TRUE)
}

trim_sd <- function(x){
  q <- quantile(x, 0.95, na.rm = TRUE)
  sd(x[x <= q], na.rm = TRUE)
}

# Daily stats
daily_stats_safe <- function(dat) {
  
  # Skip empty datasets
  if(nrow(dat) == 0) return(NULL)
  
  # Ensure date column exists and parse
  dat <- dat %>%
    mutate(datetime = ymd_hms(date),
           day = as.Date(datetime))
  
  # Required columns
  required_cols <- c("vpd_kPa", "rel_humidity", "temp_c")
  missing_cols <- setdiff(required_cols, names(dat))
  if(length(missing_cols) > 0){
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }
  
  # Compute daily stats using reframe (robust)
  dat %>%
    group_by(day) %>%
    reframe(
      vpd_trim   = list(vpd_kPa[vpd_kPa <= quantile(vpd_kPa, 0.95, na.rm = TRUE)]),
      reh_trim   = list(rel_humidity[rel_humidity <= quantile(rel_humidity, 0.95, na.rm = TRUE)]),
      temp_trim  = list(temp_c[temp_c <= quantile(temp_c, 0.95, na.rm = TRUE)]),
      
      vpd_mean = mean(unlist(vpd_trim), na.rm = TRUE),
      vpd_sd   = sd(unlist(vpd_trim), na.rm = TRUE),
      vpd_n    = length(unlist(vpd_trim)),
      
      reh_mean = mean(unlist(reh_trim), na.rm = TRUE),
      reh_sd   = sd(unlist(reh_trim), na.rm = TRUE),
      reh_n    = length(unlist(reh_trim)),
      
      temp_mean = mean(unlist(temp_trim), na.rm = TRUE),
      temp_sd   = sd(unlist(temp_trim), na.rm = TRUE),
      temp_n    = length(unlist(temp_trim))
    )
}

daily_out <- map_dfr(
  summer_dat_2,
  ~ tryCatch(daily_stats_safe(.x),
             error = function(e){
               warning("Skipped one dataset due to error: ", conditionMessage(e))
               NULL
             }),
  .id = "plot"
)



# Monthly integration - weighting by number of observations per day
monthly_stats <- daily_out %>%
  mutate(month = month(day)) %>%  
  group_by(plot, month) %>%
  summarise(
    # weighted means
    vpd_mean   = weighted.mean(vpd_mean, vpd_n, na.rm = TRUE),
    reh_mean   = weighted.mean(reh_mean, reh_n, na.rm = TRUE),
    temp_mean  = weighted.mean(temp_mean, temp_n, na.rm = TRUE),
    
    # weighted SD (pooled approximation)
    vpd_sd     = sqrt(weighted.mean(vpd_sd^2, vpd_n, na.rm = TRUE)),
    reh_sd     = sqrt(weighted.mean(reh_sd^2, reh_n, na.rm = TRUE)),
    temp_sd    = sqrt(weighted.mean(temp_sd^2, temp_n, na.rm = TRUE)),
    
    # number of days contributing
    n_days     = dplyr::n(),
    
    .groups = "drop"
  )

# Full logging period integration
season_out <- daily_out %>%
  
  group_by(plot) %>%
  
  summarise(
    
    vpd_mean = weighted.mean(vpd_mean, vpd_n, na.rm = TRUE),
    reh_mean = weighted.mean(reh_mean, reh_n, na.rm = TRUE),
    temp_mean = weighted.mean(temp_mean, temp_n, na.rm = TRUE),
    
    vpd_sd = sqrt(weighted.mean(vpd_sd^2, vpd_n, na.rm = TRUE)),
    reh_sd = sqrt(weighted.mean(reh_sd^2, reh_n, na.rm = TRUE)),
    temp_sd = sqrt(weighted.mean(temp_sd^2, temp_n, na.rm = TRUE)),
    
    total_days = n(),
    
    .groups = "drop"
  )

#-------------------------------------------------------------------------------
# Rename  col names
names(monthly_stats)[3:8] <- paste0(names(monthly_stats)[3:8], "_month")
names(season_out)[2:7] <- paste0(names(season_out)[2:7], "_szn")

monthly_stats$vpd_stable_month <- monthly_stats$vpd_mean_month / monthly_stats$vpd_sd_month

monthly_stats$reh_stable_month <- monthly_stats$reh_mean_month / monthly_stats$reh_sd_month

monthly_stats$temp_stable_month <- monthly_stats$temp_mean_month / monthly_stats$temp_sd_month



#-------------------------------------------------------------------------------
# Calculate seasonal microclimate stability
# Higher values = more stability

season_out$vpd_stable_szn <- season_out$vpd_mean_szn / season_out$vpd_sd_szn

season_out$reh_stable_szn <- season_out$reh_mean_szn / season_out$reh_sd_szn

season_out$temp_stable_szn <- season_out$temp_mean_szn / season_out$temp_sd_szn


#-------------------------------------------------------------------------------
# Merge season and monthly data
merged.dat <- merge(season_out, monthly_stats, by = "plot")

#-------------------------------------------------------------------------------

# Export data
fwrite(merged.dat,"c:/Users/maria/Desktop/Research/2024/microclimate/processed_data/microclim_szn_3.11.26.csv")
fwrite(merged.dat,"c:/Users/maria/Desktop/Research/2024/processed_df/clean.data/microclim_szn_3.11.26.csv")


