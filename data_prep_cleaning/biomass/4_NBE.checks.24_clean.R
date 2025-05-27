# 1st round checking for outliers based on differences in volumes between 2022-2023/2023-2024
# Use 2sd outlier cutoff

rm(list=ls())
################################################################################
#' -----------------------------------------------------------------------------
#' Libraries
library(data.table)
library(dplyr)
library(conflicted)
options(scipen = 99999)
conflicts_prefer(data.table::year)
conflicts_prefer(data.table::yday)
conflicts_prefer(dplyr::filter)
#' -----------------------------------------------------------------------------
#' Working path
setwd("C:/Users/maria/Desktop/Research/2024/processed_df/")
root_path <- "C:/Users/maria/Desktop/Research/2024/processed_df/"

#' -----------------------------------------------------------------------------
#' Processing

# Load data
data <- fread(paste0(root_path, "biomass.volume.all.years.12.10.24.csv"))

# Define date
data$measurement_date <- as.Date(data$measurement_date, format= "%m/%d/%Y")
data$measurement_date <- as.IDate(data$measurement_date)
data$measurement_year <- year(data$measurement_date)

# Remove data minor errors
data[species == "Juniperus virginia", species := "Juniperus virginiana"]
data[species == "Tilia america", species := "Tilia americana"]

data[plot == "1020-154", plot := "1020_154"]
data[plot == "1020-2016", plot := "1020_2016"]
data[plot == "1020-2017", plot := "1020_2017"]
data[plot == "1020-2018", plot := "1020_2018"]
data[plot == "1011-149", plot := "1011_149"]
data[plot == "1011-150", plot := "1011_150"]
data[plot == "1019-152", plot := "1019_152"]
data[plot == "1019-153", plot := "1019_153"]

#Removing columns

data <- data[, c("species_richness", 
                 "species",
                 "year_planted",
                 "treatment", 
                 "plot_area",
                 "block",
                 "plot",
                 "row",
                 "column",
                 "position",
                 "individual_id",
                 "measurement_date",
                 "measurement_year",
                 "deadmissing",
                 "volume")]

# Transform volume - change cm^3 -> m^3
data$volume <- data$volume/1000000
#-------------------------------------------------------------------------------
# Work on small plots

frame_small <- subset(data, plot_area == 100)

# Rename row and columns ** because 11 - 20 don't exist
frame_small[row == 11, row := 1]
frame_small[row == 12, row := 2]
frame_small[row == 13, row := 3]
frame_small[row == 14, row := 4]
frame_small[row == 15, row := 5]
frame_small[row == 16, row := 6]
frame_small[row == 17, row := 7]
frame_small[row == 18, row := 8]
frame_small[row == 19, row := 9]
frame_small[row == 20, row := 10]

frame_small[column == 11, column := 1]
frame_small[column == 12, column := 2]
frame_small[column == 13, column := 3]
frame_small[column == 14, column := 4]
frame_small[column == 15, column := 5]
frame_small[column == 16, column := 6]
frame_small[column == 17, column := 7]
frame_small[column == 18, column := 8]
frame_small[column == 19, column := 9]
frame_small[column == 20, column := 10]

# Remove edges
frame_small <- frame_small[column != 1,]
frame_small <- frame_small[column != 10,]
frame_small <- frame_small[row != 1,]
frame_small <- frame_small[row != 10,]

# Rename plot for new merge with large plots
frame_small$plot_new <- frame_small$plot

#-------------------------------------------------------------------------------
# Just working with small plots
trees <- frame_small

# Reshape 2022
trees_2022 <- subset(trees, year(measurement_date) == 2022)
trees_2022 <- trees_2022[, c("plot", "plot_new", "individual_id", 
                             "species", "year_planted",
                             "deadmissing", "measurement_date", "volume")]
colnames(trees_2022)[6:8] <- c("deadmissing_2022", "date_2022", "volume_2022")

# Reshape 2023
trees_2023 <- subset(trees, year(measurement_date) == 2023)
trees_2023 <- trees_2023[, c("plot", "plot_new", "individual_id", 
                             "species", "year_planted",
                             "deadmissing", "measurement_date", "volume")]
colnames(trees_2023)[6:8] <- c("deadmissing_2023", "date_2023", "volume_2023")

# Reshape 2024
trees_2024 <- subset(trees, year(measurement_date) == 2024)
trees_2024 <- trees_2024[, c("plot", "plot_new", "individual_id", 
                             "species", "year_planted",
                             "deadmissing", "measurement_date", "volume")]

colnames(trees_2024)[6:8] <- c("deadmissing_2024", "date_2024", "volume_2024")


# Merge years
inventories <- merge(trees_2022, trees_2023, by = c("plot", "plot_new", "individual_id", "species", "year_planted"),
                     all.x = TRUE, all.y = TRUE)
inventories <- merge(inventories, trees_2024, by = c("plot", "plot_new", "individual_id", "species", "year_planted"),
                     all.x = TRUE, all.y = TRUE)

# Add potential missing dates based on averages
mean(yday(inventories$date_2022), na.rm = TRUE)
inventories[is.na(date_2022), date_2022 := as.IDate("2022-10-13")]

mean(yday(inventories$date_2024), na.rm = TRUE)
inventories[is.na(date_2024), date_2024 := as.IDate("2024-10-14")] #slight change

mean(yday(inventories$date_2023), na.rm = TRUE)
inventories[is.na(date_2023), date_2023 := as.IDate("2023-10-21")] #slight change

# Remove trees that where replanted in 2021 - 2024
inventories <- inventories[year_planted != 2021,]
inventories <- inventories[year_planted != 2022,]
inventories <- inventories[year_planted != 2023,]
inventories <- inventories[year_planted != 2024,]

# Estimate AWP per tree (2022-2023)
inventories$tree_AWP_23 <- (inventories$volume_2023 - inventories$volume_2022) /
  ((inventories$date_2023 - inventories$date_2022)/365.25)

# Include the effect of mortality
inventories[!is.na(volume_2022) & is.na(volume_2023), tree_AWP_23 := 0]

# Estimate AWP per tree (2023-2024)
inventories$tree_AWP_24 <- (inventories$volume_2024 - inventories$volume_2023) /
  ((inventories$date_2024 - inventories$date_2023)/365.25)

# Include the effect of mortality
inventories[!is.na(volume_2023) & is.na(volume_2024), tree_AWP_24 := 0]

#-------------------------------------------------------------------------------
# Outlier detection
# Define range of 2 sd
# Filter trees where volume in 2023 is more than 2 SD away from 2022

#calc sd for 2022 (baseline)
sd_calc <- inventories %>%
  group_by(species) %>%
  summarise(
    sd_volume = sd(volume_2022, na.rm = TRUE),        # Calculate standard deviation
    two_sd = 2 * sd(volume_2022, na.rm = TRUE)        # Multiply by 2
  )

hist(inventories$volume_2022, breaks = 20)

inventories.2 <- merge(inventories, sd_calc, by = "species")

# Filter strange 2023 data based on 2022 sd
sd_diff_23 <- inventories.2 %>%
  group_by(individual_id) %>%
  summarise(
    volume_2022 = volume_2022,
    volume_2023 = volume_2023,
    diff = abs(volume_2023 - volume_2022),           # Absolute difference
    sd_2022 = two_sd # Standard deviation for 2023
  ) %>%
  filter(!is.na(volume_2022) & !is.na(volume_2023) & diff > 2 * sd_2022)

# Filter trees where volume in 2024 is more than 2 SD away from 2023
# Calc sd for 2023 (baseline)
sd_calc_23 <- inventories %>%
  group_by(species) %>%
  summarise(
    sd_volume = sd(volume_2023, na.rm = TRUE),        # Calculate standard deviation
    two_sd = 2 * sd(volume_2023, na.rm = TRUE)        # Multiply by 2
  )

hist(inventories$volume_2023, breaks = 20)

inventories.3 <- merge(inventories, sd_calc_23, by = "species")

# Filter strange 2024 data based on 2023 sd
sd_diff_24 <- inventories.3 %>%
  group_by(individual_id) %>%
  summarise(
    volume_2023 = volume_2023,
    volume_2024 = volume_2024,
    diff = abs(volume_2024 - volume_2023),           # Absolute difference
    sd_2022 = two_sd # Standard deviation for 2023
  ) %>%
  filter(!is.na(volume_2023) & !is.na(volume_2024) & diff > 2 * sd_2022)


to.check <- merge(sd_diff_23, sd_diff_24, by = "individual_id", all = T)

# Check these entries for unreasonable outliers
fwrite(to.check, "C:/Users/maria/Desktop/Research/2024/biomass/to.check.2sd.4.17.25.csv")
