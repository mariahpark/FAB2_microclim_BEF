# 2nd round outlier checking: trees beyond 2sd difference from mean volume per species

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
#' Read outlier data
setwd("C:/Users/maria/Desktop/Research/2024/biomass/")
outliers <- read.csv("to.check.2sd.4.17.25.csv")
plots.to.check <- read.csv("plots.to.check.nbe.4.17.25.csv")
#' 
#' Working path
setwd("C:/Users/maria/Desktop/Research/2024/processed_df/")
root_path <- "C:/Users/maria/Desktop/Research/2024/processed_df/"


#' -----------------------------------------------------------------------------
#' Processing

# Load data
data <- fread(paste0(root_path, "biomass.volume.all.years.12.10.24.csv")) #pre-cleaned


# Define date
data$measurement_date <- as.Date(data$measurement_date, format= "%m/%d/%Y")
data$measurement_date <- as.IDate(data$measurement_date)
data$measurement_year <- year(data$measurement_date)

# Remove data minor errors
data[species == "Juniperus virginia", species := "Juniperus virginiana"]
data[species == "Tilia america", species := "Tilia americana"]

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
# Work on small plots and 2024 data 

frame_small <- subset(data, plot_area == 100)
frame_small <- frame_small %>%
  filter(nchar(plot) <= 3)

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
#  Just small plots
trees <- frame_small

#continue with NBE calcs
# Reshape 2023
trees_2023 <- subset(trees, year(measurement_date) == 2023)
trees_2023 <- trees_2023[, c("plot", "plot_new", "individual_id", 
                             "species", "year_planted",
                             "deadmissing", "measurement_date", "volume", "species_richness")]
colnames(trees_2023)[6:8] <- c("deadmissing_2023", "date_2023", "volume_2023")

# Reshape 2024
trees_2024 <- subset(trees, year(measurement_date) == 2024)
trees_2024 <- trees_2024[, c("plot", "plot_new", "individual_id", 
                             "species", "year_planted",
                             "deadmissing", "measurement_date", "volume", "species_richness")]

colnames(trees_2024)[6:8] <- c("deadmissing_2024", "date_2024", "volume_2024")

# Merge years
inventories <- merge(trees_2023, trees_2024, by = c("plot", "plot_new", "individual_id", "species", "year_planted", "species_richness"),
                     all.x = TRUE, all.y = TRUE)

##make sure in IDate format
inventories[, date_2023 := as.IDate(date_2023)]
inventories[, date_2024 := as.IDate(date_2024)]

# Add potential missing dates based on averages

mean(yday(inventories$date_2024), na.rm = TRUE)
inventories[is.na(date_2024), date_2024 := as.IDate("2024-10-13")]

mean(yday(inventories$date_2023), na.rm = TRUE)
inventories[is.na(date_2023), date_2023 := as.IDate("2023-10-20")]

# Remove trees that where replanted in 2021 - 2024 ; keep 2019; all of alive plot 84 TIAM are from 2019
# Keep 2020 replanting; have 4 years of growth until 2024

inventories <- inventories[year_planted != 2021 &
                             year_planted != 2022 & year_planted != 2023 & year_planted != 2024,]

# Estimate AWP per tree
inventories$tree_AWP <- (inventories$volume_2024 - inventories$volume_2023) /
  ((inventories$date_2024 - inventories$date_2023)/365.25)

# Include the effect of mortality
inventories[!is.na(volume_2023) & is.na(volume_2024), tree_AWP := 0]

#-------------------------------------------------------------------------------
# Don't include pre-identified outliers in calculating mean
## (> 2sd difference in growth between years 2022-2023, and/or 2023-2024)
inventories <- anti_join(inventories, outliers, by = "individual_id")

#-------------------------------------------------------------------------------
# Check outliers more robustly: compare to all individuals of species in experiment
outlier_list <- inventories %>%
  group_by(species) %>%
  mutate(
    mean = mean(tree_AWP, na.rm = TRUE),
    sd = sd(tree_AWP,  na.rm = TRUE),
    is_outlier = abs(tree_AWP - mean) > 2 * sd
  ) %>%
  filter(is_outlier) %>%
  select(plot_new, species, individual_id, tree_AWP)

print(outlier_list)

# Export outlier list for manual checking
fwrite(outlier_list, "outlier.individ.4.19.25.csv")
