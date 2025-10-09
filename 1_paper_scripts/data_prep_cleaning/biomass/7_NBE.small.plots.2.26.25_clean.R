
rm(list=ls())
################################################################################
#' @title Net biodiversity effect
################################################################################

#' @description Estimation of the net biodiversity effect
#' 
#' @return A tiff file

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
data <- fread(paste0(root_path, "biomass.volume.all.years.4.22.25.csv"))


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

# Make sure distinct id per year
data <- distinct(data, individual_id_year, .keep_all = TRUE)

# Removing columns

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
# Work on small plots (10m x 10m) and 2023-2024 data - filter out subdivided 20m x 20m plots
data$plot <- as.numeric(data$plot)
dat_clean <- subset(data, !is.na(plot))

frame_small <- subset(dat_clean, plot_area == 100)

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

# Just small plots
trees <- frame_small

#-------------------------------------------------------------------------------
# Estimation of tree volume growth from 2021 to 2022.
# Sum volume of each species per plot --> input for "volume"

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
inventories <- merge(trees_2023, trees_2024, by = c("plot", "plot_new", "individual_id", "species", "year_planted"),
                     all.x = TRUE, all.y = TRUE)

# Make sure in IDate format
inventories[, date_2023 := as.IDate(date_2023)]
inventories[, date_2024 := as.IDate(date_2024)]

# Add potential missing dates based on averages

mean(yday(inventories$date_2024), na.rm = TRUE)
inventories[is.na(date_2024), date_2024 := as.IDate("2024-10-13")]

mean(yday(inventories$date_2023), na.rm = TRUE)
inventories[is.na(date_2023), date_2023 := as.IDate("2023-10-21")]

# Remove trees that where replanted in 2021 - 2024 ; keep 2019; all of alive plot 84 TIAM are from 2019
# Keep 2020 planting (4/8 years of growth)
inventories <- inventories[ year_planted != 2021 &
                              year_planted != 2022 & year_planted != 2023 & year_planted != 2024,]

# Estimate AWP per tree
inventories$tree_AWP <- (inventories$volume_2024 - inventories$volume_2023) /
  ((inventories$date_2024 - inventories$date_2023)/365.25)

# Include the effect of mortality
inventories[!is.na(volume_2023) & is.na(volume_2024), tree_AWP := 0]

# ------------------------------------------------------------------------------
# Productivity per species - remove NA trees completely
species_inventories <- inventories[!is.na(tree_AWP), .(
  ntrees = .N,
  tree_AWP = sum(tree_AWP, na.rm = TRUE)
), by = c("plot_new", "species")]

species_inventories$proportion <- species_inventories$ntrees/64

# For plot-level calcs, export proportion
fwrite(species_inventories, "species.proportions.4.22.25.csv")

# ------------------------------------------------------------------------------
# Get plot basic information
summary_plot <- trees_2023[, .(sr = length(unique(species)),
                               mean_year_planted = mean(year_planted)),
                           by = c("plot_new")]

summary_plot <- summary_plot[mean_year_planted < 2021]

# ------------------------------------------------------------------------------
# Get monocultures
plots_mono <- summary_plot[sr == 1,]


mono_inventories <- merge(plots_mono, species_inventories, by = "plot_new",
                          all.x = TRUE, all.y = FALSE)

mono_species_AWP <- mono_inventories[, .(plot_AWP = mean(tree_AWP)),
                                     by = "species"]

# ------------------------------------------------------------------------------
# Merge mixtures and monocultures
# AWP = annual wood productivity (above-ground)

# Plots with mean year planted before 2020
species_inventories <- merge(species_inventories, summary_plot, by = "plot_new", all.x = FALSE, all.y = TRUE)
species_inventories <- merge(species_inventories, mono_species_AWP, by = "species")
colnames(species_inventories)[c(4, 8)] <- c("observed_AWP", "monocultures_AWP")

# ------------------------------------------------------------------------------
# Correct by proportion of species
species_inventories$expected_AWP <- species_inventories$monocultures_AWP*species_inventories$proportion
species_inventories <- species_inventories[order(plot_new, species)]

# Productivity calculations for ALL species
tiam <- filter(species_inventories, species == "Tilia americana")
acne <- filter(species_inventories, species == "Acer negundo")
acru <- filter(species_inventories, species == "Acer rubrum")
qual <- filter(species_inventories, species == "Quercus alba")
quru <- filter(species_inventories, species == "Quercus rubra")
quel <- filter(species_inventories, species == "Quercus ellipsoidalis")
quma <- filter(species_inventories, species == "Quercus macrocarpa")
bepa <- filter(species_inventories, species == "Betula papyrifera")
juvi <- filter(species_inventories, species == "Juniperus virginiana")
piba <- filter(species_inventories, species == "Pinus banksiana")
pire <- filter(species_inventories, species == "Pinus resinosa")
pist <- filter(species_inventories, species == "Pinus strobus")

# Annual wood production measures for each species

focal.productivity <- rbind(tiam, acne, acru, qual, quru, quel, quma, bepa, juvi, piba, pire, pist)
focal.productivity$difference_AWP <- focal.productivity$observed_AWP - focal.productivity$expected_AWP
fwrite(focal.productivity, paste0(root_path, "productivity.small.2024.4.22.25.csv"))
#-------------------------------------------------------------------------------
# Net biodiversity effect (NBE aka overyielding) calculations
BE <- data.table()
idplots <- unique(species_inventories$plot_new)

for(i in 1:length(idplots)) {
  
  # Select plot
  plot <- subset(species_inventories, plot_new == idplots[i])
  
  # Number of species
  Y <- plot$observed_AWP
  M <- plot$expected_AWP
  N <- nrow(plot)
  dRY <- (Y/M) - (1/N)
  covar <- sum((dRY-mean(dRY)) * (M-mean(M)))/N
  SE <- N*covar
  CE <- N*mean(dRY)*mean(M)
  NE <- SE + CE
  
  result <- data.table(plot_new = idplots[i], 
                       CE.sm = CE, 
                       SE.sm = SE, 
                       NE.sm = NE)
  
  BE <- rbind(BE, result)
  
}

# Return values in density
#m^3/year/hectare

BE$CE.sm <- BE$CE.sm/0.0081
BE$SE.sm <- BE$SE.sm/0.0081
BE$NE.sm <- BE$NE.sm/0.0081

#CE = complementarity effect
#SE = selection effect
#NE = net effect

fwrite(BE, paste0(root_path, "/small_plot_NBE_2024 (2025-4-22).csv"))
