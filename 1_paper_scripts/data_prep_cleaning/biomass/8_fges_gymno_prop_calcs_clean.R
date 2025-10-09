
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
# Work on small plots (original 10m x 10m plantings)
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

# Rename plot
frame_small$plot_new <- frame_small$plot

# Just small plots
trees <- frame_small

#-------------------------------------------------------------------------------
# Get 2024 volume data
# Reshape 2024
trees_2024 <- subset(trees, year(measurement_date) == 2024)
trees_2024 <- trees_2024[, c("plot", "plot_new", "individual_id", 
                             "species", "year_planted",
                             "deadmissing", "measurement_date", "volume")]

colnames(trees_2024)[6:8] <- c("deadmissing_2024", "date_2024", "volume_2024")


# Make sure in IDate format
trees_2024[, date_2024 := as.IDate(date_2024)]

# Add potential missing dates based on averages
mean(yday(trees_2024$date_2024), na.rm = TRUE)
trees_2024[is.na(date_2024), date_2024 := as.IDate("2024-10-13")]


# Remove trees that where replanted in 2021 - 2024 ; keep 2019; all of alive plot 84 TIAM are from 2019
# Keep 2020 planting
trees_2024 <- trees_2024[ year_planted != 2021 &
                            year_planted != 2022 & year_planted != 2023 & year_planted != 2024,]

# Wood volume per species - remove NA trees completely
species_inventories <- trees_2024[!is.na(volume_2024), .(
  ntrees = .N,
  tree_vol = sum(volume_2024, na.rm = TRUE)
), by = c("plot_new", "species")]

# Column for total plot volume
plot.vol <- species_inventories[, .(plot_vol = sum(tree_vol)), by = "plot_new"]

species_inventories <- merge(species_inventories, plot.vol, by = "plot_new")

# ------------------------------------------------------------------------------
#categorize: gymnosperms, FGES

# Gymnosperm / Angiosperm
gymno_species <- function(df, species_col) {
  df$Clade <- ifelse(df[[species_col]] %in% c("Tilia americana", "Quercus macrocarpa", "Quercus rubra",
                                              "Acer negundo", "Quercus alba", "Quercus ellipsoidalis",
                                              "Acer rubrum", "Betula papyrifera"), "Angiosperm",
                     ifelse(df[[species_col]] %in% c("Juniperus virginiana", "Pinus resinosa", "Pinus banksiana",
                                                     "Pinus strobus"), "Gymnosperm", NA))
  return(df)
}

# FGES / non_FGES
fges_species <- function(df, species_col) {
  df$FGES <- ifelse(df[[species_col]] %in% c("Tilia americana", "Quercus macrocarpa", "Quercus rubra",
                                             "Acer negundo", "Quercus alba", "Quercus ellipsoidalis",
                                             "Acer rubrum", "Juniperus virginiana"), "non_FGES",
                    ifelse(df[[species_col]] %in% c("Pinus resinosa", "Pinus banksiana",
                                                    "Pinus strobus","Betula papyrifera"), "FGES", NA))
  return(df)
}

species.dat <- gymno_species(species_inventories, "species")
species.dat <- fges_species(species.dat, "species")

#-------------------------------------------------------------------------------
#calculate proportions by volume

gymno.vol <- species.dat[, .(
  gymno_prop = sum(tree_vol)), by = c("plot_new")]

# gymno proportion: gymno plot vol / total plot vol
gymno_prop <- species.dat[, .(
  gymno_prop = sum(tree_vol)/plot_vol
), by = c("plot_new", "Clade")]

#fges proportion: fges plot vol / total plot vol
fges_prop <- species.dat[, .(
  fges_prop = sum(tree_vol)/plot_vol
), by = c("plot_new", "FGES")]

#-------------------------------------------------------------------------------
# Create gymnosperm and FGES row for each plot
# Find plots that do NOT have an "FGES" row
plots_missing_fges_yes <- fges_prop[FGES == "FGES", unique(plot_new)]
all_plots <- unique(fges_prop$plot_new)
plots_to_add <- setdiff(all_plots, plots_missing_fges_yes)

# Create new rows for those plots
new_rows <- data.table(plot_new = plots_to_add, FGES = "FGES")

fges_infill <- rbind(fges_prop, new_rows, fill = TRUE)

# Set fges_prop to 0 if equals NA
fges_infill$fges_prop[is.na(fges_infill$fges_prop)] <- 0

# Do same for gymnosperms
plots_missing_gymno_yes <- gymno_prop[Clade == "Gymnosperm", unique(plot_new)]
all_plots <- unique(gymno_prop$plot_new)
plots_to_add <- setdiff(all_plots, plots_missing_gymno_yes)

# Create new rows for those plots
new_rows <- data.table(plot_new = plots_to_add, Clade = "Gymnosperm")

gymno_infill <- rbind(gymno_prop, new_rows, fill = TRUE)

# Set fges_prop to 0 if equals NA
gymno_infill$gymno_prop[is.na(gymno_infill$gymno_prop)] <- 0

#-------------------------------------------------------------------------------
# Have only one FGES proportion and gymnosperm proportion entry per plot

# Gynmnosperm filtering
gymno_filter <- gymno_infill[gymno_infill$Clade != "Angiosperm", ]

df_gymnosperm <- gymno_filter[!duplicated(gymno_filter$plot_new), ]

# FGES filtering
fges_filter <- fges_infill[fges_infill$FGES != "non_FGES", ]

df_fges <- fges_filter[!duplicated(fges_filter$plot_new), ]

#-------------------------------------------------------------------------------
# Remerge with master dataframe
species.dat <- merge(species.dat, df_gymnosperm, by = "plot_new")
species.dat <- merge(species.dat, df_fges, by = "plot_new")

plot.dat <- species.dat[!duplicated(species.dat$plot_new), ]
plot.dat <- select(plot.dat, plot_new, gymno_prop, fges_prop, plot_vol)

# Export dataframe
fwrite(plot.dat, "gymno.fges.calcs.4.22.25.csv")
