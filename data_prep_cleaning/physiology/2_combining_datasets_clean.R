# Combine dataframes

rm(list=ls())

library(dplyr)
library(plyr)
library(data.table)
library(tidyverse)

# Read in dataframes

setwd("C:/Users/maria/Desktop/Research/2024/processed_df/clean.data/")

licor.dat <- read.csv("2024_licor_R.csv") # LiCOR
fulcrum.dat <- read.csv("fab2_2024_sampling_R_clean.csv") # Physiology
microclim.dat <- read.csv("merged_sample_wk_amplitude_df_10.9.25.csv") # Microclimate amplitude
microclim.stats <- read.csv("merged_sample_wk_df_5_1_25_clean.csv") # Microclimate stats
nbe.dat <- read.csv("small_plot_NBE_2024 (2025-4-22).csv") # NBE
prop.dat <- read.csv("gymno.fges.calcs.4.22.25.csv") # Gymno and FAST proportions
lidar.dat <- read.csv("lidar.jul.2024.csv") # LiDAR
vwc.dat <- read.csv("vwc.july.2024.stats.5.9.25.csv") # VWC
spectra.dat <- read.csv("spectral_indices_plsr_jul_24_5_14_25.csv") # Spectra
lma.dat <- read.csv("lma.2024.R.csv") # LMA; slight manual cleaning so letter cases match UniqueID
light.dat <- read.csv("light.R.clean.csv") # Light
div.dat <- read.csv("combined.div.metrics.csv") # Diversity
prod.dat <- read.csv("productivity.small.2024.10.9.25.csv") # Species productivity

#-------------------------------------------------------------------------------
# Clean and trim data

# For all dataframes: delete first column (LTERYEAR)
# FUNCTION HERE
# Loop through all objects in the environment
for (obj_name in ls()) {
  obj <- get(obj_name)
  
  # Check if the object is a data frame and has the column 'LTERYEAR'
  if (is.data.frame(obj) && "LTERYEAR" %in% colnames(obj)) {
    obj$LTERYEAR <- NULL         # Remove the column
    assign(obj_name, obj)        # Reassign the modified data frame back to the environment
  }
}




# Rename plot column
colnames(nbe.dat)[1] <- "plot"
colnames(lidar.dat)[1] <- "plot"
colnames(prop.dat)[1] <- "plot"
colnames(prod.dat)[2] <- "plot"

# Filter one entry for tree
licor.dat.avg <- licor.dat %>% distinct(UniqueID, .keep_all = TRUE)

# Delete unnecessary columns
vwc.jul <- vwc.dat[, -c(1:4,6:8)]

# Delete 20m x 20m subset plots
div.dat$plot <- as.numeric(div.dat$plot)
div.dat <- div.dat[!is.na(div.dat$plot), ]

lidar.dat$plot <- as.numeric(lidar.dat$plot)
lidar.dat <- lidar.dat[!is.na(lidar.dat$plot), ]

# Species codes for productivity data
species_codes <- c("Tilia americana" = "TIAM",
                   "Quercus macrocarpa" = "QUMA",
                   "Quercus rubra" = "QURU",
                   "Acer negundo" = "ACNE",
                   "Quercus alba" = "QUAL",
                   "Quercus ellipsoidalis" = "QUEL",
                   "Quercus ellipsodalis" = "QUEL",
                   "Acer rubrum" = "ACRU",
                   "Betula papyrifera" = "BEPA",
                   "Juniperus virginiana" = "JUVI",
                   "Pinus resinosa" = "PIRE",
                   "Pinus banksiana" = "PIBA",
                   "Pinus strobus" = "PIST")

prod.dat$species_code.x <- prod.dat$species
prod.dat <- prod.dat %>%
  mutate(species_code.x = str_replace_all(species_code.x, species_codes))

#-------------------------------------------------------------------------------
# Merge dataframes

dat <- merge(fulcrum.dat, licor.dat.avg, by = "UniqueID2", all = TRUE)

# Rename plot.x to plot
colnames(dat)[4] <- "plot"

dat <- merge(dat, spectra.dat, by = "UniqueID2", all = TRUE)
dat <- merge(dat, lma.dat, by = "UniqueID2", all = TRUE)

# Give proper plot and species attribution to all measurements

# Only update rows where plot is NA
missing_plot_rows <- is.na(dat$plot)

# Extract plot (leading digits)
dat$plot[missing_plot_rows] <- as.numeric(sub("^([0-9]+).*", "\\1",
                                              dat$UniqueID2[missing_plot_rows]))

# Extract species_code (4 letters after digits)
dat$species_code.x[missing_plot_rows] <- sub("^[0-9]+([A-Z]{4}).*", "\\1",
                                             dat$UniqueID2[missing_plot_rows])

# MERGE PROD DAT
dat <- merge(dat, prod.dat, by = c("species_code.x", "plot"), all = TRUE)


# Continue with merging
dat <- merge(dat, microclim.dat, by = "plot", all = TRUE)
dat <- merge(dat, microclim.stats, by = "plot", all = TRUE)
dat <- merge(dat, nbe.dat, by = "plot", all = TRUE)
dat <- merge(dat, lidar.dat, by = "plot", all = TRUE)
dat <- merge(dat, vwc.jul, by = "plot", all = TRUE)
dat <- merge(dat, light.dat, by = "plot", all = TRUE)
dat <- merge(dat, prop.dat, by = "plot", all = TRUE)
dat <- merge(dat, div.dat, by = "plot", all =TRUE)

# Calculate LMA in terms of g/m^2
dat$total.leaf.area.m2 <- (dat$total.leaf.area.cm)/10000
dat$lma <- (dat$lma_dry_weight_g)/(dat$total.leaf.area.m2)

# Set species as factor
dat$species_code.x <- as.factor(dat$species_code.x)

# Export data
fwrite(dat, "C:/Users/maria/Desktop/Research/2024/processed_df/combo.dat.2024.10.13.25.csv")


