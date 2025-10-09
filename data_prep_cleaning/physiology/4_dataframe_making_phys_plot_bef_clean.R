# Final dataframe cleaning and exporting

rm(list=ls())
#libraries
library(ggplot2)
library(dplyr)
library(data.table)
library(conflicted)
conflict_prefer("mutate", "dplyr")
conflict_prefer("summarise", "dplyr")
conflicts_prefer(dplyr::filter)

setwd("C:/Users/maria/Desktop/Research/2024/processed_df/")
dat <- read.csv("combo.dat.2024.all.plots.trait.means.10.9.25.csv")


# Find outlier for vpd_amp --plot 97: had fallen on ground
which.max(dat$vpd_amp)

# Make microclimate data NA
dat[570, c(90:119)] <- NA

# Make sure FC is <= 1 (errors if slightly above 1)
dat$FC <- ifelse(dat$FC > 1, 1, dat$FC)

# Remove control plots (not used in this analysis)
dat <- dat[!is.na(dat$faith.no.root.PD), ]

# Physiology dataframe
species.dat <- dat %>% distinct(plot, species_code.x, .keep_all = TRUE)
phys.dat <- species.dat[!(is.na(species.dat$UniqueID2) |
                                 species.dat$UniqueID2 == ""), ]

# Plot dataframe
plot.dat <- dat %>% distinct(plot, .keep_all = TRUE)


##export data
fwrite(plot.dat, "C:/Users/maria/Desktop/Research/2024/processed_df/plot.dat.10.9.25.csv")
fwrite(phys.dat, "C:/Users/maria/Desktop/Research/2024/processed_df/phys.dat.10.9.25.csv")
fwrite(species.dat, "C:/Users/maria/Desktop/Research/2024/processed_df/species.dat.10.9.25.csv")

