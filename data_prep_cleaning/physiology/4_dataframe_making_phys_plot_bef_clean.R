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
dat <- read.csv("combo.dat.2024.all.plots.trait.means.3.19.26.csv")


# Find outlier for vpd_amp --plot 97: had fallen on ground
which.max(dat$vpd_amp)

# Make microclimate data NA
dat[!is.na(dat$plot) & dat$plot == 97, c(90:119, 151:171)] <- NA

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

# Plot x month dataframes
# Unique ID
#dat$ID <- paste0(dat$plot, dat$month.x)

#month.dat <- dat %>% distinct(ID, .keep_all = TRUE)

# Realized Plot 128 has outlier relative humidity data
# Remove outliers
plot.dat[plot.dat$plot == 128, c(90:119,151:171)] <- NA
#month.dat[month.dat$plot == 128, c(90:119,151:171)] <- NA
species.dat[species.dat$plot == 128, c(90:119,151:171)] <- NA
phys.dat[phys.dat$plot == 128, c(90:119,151:171)] <- NA

##export data
fwrite(plot.dat, "C:/Users/maria/Desktop/Research/2024/processed_df/plot.dat.3.19.26.csv")
fwrite(phys.dat, "C:/Users/maria/Desktop/Research/2024/processed_df/phys.dat.3.19.26.csv")
fwrite(species.dat, "C:/Users/maria/Desktop/Research/2024/processed_df/species.dat.3.19.26.csv")
#fwrite(month.dat, "C:/Users/maria/Desktop/Research/2024/processed_df/month.dat.3.19.26.csv")

