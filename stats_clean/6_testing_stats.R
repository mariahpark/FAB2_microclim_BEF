# Testing validity of statistics

rm(list=ls())
#libraries
library(ggplot2)
library(dplyr)
library(data.table)
library(ggpmisc)
library(tidyverse)
library(broom)
library(conflicted)
library(ggpubr)

conflict_prefer("mutate", "dplyr")
conflict_prefer("summarise", "dplyr")
conflicts_prefer(dplyr::filter)

# Read data
setwd("C:/Users/maria/Desktop/Research/2024/psr/predicted_traits")
spectra <- read.csv("USE_fab_jul_2024_traits_PLSR.csv")

setwd("C:/Users/maria/Desktop/Research/2024/processed_df/")
plot.dat <- read.csv("plot.dat.10.13.25.csv")

phys.dat <- read.csv("phys.dat.10.13.25.csv")

poly <- plot.dat %>% filter(SR.y != 1)
#-------------------------------------------------------------------------------
# Calculate mean sd of lignin and wc

# 1.579637
mean(spectra$lignin.sd)

# 2.942566
mean(spectra$WC.sd)

#-------------------------------------------------------------------------------
# TFC calculation
plot.dat$TFC <- -sqrt(1-plot.dat$FC)



# FC and VPD
summary(lm(vpd_q95 ~ FC, data = plot.dat))

summary(lm(vpd_mean ~ FC, data = plot.dat))

# FC and temp, humidity
summary(lm(rh_amp ~ FC, data = plot.dat))

summary(lm(temp_amp ~ FC, data = plot.dat))

# humidity, vpd
summary(lm(vpd_amp ~ rh_amp + temp_amp, data = plot.dat))

# FC / TFC and VPDamp

summary(lm(vpd_amp ~ FC, data = plot.dat))

summary(lm(vpd_amp ~ TFC, data = plot.dat))

# TFC FAST
summary(lm(vpd_amp ~ TFC + fges_prop, data = plot.dat))

# FC and light
summary(lm(light_ratio_mid_to_open ~ FC, data = plot.dat))

# VPD FC light
summary(lm(light_ratio_mid_to_open ~ FC + vpd_amp, data = plot.dat))

# VPDamp PAR
summary(lm(light_ratio_mid_to_open ~ vpd_amp, data = plot.dat))


# VPDamp VPD range
# Double check that vpd_range was calculated using the 95th percentile
plot.dat$vpd_range_95 = plot.dat$vpd_q95 - plot.dat$vpd_min
plot.dat$vpd_range_95
plot.dat$vpd_range

summary(lm(vpd_amp ~ vpd_range, data = plot.dat))
#-------------------------------------------------------------------------------
# AWP

summary(lm(NE.sm ~ vpd_amp, data = poly))

#-------------------------------------------------------------------------------
# Justify use of PSV
summary(lm(FC ~ SR.y, data = plot.dat))
summary(lm(FC ~ FD_PSV, data = plot.dat))
summary(lm(FC ~ FD_faith.no.root_PD, data = plot.dat))
summary(lm(FC ~ faith.no.root.PD, data = plot.dat))
summary(lm(FC ~ PSVs, data = plot.dat))


mod <- lm(FC ~ SR.y, data = plot.dat)
AIC(mod)

mod <- lm(FC ~ FD_PSV, data = plot.dat)
AIC(mod)

mod <- lm(FC ~ FD_faith.no.root_PD, data = plot.dat)
AIC(mod)

mod <- lm(FC ~ faith.no.root.PD, data = plot.dat)
AIC(mod)

mod <- lm(FC ~ PSVs, data = plot.dat)
AIC(mod)

#-------------------------------------------------------------------------------
# Physiology: connecting photosynthesis ~ AWP
phys.dat$species.y <- as.factor(phys.dat$species.y)

mod <- lm(difference_AWP ~ Avg_A*species.y, data = phys.dat)
summary(mod)

# LMA ~ AWP
mod <- lm(difference_AWP ~ avg.lma*species.y, data = phys.dat)
summary(mod)


#-------------------------------------------------------------------------------
# Height and FC

mod <- lm(FC ~ height, data = plot.dat)
summary(mod)

mod <- lm(TFC ~ height, data = plot.dat)
summary(mod)

mod <- lm(vpd_amp ~ height, data = plot.dat)
summary(mod)

mod <- lm(vpd_amp ~ TFC, data = plot.dat)
summary(mod)

mod <- lm(light_ratio_mid_to_open ~ height, data = plot.dat)
summary(mod)

mod <- lm(light_ratio_mid_to_open ~ FC, data = plot.dat)
summary(mod)


