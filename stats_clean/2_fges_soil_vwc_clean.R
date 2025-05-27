## Testing relationships of variables with VWC

rm(list=ls())
#libraries
library(dplyr)

setwd("C:/Users/maria/Desktop/Research/2024/processed_df/")
plot.dat <- read.csv("plot.dat.5.26.25.csv")

#-------------------------------------------------------------------------------
# VWC stats testing
# Gymnosperm proportion
mod <- lm(vwc_max ~ gymno_prop, data = plot.dat)
summary(mod)

mod <- lm(vwc_mean ~ gymno_prop, data = plot.dat)
summary(mod)

mod <- lm(vwc_med ~ gymno_prop, data = plot.dat)
summary(mod)

mod <- lm(vwc_min ~ gymno_prop, data = plot.dat)
summary(mod)

mod <- lm(vwc_range ~ gymno_prop, data = plot.dat)
summary(mod)

# FGES proportion
mod <- lm(vwc_max ~ fges_prop, data = plot.dat)
summary(mod)

mod <- lm(vwc_mean ~ fges_prop, data = plot.dat)
summary(mod)

mod <- lm(vwc_med ~ fges_prop, data = plot.dat)
summary(mod)

mod <- lm(vwc_min ~ fges_prop, data = plot.dat)
summary(mod)

mod <- lm(vwc_range ~ fges_prop, data = plot.dat)
summary(mod)

# Total plot volume (biomass)
mod <- lm(vwc_max ~ plot_vol, data = plot.dat)
summary(mod)

mod <- lm(vwc_mean ~ plot_vol, data = plot.dat)
summary(mod)

mod <- lm(vwc_med ~ plot_vol, data = plot.dat)
summary(mod)

mod <- lm(vwc_min ~ plot_vol, data = plot.dat)
summary(mod)

mod <- lm(vwc_range ~ plot_vol, data = plot.dat)
summary(mod)

#tests of other environmental and diversity variables

# VPD not really related
mod <- lm(vpd_amp ~ vwc_range, data = plot.dat)
summary(mod)

# Light is slightly related
mod <- lm(light_ratio_mid_to_open ~ vwc_range, data = plot.dat)
summary(mod)

# VWC with diversity?
mod <- lm(vwc_range ~ PSVs, data = plot.dat)
summary(mod)
