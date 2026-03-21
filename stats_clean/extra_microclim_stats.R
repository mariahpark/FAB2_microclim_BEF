# Stats for physiology graph regressions

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
setwd("C:/Users/maria/Desktop/Research/2024/processed_df/")

dat <- read.csv("plot.dat.3.19.26.csv")

dat$TFC <- -sqrt(1-dat$FC)
#-------------------------------------------------------------------------------
# Sampling week microclimate stats

# 95th percentile

summary(lm(vpd_q95 ~ FC, data = dat))

summary(lm(rel_humidity_q95 ~ FC, data = dat))

summary(lm(temp_q95 ~ FC, data = dat))


# min
summary(lm(vpd_min ~ FC, data = dat))

summary(lm(rel_humidity_min ~ FC, data = dat)) # Sig

summary(lm(temp_min ~ FC, data = dat)) # sig


# mean

summary(lm(vpd_mean ~ FC, data = dat))

summary(lm(rel_humidity_mean ~ FC, data = dat))

summary(lm(temp_mean ~ FC, data = dat))


# amp

summary(lm(vpd_amp ~ FC, data = dat))

summary(lm(vpd_amp ~ TFC, data = dat))

summary(lm(rh_amp ~ FC, data = dat))

summary(lm(temp_amp ~ FC, data = dat))

summary(lm(vpd_amp ~ rh_amp + temp_amp, data = dat))



#-------------------------------------------------------------------------------
# Seasonally

# mean

summary(lm(vpd_mean_szn ~ FC, data = dat))

summary(lm(reh_mean_szn ~ FC, data = dat))

summary(lm(temp_mean_szn ~ FC, data = dat))

# sd

summary(lm(vpd_sd_szn ~ FC, data = dat))

summary(lm(reh_sd_szn ~ FC, data = dat))

summary(lm(temp_sd_szn ~ FC, data = dat))

# stability

summary(lm(vpd_stable_szn ~ FC, data = dat))

summary(lm(reh_stable_szn ~ FC, data = dat))

summary(lm(temp_stable_szn ~ FC, data = dat))



