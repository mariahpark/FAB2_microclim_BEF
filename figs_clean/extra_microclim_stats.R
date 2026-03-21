# Supplementary Table - Microclimate stats in relation to FC and FAST

rm(list=ls())
#libraries
library(ggplot2)
library(dplyr)
library(scales)
library(ggpp)
library(ggpmisc)
library(tidyverse)
library(conflicted)
library(ggpubr)
library(viridis)
library(broom)

# Read data
setwd("C:/Users/maria/Desktop/Research/2024/processed_df/")
plot.dat <- read.csv("plot.dat.3.19.26.csv")

#-------------------------------------------------------------------------------
# Condense file

df_new <- plot.dat %>%
  select(plot, SR.x, PSVs, FC, fges_prop,
         vpd_amp, temp_amp, rh_amp,
         vpd_mean, vpd_min, vpd_q95,
         temp_mean, temp_min, temp_q95,
         rel_humidity_mean, rel_humidity_min, rel_humidity_q95,
         vpd_mean_szn, vpd_sd_szn, vpd_stable_szn,
         temp_mean_szn, temp_sd_szn, temp_stable_szn,
         reh_mean_szn, reh_sd_szn, reh_stable_szn
         )

fwrite(df_new, "microclim.dat.3.19.26.csv")
