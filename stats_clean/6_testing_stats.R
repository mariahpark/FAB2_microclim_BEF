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

#-------------------------------------------------------------------------------
# Calculate mean sd of lignin and wc

# 1.579637
mean(spectra$lignin.sd)

# 2.942566
mean(spectra$WC.sd)



