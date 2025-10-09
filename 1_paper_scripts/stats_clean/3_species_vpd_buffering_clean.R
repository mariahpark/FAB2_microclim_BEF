# Which species are contributing to VPD buffering?
rm(list=ls())

#libraries
library(dplyr)
library(data.table)
library(scales)
library(ggpp)
library(ggpmisc)
library(tidyverse)
library(tidyr)
library(xts)
library(broom)
library(ggpmisc)
library(lmtest)
library(conflicted)

conflict_prefer("mutate", "dplyr")
conflict_prefer("summarise", "dplyr")
conflicts_prefer(dplyr::filter)

setwd("C:/Users/maria/Desktop/Research/2024/processed_df/")
plot.dat <- read.csv("plot.dat.5.26.25.csv")

#-------------------------------------------------------------------------------
# Filter monocultures
mono <- filter(plot.dat, SR.y == 1)

# FGES categories (*in paper, FGES = FAST)
# Creating  new column (FGES) based on species_code
mono <- mono %>%
  mutate(fges = ifelse(species_code.x %in%
                         c("BEPA", "PIBA", "PIRE", "PIST"), "FGES",
                       ifelse(species_code.x %in%
                                c("JUVI", "ACRU", "ACNE",
                                  "TIAM", "QUAL", "QURU",
                                  "QUEL", "QUMA"), "Non-FGES", NA)))

# Species influence VPDamp differently
mod <- aov(vpd_amp ~ species_code.x, data = mono)
summary(mod)
TukeyHSD(mod)

# FGES reduce VPDamp a lot in monocultures
mod <- aov(vpd_amp ~ fges, data = mono)
summary(mod)
TukeyHSD(mod)

# FGES proportion -> VPDamp
mod <- lm(vpd_amp ~ fges_prop, data = plot.dat)
summary(mod)

# Contribution of RH vs temp to VPD
mod <- lm(vpd_amp ~ rh_amp + temp_amp, data = plot.dat)
summary(mod)

mod <- lm(vpd_amp ~ rh_amp, data = plot.dat)
summary(mod)

# VPDamp ~ VWC_amp? not super correlated
mod <- lm(vwc_range ~ vpd_amp, data = plot.dat)
summary(mod)

# Checking VPDamp ~ VPDrange relationship
mod <- lm(vpd_amp ~ vpd_range, data = plot.dat)
summary(mod)

# FGES and diversity increase CE and NBE, but don't significantly affect SE
mod <- lm(CE.sm ~ fges_prop + PSVs, data = plot.dat)
summary(mod)

mod <- lm(SE.sm ~ fges_prop + PSVs, data = plot.dat)
summary(mod)

mod <- lm(NE.sm ~ fges_prop + PSVs, data = plot.dat)
summary(mod)
