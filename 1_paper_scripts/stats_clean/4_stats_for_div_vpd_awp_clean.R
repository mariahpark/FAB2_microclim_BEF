# Get results of linear regressions between species AWP and diversity
## AWP and VPDamp (pre-infill)

rm(list=ls())
#libraries
library(ggplot2)
library(dplyr)
library(data.table)
library(purrr)
library(ggpp)
library(ggpmisc)
library(tidyverse)
library(broom)
library(conflicted)

conflict_prefer("mutate", "dplyr")
conflict_prefer("summarise", "dplyr")
conflicts_prefer(dplyr::filter)

# Read data

setwd("C:/Users/maria/Desktop/Research/2024/processed_df/")
combo.dat <- read.csv("species.dat.5.26.25.csv")

#-------------------------------------------------------------------------------
# Make sure each species has plot-level metrics associated

filled.dat <- combo.dat %>%
  group_by(plot) %>%
  fill(SR.y, .direction = "downup") %>%
  fill(PSVs, .direction = "downup") %>%
  fill(faith.no.root.PD, .direction = "downup") %>%
  fill(FD_faith.no.root_PD, .direction = "downup") %>%
  fill(FD_PSV, .direction = "downup") %>%
  fill(fges_prop, .direction = "downup") %>%
  fill(FC, .direction = "downup") %>%
  fill(vpd_amp, .direction = "downup")


# IF DEALING WITH DIFFERENCE_AWP: monocultures = 0
filled.dat <- filled.dat %>%
  mutate(difference_AWP = ifelse(SR.y == 1, 0, difference_AWP))

# Filter each species
tiam <- filter(filled.dat, species.y == "Tilia americana")
acne <- filter(filled.dat, species.y == "Acer negundo")
acru <- filter(filled.dat, species.y == "Acer rubrum")
qual <- filter(filled.dat, species.y == "Quercus alba")
quru <- filter(filled.dat, species.y == "Quercus rubra")
quel <- filter(filled.dat, species.y == "Quercus ellipsoidalis")
quma <- filter(filled.dat, species.y == "Quercus macrocarpa")
bepa <- filter(filled.dat, species.y == "Betula papyrifera")
juvi <- filter(filled.dat, species.y == "Juniperus virginiana")
piba <- filter(filled.dat, species.y == "Pinus banksiana")
pire <- filter(filled.dat, species.y == "Pinus resinosa")
pist <- filter(filled.dat, species.y == "Pinus strobus")

# Make a list of the species
species_list <- list(tiam = tiam, acne = acne, acru = acru, qual = qual, quru = quru,
                     quel = quel, quma = quma, bepa = bepa, juvi = juvi, piba = piba,
                     pire = pire, pist = pist)

#-------------------------------------------------------------------------------
# Function to extract stats from linear regression
# Diversity metrics: PSVs, SR.y, faith.no.root.PD, FD_faith.no.root_PD, FD_PSV
# VPD (only observed, pre-infilled data)

# Phylogenetic species variability
psv_lm_stats <- function(df, formula = difference_AWP ~  PSVs) {
  model <- lm(formula, data = df)
  tidy(model) %>%
    select(term, estimate, std.error, statistic, p.value)
}

psv.results <- map_df(species_list, psv_lm_stats, .id = "dataset")  

# Species richness
sr_lm_stats <- function(df, formula = difference_AWP ~  SR.y) {
  model <- lm(formula, data = df)
  tidy(model) %>%
    select(term, estimate, std.error, statistic, p.value)
}

sr.results <- map_df(species_list, sr_lm_stats, .id = "dataset")  

# Phylogenetic diversity
pd_lm_stats <- function(df, formula = difference_AWP ~  faith.no.root.PD) {
  model <- lm(formula, data = df)
  tidy(model) %>%
    select(term, estimate, std.error, statistic, p.value)
}

pd.results <- map_df(species_list, pd_lm_stats, .id = "dataset")  

# Functional diversity
fd_lm_stats <- function(df, formula = difference_AWP ~  FD_faith.no.root_PD) {
  model <- lm(formula, data = df)
  tidy(model) %>%
    select(term, estimate, std.error, statistic, p.value)
}

fd.results <- map_df(species_list, fd_lm_stats, .id = "dataset")  

# Functional species variability
fsv_lm_stats <- function(df, formula = difference_AWP ~  FD_PSV) {
  model <- lm(formula, data = df)
  tidy(model) %>%
    select(term, estimate, std.error, statistic, p.value)
}

fsv.results <- map_df(species_list, fsv_lm_stats, .id = "dataset")  

# Vapor pressure deficit amplitude
vpd_lm_stats <- function(df, formula = difference_AWP ~  vpd_amp) {
  model <- lm(formula, data = df)
  tidy(model) %>%
    select(term, estimate, std.error, statistic, p.value)
}

vpd.results <- map_df(species_list, vpd_lm_stats, .id = "dataset")


# Add header column
psv.results$test <- "PSV"
sr.results$test <- "SR"
pd.results$test <- "PD"
fd.results$test <- "FD"
fsv.results$test <- "FSV"
vpd.results$test <- "VPD_pre_infill"

# Bind and export linear regression results
results.list <- rbind(psv.results, sr.results, pd.results, fd.results,
                      fsv.results, vpd.results)

fwrite(results.list, "diversity.vpd.awp.stats.5.26.25.csv")
