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
phys.only <- read.csv("phys.dat.5.26.25.csv")

#-------------------------------------------------------------------------------
# Change VWC range from decimal to %
phys.only$vwc_range <- phys.only$vwc_range*100

# Filter each species
tiam <- filter(phys.only, species.y == "Tilia americana")
acru <- filter(phys.only, species.y == "Acer rubrum")
qual <- filter(phys.only, species.y == "Quercus alba")
quru <- filter(phys.only, species.y == "Quercus rubra")

# Make a list of the species
species_list <- list(tiam = tiam, acru = acru, qual = qual, quru = quru)

#-------------------------------------------------------------------------------
# Function to extract stats from linear regression

# 1. [Not included in Fig. 5]

# VPD -> VWC? Not really

vpd.vwc <- function(df, formula = vpd_amp ~ vwc_range) {
  model <- lm(formula, data = df)
  tidy_df <- tidy(model) %>%
    select(term, estimate, std.error, statistic, p.value)
  
  r2 <- glance(model)$r.squared
  
  tidy_df %>%
    mutate(r.squared = r2)
}

vpd.vwc.df <- map_df(species_list, vpd.vwc, .id = "dataset")

# Lignin -> LMA

lig.lma <- function(df, formula = avg.lma ~ avg.lignin) {
  model <- lm(formula, data = df)
  tidy_df <- tidy(model) %>%
    select(term, estimate, std.error, statistic, p.value)
  
  r2 <- glance(model)$r.squared
  
  tidy_df %>%
    mutate(r.squared = r2)
}

lig.lma.df <- map_df(species_list, lig.lma, .id = "dataset")

# Lignin -> Carter Miller Stress Index

lig.cms <- function(df, formula = avg.cms ~ avg.lignin) {
  model <- lm(formula, data = df)
  tidy_df <- tidy(model) %>%
    select(term, estimate, std.error, statistic, p.value)
  
  r2 <- glance(model)$r.squared
  
  tidy_df %>%
    mutate(r.squared = r2)
}

lig.cms.df <- map_df(species_list, lig.cms, .id = "dataset")

#-------------------------------------------------------------------------------
# 2. VWC relationships

# VWC -> stomatal conductance

vwc.gsw <- function(df, formula = avg.gsw.plot ~ vwc_range) {
  model <- lm(formula, data = df)
  tidy_df <- tidy(model) %>%
    select(term, estimate, std.error, statistic, p.value)
  
  r2 <- glance(model)$r.squared
  
  tidy_df %>%
    mutate(r.squared = r2)
}

vwc.gsw.df <- map_df(species_list, vwc.gsw, .id = "dataset")

# VWC -> leaf water content

vwc.wc <- function(df, formula = avg.wc ~ vwc_range) {
  model <- lm(formula, data = df)
  tidy_df <- tidy(model) %>%
    select(term, estimate, std.error, statistic, p.value)
  
  r2 <- glance(model)$r.squared
  
  tidy_df %>%
    mutate(r.squared = r2)
}

vwc.wc.df <- map_df(species_list, vwc.wc, .id = "dataset")

# VWC -> LMA

vwc.lma <- function(df, formula = avg.lma ~ vwc_range) {
  model <- lm(formula, data = df)
  tidy_df <- tidy(model) %>%
    select(term, estimate, std.error, statistic, p.value)
  
  r2 <- glance(model)$r.squared
  
  tidy_df %>%
    mutate(r.squared = r2)
}

vwc.lma.df <- map_df(species_list, vwc.lma, .id = "dataset")

#-------------------------------------------------------------------------------
# 3. VPD relationships

# VPD -> stomatal conductance

vpd.gsw <- function(df, formula = avg.gsw.plot ~ vpd_amp) {
  model <- lm(formula, data = df)
  tidy_df <- tidy(model) %>%
    select(term, estimate, std.error, statistic, p.value)
  
  r2 <- glance(model)$r.squared
  
  tidy_df %>%
    mutate(r.squared = r2)
}

vpd.gsw.df <- map_df(species_list, vpd.gsw, .id = "dataset")

# VPD -> leaf water content

vpd.wc <- function(df, formula = avg.wc ~ vpd_amp) {
  model <- lm(formula, data = df)
  tidy_df <- tidy(model) %>%
    select(term, estimate, std.error, statistic, p.value)
  
  r2 <- glance(model)$r.squared
  
  tidy_df %>%
    mutate(r.squared = r2)
}

vpd.wc.df <- map_df(species_list, vpd.wc, .id = "dataset")

# VPD -> LMA

vpd.lma <- function(df, formula = avg.lma ~ vpd_amp) {
  model <- lm(formula, data = df)
  tidy_df <- tidy(model) %>%
    select(term, estimate, std.error, statistic, p.value)
  
  r2 <- glance(model)$r.squared
  
  tidy_df %>%
    mutate(r.squared = r2)
}

vpd.lma.df <- map_df(species_list, vpd.lma, .id = "dataset")

#-------------------------------------------------------------------------------
# 4. Physiology trait associations

# Leaf water content -> Carter Miller Stress Index
wc.cms <- function(df, formula = avg.cms ~ avg.wc) {
  model <- lm(formula, data = df)
  tidy_df <- tidy(model) %>%
    select(term, estimate, std.error, statistic, p.value)
  
  r2 <- glance(model)$r.squared
  
  tidy_df %>%
    mutate(r.squared = r2)
}

wc.cms.df <- map_df(species_list, wc.cms, .id = "dataset")

# Stomatal conductance -> Photosynthesis
gsw.c <- function(df, formula = avg.a.plot ~ avg.gsw.plot) {
  model <- lm(formula, data = df)
  tidy_df <- tidy(model) %>%
    select(term, estimate, std.error, statistic, p.value)
  
  r2 <- glance(model)$r.squared
  
  tidy_df %>%
    mutate(r.squared = r2)
}

gsw.c.df <- map_df(species_list, gsw.c, .id = "dataset")

#-------------------------------------------------------------------------------
# Add header column
gsw.c.df$test <- "gsw.c.df"
lig.cms.df$test <- "lig.cms"
lig.lma.df$test <- "lig.lma"
vpd.gsw.df$test <- "vpd.gsw"
vpd.lma.df$test <- "vpd.lma"
vpd.vwc.df$test <- "vpd.vwc"
vpd.wc.df$test <- "vpd.wc"
vwc.gsw.df$test <- "vwc.gsw"
vwc.lma.df$test <- "vwc.lma"
vwc.wc.df$test <- "vwc.wc"
wc.cms.df$test <- "wc.cms"

# Bind and export linear regression results

results.list <- rbind(gsw.c.df, lig.cms.df, lig.lma.df, vpd.gsw.df, vpd.lma.df, vpd.vwc.df,
                vpd.wc.df, vwc.gsw.df, vwc.lma.df, vwc.wc.df, wc.cms.df)


fwrite(results.list, "phys.stats.5.26.25.csv")
