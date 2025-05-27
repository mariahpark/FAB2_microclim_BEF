# Calculate spectral indices

rm(list=ls())
library(devtools)
library(spectrolab)
library(dplyr)

setwd("c:/Users/maria/Desktop/Research/2024/psr/Spectra/All/M2")
Mspectra <- read.csv("Jul_2024_spectra_cut_no_smooth.csv")


# Make a dataframe with ID to save index in
indices.dat<- Mspectra[1:3]

#Carter Miller Stress CMS - Photosynthetic activity
#R694/R760
indices.dat$cms <- (Mspectra$X694/Mspectra$X760)

write.csv(indices.dat, "jul_2024_cmsi_5_10_25.csv")
