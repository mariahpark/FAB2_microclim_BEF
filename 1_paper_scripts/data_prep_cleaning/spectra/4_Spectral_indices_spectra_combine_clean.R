# Merge spectral index and trait data

rm(list=ls())
library(devtools)
library(spectrolab)
library(dplyr)

setwd("c:/Users/maria/Desktop/Research/2024/psr/Spectra/All/M2")
spec.dat <- read.csv("jul_2024_cmsi_5_10_25.csv")

setwd("c:/Users/maria/Desktop/Research/2024/psr")
meta.dat <- read.csv("spectra_metadata.csv")

setwd("c:/Users/maria/Desktop/Research/2024/psr/predicted_traits")
plsr.dat <- read.csv("fab_jul_2024_traits_PLSR_5_10_25.csv")

#-------------------------------------------------------------------------------

# merge PLSR trait data with metadata
plsr.dat <- merge(meta.dat, plsr.dat, by = "ID")

# delete unneeded columns
spec.dat <- spec.dat[, -c(1,2,4)]
plsr.dat <- plsr.dat[, -c(1,3,4,5,6,7,8)]

# average spectral index and trait values based on tree id
avg.spec <- aggregate(spec.dat[, 2], list(spec.dat$UniqueID2), mean)
avg.plsr <- aggregate(plsr.dat[, 2:5], list(plsr.dat$UniqueID2), mean)

# rename column
colnames(avg.spec)[2] <- "cms.mean"

dat <- merge(avg.spec, avg.plsr, by = "Group.1")

# rename first col
colnames(dat)[1] <- "UniqueID2"

# export dataframe
write.csv(dat, "spectral_indices_plsr_jul_24_5_14_25.csv")

