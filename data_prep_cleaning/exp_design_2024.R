# Check experimental design with stats and visualizations

rm(list=ls())
library(ggplot2)
library(dplyr)
library(grid)
library(gridExtra)
library(ggiraphExtra)
library(graphics)
library(pwr)

# Power tests
# For microclimate: should be able to detect ~medium effect sizes based on Cohen's D
# 80% pwoer at 74 samples
# https://www.statmethods.net/stats/power.html
# https://environmentalcomputing.net/statistics/power-analysis/

pwr.r.test(n=74, r=0.3, sig.level=0.05, power=NULL )
pwr.r.test(n=74, r=0.32, sig.level=0.05, power=NULL )
pwr.r.test(n=74, r=0.5, sig.level=0.05, power=NULL )
pwr.r.test(n=74, r=NULL, sig.level=0.05, power=.8 )

# For soil moisture
pwr.r.test(n=23, r=NULL, sig.level=0.05, power=.8)


setwd("c:/Users/maria/Desktop/Research/2024/")
dat <- read.csv("Experimental_Design_2024_div3.csv")

# Make numeric
dat$FD_PSV <- as.numeric(dat$FD_PSV)
dat$FD_faith.no.root <- as.numeric(dat$FD_faith.no.root)
dat$PSV <- as.numeric(dat$PSV)
dat$PD_faith.no.root <- as.numeric(dat$PD_faith.no.root)


# Filter
dat2 <- filter(dat, dat$soil_moisture_2024 == "1")

p1 <- hist(dat2$Species.Richness, breaks=10)
p2 <- hist(dat2$FD_PSV, breaks=10)
p3 <- hist(dat2$FD_faith.no.root, breaks=10)
p4 <- hist(dat2$PSV, breaks=10)
p5 <- hist(dat2$PD_faith.no.root, breaks=10)
p6 <- hist(dat2$Gymno_Angio_Ratio, breaks=10)

# Plot distributions for different diversity metrics
par(mfrow = c(2,3))
hist(dat2$Species.Richness, breaks=10)
hist(dat2$FD_PSV, breaks=10)
hist(dat2$FD_faith.no.root, breaks=10)
hist(dat2$PSV, breaks=10)
hist(dat2$PD_faith.no.root, breaks=10)
hist(dat2$Gymno_Angio_Ratio, breaks=10)

# Graph to see distribution of plots between diversity metrics
ggplot(dat2,aes(x=PD_faith.no.root,y=FD_faith.no.root))+
  geom_point(size=2)+
  geom_jitter(width = 0.01, height = 0.01)+
  geom_smooth(method=lm)
