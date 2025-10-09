# Structural Equation Model

rm(list=ls())
#libraries
library(ggplot2)
library(dplyr)
library(data.table)
library(conflicted)
library(ggpubr)
library(lavaan)
library(semPlot)
library(xfun)
library(car)
#library(tidySEM)

conflict_prefer("mutate", "dplyr")
conflict_prefer("summarise", "dplyr")
conflicts_prefer(dplyr::filter)

setwd("C:/Users/maria/Desktop/Research/2024/processed_df/")
plot.dat <- read.csv("plot.dat.10.9.25.csv")

#-------------------------------------------------------------------------------
# New variables
plot.dat$Pgap_sqrt <- sqrt(1-plot.dat$FC)
plot.dat$FC_sqrt <- -(plot.dat$Pgap_sqrt)

# Filter for plots with VPD measurements 
df_filtered <- plot.dat %>% filter(!is.na(vpd_amp))

# Polycultures only (for NBE correlations)
poly <- df_filtered %>%filter(SR.y != 1)

# Scaling
poly[, c("PSVs", "vpd_amp", "fges_prop", "NE.sm", "FC_sqrt")] <-
  scale(poly[, c("PSVs", "vpd_amp", "fges_prop", "NE.sm", "FC_sqrt")])

#-------------------------------------------------------------------------------
# SEM building

mod.new <- '
          #regressions
          
          FC_sqrt ~ PSVs + fges_prop
          vpd_amp ~ FC_sqrt + fges_prop
          NE.sm ~ vpd_amp + FC_sqrt + PSVs + fges_prop
          
'

#-------------------------------------------------------------------------------
# Run the SEM
sem <- sem(mod.new, data=poly) 
summary(mod.new, standardized = TRUE, fit.measures=TRUE, rsquare=TRUE)
standardizedSolution(sem, type="std.all")

# Check the fit
fit <- lavInspect(sem, "fit")
fit["chisq"]; fit["pvalue"]

# AIC: 170.9241
AIC(sem)


#-------------------------------------------------------------------------------
# Check for multicollinearity: Variance inflation factor (VIF)

lm_test <- lm(FC_sqrt ~ PSVs + fges_prop, data = poly)
car::vif(lm_test)

lm_test <- lm(vpd_amp ~ FC_sqrt + fges_prop, data = poly)
car::vif(lm_test)

lm_test <- lm(NE.sm ~ vpd_amp + FC_sqrt + PSVs + fges_prop, data = poly)
car::vif(lm_test)

lm_test <- lm(NE.sm ~ vpd_amp + PSVs + fges_prop, data = poly)
car::vif(lm_test)



# Correlation (standardized) units
inspect(sem, what="cor.all")

# Visualization
semPaths(sem, what='std', nCharNodes=6, sizeMan=10,
         edge.label.cex=1.25, curvePivot = TRUE, fade=FALSE)

# Another visualization
graph_sem(model=sem, what = "std")
