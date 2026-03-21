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
plot.dat <- read.csv("plot.dat.3.19.26.csv")

setwd("C:/Users/maria/Desktop/Research/2024/processed_df/clean.data/")
# LiDAR July
#lidar <- read.csv("lidar.jul.2024.h.csv")
#lidar<- rename(lidar, FC_jul = FC)
#lidar <- lidar[, c(2,9)]

#plot.dat <- read.csv("month.dat.3.12.26.csv")

# Just July
#plot.dat <- plot.dat %>% filter(month.x == 7)
# Merge
#plot.dat <- merge(plot.dat, lidar, by = "plot", .keep_all = TRUE)

#-------------------------------------------------------------------------------

# Make sure FC is <= 1 (errors if slightly above 1)
#plot.dat$FC_jul <- ifelse(plot.dat$FC_jul > 1, 1, plot.dat$FC_jul)

# New variables
plot.dat$Pgap_sqrt <- sqrt(1-plot.dat$FC)
plot.dat$FC_sqrt <- -(plot.dat$Pgap_sqrt)

# Filter for plots with VPD measurements 
df_filtered <- plot.dat %>% filter(!is.na(vpd_amp))

# Polycultures only (for NBE correlations)
poly <- df_filtered %>%filter(SR.x != 1)

#-------------------------------------------------------------------------------
# Scaling
poly[, c("PSVs", "vpd_amp", "fges_prop", "NE.sm", "FC_sqrt", "vpd_sd_szn")] <-
  scale(poly[, c("PSVs", "vpd_amp", "fges_prop", "NE.sm", "FC_sqrt", "vpd_sd_szn")])



#-------------------------------------------------------------------------------
# SEM building
# Try vpdamp

mod.new <- '
          #regressions
          
          FC_sqrt ~ PSVs + fges_prop
          vpd_amp ~ FC_sqrt + fges_prop
          NE.sm ~ vpd_amp + FC_sqrt + PSVs + fges_prop
          
'

#-------------------------------------------------------------------------------
# SEM the other way

mod.vpd <- '
          #regressions
          
          FC_sqrt ~ PSVs + fges_prop
          NE.sm ~ FC_sqrt + fges_prop
          vpd_amp ~ NE.sm + FC_sqrt + PSVs + fges_prop
          
'

#-------------------------------------------------------------------------------
# SEM building
# Try vpd sd instead of amp

mod.new <- '
          #regressions
          
          FC_sqrt ~ PSVs + fges_prop
          vpd_sd_szn ~ FC_sqrt + fges_prop
          NE.sm ~ vpd_sd_szn + FC_sqrt + PSVs + fges_prop
          
'

#-------------------------------------------------------------------------------
# SEM the other way

mod.vpd <- '
          #regressions
          
          FC_sqrt ~ PSVs + fges_prop
          NE.sm ~ FC_sqrt + fges_prop
          vpd_sd_szn ~ NE.sm + FC_sqrt + PSVs + fges_prop
          
'

#-------------------------------------------------------------------------------
# Run the SEM
sem <- sem(mod.new, data=poly) 
summary(mod.new, standardized = TRUE, fit.measures=TRUE, rsquare=TRUE)
standardizedSolution(sem, type="std.all")
parameterEstimates(mod.new, standardized = TRUE, ci = TRUE)

# Check the fit
fit <- lavInspect(sem, "fit")
fit["chisq"]; fit["pvalue"]

# AIC: 170.9241
AIC(sem)


#-------------------------------------------------------------------------------
# Alternate methods give similar results
sem <- sem(mod.vpd, data=poly) 
summary(mod.new, standardized = TRUE, fit.measures=TRUE, rsquare=TRUE)
standardizedSolution(sem, type="std.all")

# Check the fit
fit <- lavInspect(sem, "fit")
fit["chisq"]; fit["pvalue"]

# AIC: 174.0699
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
