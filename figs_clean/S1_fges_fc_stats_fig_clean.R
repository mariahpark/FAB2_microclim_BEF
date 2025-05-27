# Supplementary Figure 1
# Model and plot relationship between FGES and FC

rm(list=ls())
#libraries
library(ggplot2)
library(dplyr)

#-------------------------------------------------------------------------------
setwd("C:/Users/maria/Desktop/Research/2024/processed_df/")
plot.dat <- read.csv("plot.dat.5.26.25.csv")

# Linear regression model for FC ~ FGES

mod <- lm(FC ~ fges_prop, data = plot.dat)
summary(mod)

#-------------------------------------------------------------------------------
# Plot graph
# Export pixel dimensions: 600 x 600

a.0 <- ggplot(plot.dat,aes(x=fges_prop, y=FC))+
  geom_point(shape = 21, size = 4, fill = "black", color ="grey")+
  theme_classic()+
  xlab("Proportion of FAST species")+
  ylab("Fractional cover") +
  stat_smooth(method = "lm",
              formula = y ~ x,
              geom = "smooth")+
  stat_poly_eq(aes(label = paste(..rr.label..,
                                 ..p.value.label.., sep = "~~~")),
               method = "lm",
               formula = y ~ x,
               label.x = "right",
               label.y = "bottom",
               size = 5.5)+
  theme(axis.title.y = element_text(size = 18))+
  theme(axis.title.x = element_text(size = 18))+
  theme(
    axis.text.x = element_text(size = 16),  # Change x-axis text size
    axis.text.y = element_text(size = 16)   # Change y-axis text size
  )
a.0

