# Figure 3 - part 1
# Fractional cover relationships with VPDamp and PAR transmission

rm(list=ls())
#libraries
library(ggplot2)
library(dplyr)
library(scales)
library(ggpp)
library(ggpmisc)
library(tidyverse)
library(conflicted)
library(ggpubr)
library(viridis)

# Read data
setwd("C:/Users/maria/Desktop/Research/2024/processed_df/")
plot.dat <- read.csv("plot.dat.5.26.25.csv")

#-------------------------------------------------------------------------------
# THIS WORKS
# VPD Amplitude model
# sqrt transform to separate out the high FC values and show the trend
## call it transformed fractional cover (TFC)
# Think Beer-Lambert Law: light attenuation principles
plot.dat$TFC <- -sqrt(1-plot.dat$FC)

mod <- lm(vpd_amp ~ TFC, data = plot.dat)
summary(mod)
AIC(mod)

# VPDamp ~ FGES
mod <- lm(vpd_amp ~ fges_prop, data = plot.dat)
summary(mod)
AIC(mod)

# FGES and TFC predict VPDamp well together
mod <- lm(vpd_amp ~ TFC + fges_prop, data = plot.dat)
summary(mod)
AIC(mod)

#-------------------------------------------------------------------------------
# Relationship between TFC and light
mod <- lm(light_ratio_mid_to_open ~ TFC, data = plot.dat)
summary(mod)
AIC(mod)

mod <- lm(light_ratio_mid_to_open ~ fges_prop, data = plot.dat)
summary(mod)
AIC(mod)

# slightly better AIC with inclusiong of FGES
mod <- lm(light_ratio_mid_to_open ~ TFC + fges_prop, data = plot.dat)
summary(mod)
AIC(mod)

#-------------------------------------------------------------------------------
# Plotting

gui <- guides(fill = guide_colourbar(barwidth = 15, 
                                     barheight = 0.7, 
                                     title.position = "top",
                                     title.hjust = 0.5))

#Color Palette
colour_PA <- scale_fill_viridis("Proportion FAST",
                                option = "D",
                                direction = -1,
                                begin = 0,
                                end = 0.95,
                                limits = c(0, 1),
                                breaks = c(0.0, 0.5, 1.0))

# VPD amp ~ TFC 
a <- ggplot(plot.dat, aes(x = TFC, y = (vpd_amp),
                          fill = fges_prop)) +
  geom_point(size=5, shape = 21, colour = "black")+
  colour_PA+
  ylim(0.24, 1.32)+
  geom_smooth(method = "lm",
              formula = y ~ x,
              colour = "black") +
  stat_poly_eq(aes(label = paste(..rr.label..,
                                 ..p.value.label.., sep = "~~~")),
               method = "lm",
               formula = y ~ x,#log(x),
               label.x = "right",
               label.y = "top",
               #col = 2, coordinates
               size = 5.5)+
  labs(fill = "FAST Proportion") +       
  xlab(" ") +
  ylab("VPD amplitude (kPa)") +
  theme_classic()+
  gui+
  theme(axis.title.y = element_text(size = 18, margin = margin(r = 35)))+
  theme(axis.title.x = element_text(size = 18, margin = margin(t = 30)))+
  theme(
    axis.text.x = element_text(size = 16),  # Change x-axis text size
    axis.text.y = element_text(size = 16),   # Change y-axis text size
    legend.text = element_text(size = 15),  # Change legend text size
    legend.title = element_text(size = 16)  # Change legend title size
  )

a

# Light (PAR transmission) ~ TFC

b <- ggplot(plot.dat, aes(x = TFC, y = (light_ratio_mid_to_open),
                          fill = fges_prop)) +
  geom_point(size=5, shape = 21, colour = "black")+
  colour_PA+
  geom_smooth(method = "lm",
              formula = y ~ x,
              colour = "black") +
  stat_poly_eq(aes(label = paste(..rr.label..,
                                 ..p.value.label.., sep = "~~~")),
               method = "lm",
               formula = y ~ x,#log(x),
               label.x = "right",
               label.y = "top",
               #col = 2, coordinates
               size = 5.5)+
  labs(color = "FAST Proportion") + 
  xlab(" ") +
  ylab("PAR transmission") +
  theme_classic()+
  gui+      
  theme(axis.title.y = element_text(size = 18, margin = margin(r = 35)))+
  theme(axis.title.x = element_text(size = 18, margin = margin(t = 30)))+
  theme(
    axis.text.x = element_text(size = 16),  # Change x-axis text size
    axis.text.y = element_text(size = 16),   # Change y-axis text size
    legend.text = element_text(size = 15),  # Change legend text size
    legend.title = element_text(size = 16)  # Change legend title size
  )

b

# Combine graphs

fig <- ggarrange(a,b,
                 ncol = 2, nrow = 1,
                 common.legend= TRUE,
                 legend = "top",
                 labels = c("A","B")
)

annotate_figure(
  fig,
  bottom = text_grob("Transformed fractional cover", size = 18
  )
)

