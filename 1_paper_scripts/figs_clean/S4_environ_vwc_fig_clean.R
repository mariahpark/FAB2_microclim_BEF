# Supplementary Figure 4
# Graph VWC relationships with gymnosperm proportion in plots

rm(list=ls())
#libraries
library(ggplot2)
library(dplyr)
library(scales)
library(ggpp)
library(ggpmisc)
library(tidyverse)
library(ggpubr)
library(viridis)
#-------------------------------------------------------------------------------
# Read data
setwd("C:/Users/maria/Desktop/Research/2024/processed_df/")
plot.dat <- read.csv("plot.dat.5.26.25.csv")

#-------------------------------------------------------------------------------
# Make VWC into percent
plot.dat$vwc_range <- plot.dat$vwc_range*100
plot.dat$vwc_min <- plot.dat$vwc_min*100
plot.dat$vwc_max <- plot.dat$vwc_max*100
plot.dat$vwc_mean <- plot.dat$vwc_mean*100

#make gymnosperm prop into a percent
plot.dat$gymno_prop <- plot.dat$gymno_prop*100

#-------------------------------------------------------------------------------
# Plotting VWC relationships

gui <- guides(fill = guide_colourbar(barwidth = 15, 
                                     barheight = 0.7, 
                                     title.position = "top",
                                     title.hjust = 0.5))

# Color Palette
colour_PA <- scale_fill_viridis("PAR transmission",
                                option = "D",
                                direction = 1,
                                begin = 0,
                                end = 0.95,
                                limits = c(0, 1),
                                breaks = c(0.0, 0.5, 1.0))


gymno.range <- ggplot(plot.dat, aes(x = gymno_prop, y = vwc_range,
                                    fill = light_ratio_mid_to_open)) +
  geom_point(size=5, shape = 21, colour = "black")+
  colour_PA+
  geom_smooth(method = "lm",
              formula = y ~ x,
              colour = "black") +
  stat_poly_eq(aes(label = paste(..rr.label..,
                                 ..p.value.label.., sep = "~~~")),
               method = "lm",
               formula = y ~ x,
               label.x = "right",
               label.y = "top",
               size = 5.5)+
  ylim(0,40)+
  labs(fill = "PAR transmission") +      
  xlab(" ") +
  ylab("VWC range (%)") +
  theme_classic()+
  gui+
  theme(axis.title.y = element_text(size = 18))+
  theme(axis.title.x = element_text(size = 18))+
  theme(
    axis.text.x = element_text(size = 16),  # Change x-axis text size
    axis.text.y = element_text(size = 16),  # Change y-axis text size
    legend.text = element_text(size = 15),  # Change legend text size
    legend.title = element_text(size = 16)  # Change legend title size
  )
gymno.range


gymno.min <- ggplot(plot.dat, aes(x = gymno_prop, y = vwc_min,
                                  fill = light_ratio_mid_to_open)) +
  geom_point(size=5, shape = 21, colour = "black")+
  colour_PA+
  geom_smooth(method = "lm",
              formula = y ~ x,
              colour = "black") +
  stat_poly_eq(aes(label = paste(..rr.label..,
                                 ..p.value.label.., sep = "~~~")),
               method = "lm",
               formula = y ~ x,
               label.x = "right",
               label.y = "top",
               size = 5.5)+
  ylim(0,40)+
  labs(fill = "PAR transmission") +      
  xlab(" ") +
  ylab("VWC minimum (%)") +
  theme_classic()+
  gui+
  theme(axis.title.y = element_text(size = 18))+
  theme(axis.title.x = element_text(size = 18))+
  theme(
    axis.text.x = element_text(size = 16),  # Change x-axis text size
    axis.text.y = element_text(size = 16),   # Change y-axis text size
    legend.text = element_text(size = 15),  # Change legend text size
    legend.title = element_text(size = 16)  # Change legend title size
  )
gymno.min

gymno.mean <- ggplot(plot.dat, aes(x = gymno_prop, y = vwc_mean,
                                   fill = light_ratio_mid_to_open)) +
  geom_point(size=5, shape = 21, colour = "black")+
  colour_PA+
  geom_smooth(method = "lm",
              formula = y ~ x,
              colour = "black") +
  stat_poly_eq(aes(label = paste(..rr.label..,
                                 ..p.value.label.., sep = "~~~")),
               method = "lm",
               formula = y ~ x,
               label.x = "right",
               label.y = "top",
               size = 5.5)+
  labs(fill = "PAR transmission") +   
  ylim(0,40)+
  xlab(" ") +
  ylab("VWC mean (%)") +
  theme_classic()+
  gui+
  theme(axis.title.y = element_text(size = 18))+
  theme(axis.title.x = element_text(size = 18))+
  theme(
    axis.text.x = element_text(size = 16),  # Change x-axis text size
    axis.text.y = element_text(size = 16),  # Change y-axis text size
    legend.text = element_text(size = 15),  # Change legend text size
    legend.title = element_text(size = 16)  # Change legend title size
  )
gymno.mean

gymno.max <- ggplot(plot.dat, aes(x = gymno_prop, y = vwc_max,
                                  fill = light_ratio_mid_to_open)) +
  geom_point(size=5, shape = 21, colour = "black")+
  colour_PA+
  labs(fill = "PAR transmission") +   
  ylim(0,40)+
  xlab(" ") +
  ylab("VWC maximum (%)") +
  theme_classic()+
  gui+
  theme(axis.title.y = element_text(size = 18))+
  theme(axis.title.x = element_text(size = 18))+
  theme(
    axis.text.x = element_text(size = 16),  # Change x-axis text size
    axis.text.y = element_text(size = 16),  # Change y-axis text size
    legend.text = element_text(size = 15),  # Change legend text size
    legend.title = element_text(size = 16)  # Change legend title size
  )
gymno.max

#-------------------------------------------------------------------------------
# Combine plots
# Export dimensions: 1500 x 512 pixels

fig <- ggarrange(gymno.max, gymno.mean, gymno.min, gymno.range,
                 ncol = 4, nrow = 1,
                 common.legend= TRUE,
                 legend = "top"
)

annotate_figure(
  fig,
  bottom = text_grob("Gymnosperm proportion (%)", size = 18
  )
)
