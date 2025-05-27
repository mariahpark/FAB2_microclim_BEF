# Figure 2
# Plot relationships between diversity metrics and FC

rm(list=ls())
#libraries
library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(scales)
library(ggpp)
library(ggpmisc)
library(tidyverse)
library(tidyr)
library(xts)
library(broom)
library(ggpmisc)
library(ggpubr)

#-------------------------------------------------------------------------------
# Read data
setwd("C:/Users/maria/Desktop/Research/2024/processed_df/")
plot.dat <- read.csv("plot.dat.5.26.25.csv")


#-------------------------------------------------------------------------------
# Plots

gui <- guides(fill = guide_colourbar(barwidth = 15, 
                                     barheight = 0.7, 
                                     title.position = "top",
                                     title.hjust = 0.5))


colour_PA <- scale_fill_gradientn("FC",
                                  colours = c("#fc8d59","#ffffbf","#91bfdb"),
                                  values = scales::rescale(c(0,0.5,1)),
                                  limits = c(0,1),
                                  breaks = c(0,0.5,1))


# Plot with  quantile regression lines
b.3.1 <- ggplot(plot.dat, aes(x = PSVs, y = FC)) +
  geom_point(aes(fill = FC),size=5, shape=21, colour = "black")+
  geom_quantile(quantiles = 0.25, color = "black", linewidth = 1.2)+
  gui+
  colour_PA+
  labs(#title = "Quantile Regression in ggplot2",
    #subtitle = "25th Quantile",
    x = "Phylogenetic species variability", y = " ") +
  theme_classic() +
  theme(plot.margin = margin(10, 10, 10))+
  theme(
    axis.text.x = element_text(size = 18),  # Change x-axis text size
    axis.text.y = element_text(size = 18),
    axis.title.x = element_text(size=21),
    axis.title.y = element_text(size=21),
    legend.text = element_text(size = 18),  # Change legend text size
    legend.title = element_text(size = 21)  # Change legend title size
  )
b.3.1

b.4.1 <- ggplot(plot.dat, aes(x = SR.y, y = FC)) +
  geom_point(aes(fill = FC),size=5, shape=21, colour = "black")+#alpha = 0.6) +  # Scatter plot
  geom_quantile(quantiles = 0.25, color = "black", linewidth = 1.2)+
  gui+
  colour_PA+
  scale_x_continuous(breaks = c(1, 4, 8, 12))+
  labs(
    x = "Species richness", y = " ") +
  theme_classic() +
  theme(plot.margin = margin(10, 10, 10))+
  theme(
    axis.text.x = element_text(size = 18),  # Change x-axis text size
    axis.text.y = element_text(size = 18),
    axis.title.x = element_text(size=21),
    axis.title.y = element_text(size=21),
    legend.text = element_text(size = 18),  # Change legend text size
    legend.title = element_text(size = 21)  # Change legend title size
  )
b.4.1

b.5.1 <- ggplot(plot.dat, aes(x = FD_faith.no.root_PD, y = FC)) +
  geom_point(aes(fill = FC),size=5, shape=21, colour = "black")+#alpha = 0.6) +  # Scatter plot
  geom_quantile(quantiles = 0.25, color = "black", linewidth = 1.2)+
  gui+
  colour_PA+
  labs(
    x = "Functional diversity", y = " ") +
  theme_classic() +
  theme(plot.margin = margin(10, 10, 10))+
  theme(
    axis.text.x = element_text(size = 18),  # Change x-axis text size
    axis.text.y = element_text(size = 18),
    axis.title.x = element_text(size=21),
    axis.title.y = element_text(size=21),
    legend.text = element_text(size = 18),  # Change legend text size
    legend.title = element_text(size = 21)  # Change legend title size
  )
b.5.1

b.6.1 <- ggplot(plot.dat, aes(x = faith.no.root.PD, y = FC)) +
  geom_point(aes(fill = FC),size=5, shape=21, colour = "black")+#alpha = 0.6) +  # Scatter plot
  geom_quantile(quantiles = 0.25, color = "black", linewidth = 1.2)+
  scale_x_continuous(breaks = c(0, 500, 1000))+
  gui+
  colour_PA+
  labs(
    x = "Phylogenetic diversity", y = " ") +
  theme_classic()+
  theme(plot.margin = margin(10, 10, 10))+
  theme(
    axis.text.x = element_text(size = 18),  # Change x-axis text size
    axis.text.y = element_text(size = 18),
    axis.title.x = element_text(size=21),
    axis.title.y = element_text(size=21),
    legend.text = element_text(size = 18),  # Change legend text size
    legend.title = element_text(size = 21)  # Change legend title size
  )
b.6.1

# Combine graphs
# Export pixel dimensions: 2000 x 624

fig.3 <- ggarrange(b.4.1, b.5.1, b.6.1, b.3.1,
                   ncol = 4, nrow = 1,
                   common.legend= TRUE,
                   legend = "top",
                   labels = c("A","B", "C", "D")
)

annotate_figure(
  fig.3,
  left = text_grob("Fractional cover", size = 21, rot = 90, vjust = 1
  )
)
