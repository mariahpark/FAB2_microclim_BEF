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
#plot.dat <- read.csv("plot.dat.5.26.25.csv")
plot.dat <- read.csv("plot.dat.3.19.26.csv")

#plot.dat$TFC <- -sqrt(1-plot.dat$FC)

# Remove outliers
#plot.dat[plot.dat$plot == 128, c(90:119)] <- NA

#-------------------------------------------------------------------------------
# Plots

gui <- guides(fill = guide_colourbar(barwidth = 15, 
                                     barheight = 0.7, 
                                     title.position = "top",
                                     title.hjust = 0.5))


colour_PA <- scale_fill_gradientn("FAST species proportion",
                                  colours = c("#fc8d59","#ffffbf","#91bfdb"),
                                  values = scales::rescale(c(0,0.5,1)),
                                  limits = c(0,1),
                                  breaks = c(0,0.5,1))


# FC -> max values
a <- ggplot(plot.dat, aes(x = FC, y = vpd_max,
                          fill = fges_prop)) +
  geom_point(size=5, shape = 21, colour = "black")+
  colour_PA+
  ylim(0.75,3.25)+
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
  ylab("VPD max (kPa)") +
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

a

# FC -> max values
a.2 <- ggplot(plot.dat, aes(x = FC, y = temp_max,
                          fill = fges_prop)) +
  geom_point(size=5, shape = 21, colour = "black")+
  colour_PA+
  ylim(22,35)+
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
  ylab("Temp max (C)") +
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

a.2

# FC -> max values
a.3 <- ggplot(plot.dat, aes(x = FC, y = rel_humidity_max,
                            fill = fges_prop)) +
  geom_point(size=5, shape = 21, colour = "black")+
  colour_PA+
  ylim(92, 100)+
  geom_smooth(method = "lm",
              formula = y ~ x,
              colour = "black") +
  stat_poly_eq(aes(label = paste(..rr.label..,
                                 ..p.value.label.., sep = "~~~")),
               method = "lm",
               formula = y ~ x,#log(x),
               label.x = "right",
               label.y = "bottom",
               #col = 2, coordinates
               size = 5.5)+
  labs(fill = "FAST Proportion") +       
  xlab(" ") +
  ylab("RH max (%)") +
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

a.3

# FC -> 95th percentile values
b <- ggplot(plot.dat, aes(x = FC, y = vpd_q95,
                          fill = fges_prop)) +
  geom_point(size=5, shape = 21, colour = "black")+
  colour_PA+
  ylim(0.75,3.25)+
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
  ylab("VPD 95th percentile (kPa)") +
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

b

b.2 <- ggplot(plot.dat, aes(x = FC, y = temp_q95,
                          fill = fges_prop)) +
  geom_point(size=5, shape = 21, colour = "black")+
  colour_PA+
  ylim(22,35)+
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
  ylab("Temp 95th percentile (C)") +
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

b.2

# Outlier plot 128, under a branch? -- removed
b.3 <- ggplot(plot.dat, aes(x = FC, y = rel_humidity_q95,
                            fill = fges_prop)) +
  geom_point(size=5, shape = 21, colour = "black")+
  colour_PA+
  ylim(92,100)+
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
  ylab("RH 95th percentile (%)") +
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

b.3

# Mean
c <- ggplot(plot.dat, aes(x = FC, y = vpd_mean,
                          fill = fges_prop)) +
  geom_point(size=5, shape = 21, colour = "black")+
  #ylim(0.24, 1.32)+
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
  labs(fill = "FAST Proportion") +       
  xlab(" ") +
  ylab("VPD mean (kPa)") +
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

c

c.2 <- ggplot(plot.dat, aes(x = FC, y = temp_mean,
                          fill = fges_prop)) +
  geom_point(size=5, shape = 21, colour = "black")+
  colour_PA+
  #ylim(0.24, 1.32)+
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
  ylab("Temp mean (C)") +
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

c.2

# Mean -- outlier of Plot 128; under a branch? - removed
c.3 <- ggplot(plot.dat, aes(x = FC, y = rel_humidity_mean,
                          fill = fges_prop)) +
  geom_point(size=5, shape = 21, colour = "black")+
  colour_PA+
  #ylim(0.24, 1.32)+
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
  ylab("RH mean (%)") +
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

c.3

# Min

d <- ggplot(plot.dat, aes(x = FC, y = vpd_min,
                          fill = fges_prop)) +
  geom_point(size=5, shape = 21, colour = "black")+
  #ylim(0.24, 1.32)+
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
  labs(fill = "FAST Proportion") +       
  xlab(" ") +
  ylab("VPD min (kPa)") +
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

d

d.2 <- ggplot(plot.dat, aes(x = FC, y = temp_min,
                            fill = fges_prop)) +
  geom_point(size=5, shape = 21, colour = "black")+
  colour_PA+
  #ylim(0.24, 1.32)+
  geom_smooth(method = "lm",
              formula = y ~ x,
              colour = "black") +
  stat_poly_eq(aes(label = paste(..rr.label..,
                                 ..p.value.label.., sep = "~~~")),
               method = "lm",
               formula = y ~ x,#log(x),
               label.x = "right",
               label.y = "bottom",
               #col = 2, coordinates
               size = 5.5)+
  labs(fill = "FAST Proportion") +       
  xlab(" ") +
  ylab("Temp min (C)") +
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

d.2

# Mean -- outlier of Plot 128; under a branch?
d.3 <- ggplot(plot.dat, aes(x = FC, y = rel_humidity_min,
                            fill = fges_prop)) +
  geom_point(size=5, shape = 21, colour = "black")+
  colour_PA+
  #ylim(0.24, 1.32)+
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
  ylab("RH min (%)") +
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

d.3

#-------------------------------------------------------------------------------
# Combine figures
# Export: 1200 x 1600

fig <- ggarrange(a, a.2, a.3,
                 b, b.2, b.3,
                 c, c.2, c.3,
                 d, d.2, d.3,
                 ncol = 3, nrow = 4,
                 common.legend= TRUE,
                 legend = "top"
                 #labels = c("C","D")
)

annotate_figure(
  fig,
  bottom = text_grob("Fractional cover", size = 18
  )
)
