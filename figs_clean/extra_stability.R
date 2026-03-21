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
plot.dat <- read.csv("plot.dat.3.19.26.csv")

#-------------------------------------------------------------------------------
# TFC Variable
plot.dat$TFC <- -sqrt(1-plot.dat$FC)

# Test if seasonal stability metrics work in lieu of vpd_amp

mod <- lm(vpd_amp ~ vpd_stable_szn, data = plot.dat)
summary(mod)

mod <- lm(vpd_stable_szn ~ FC, data = plot.dat)
summary(mod)

mod <- lm(vpd_stable_szn ~ TFC, data = plot.dat)
summary(mod)

mod <- lm(vpd_stable_szn ~ FD_PSV, data = plot.dat)
summary(mod)



#-------------------------------------------------------------------------------
# Plotting

gui <- guides(fill = guide_colourbar(barwidth = 15, 
                                     barheight = 0.7, 
                                     title.position = "top",
                                     title.hjust = 0.5))

#Color Palette
colour_PA <- scale_fill_viridis("FAST species proportion",
                                option = "D",
                                direction = -1,
                                begin = 0,
                                end = 0.95,
                                limits = c(0, 1),
                                breaks = c(0.0, 0.5, 1.0))

# VPD mean ~ TFC 
a <- ggplot(plot.dat, aes(x = TFC, y = (vpd_mean_szn),
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
  ylab("season VPD mean") +
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


# VPD sd ~ TFC 
b <- ggplot(plot.dat, aes(x = TFC, y = (vpd_sd_szn),
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
  ylab("season VPD sd") +
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


# VPD sd ~ TFC 
c <- ggplot(plot.dat, aes(x = TFC, y = (vpd_stable_szn),
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
               label.x = "left",
               label.y = "top",
               #col = 2, coordinates
               size = 5.5)+
  labs(fill = "FAST Proportion") +       
  xlab(" ") +
  ylab("season VPD stability") +
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


# VPD amp ~ TFC 
d <- ggplot(plot.dat, aes(x = TFC, y = (vpd_amp),
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
  ylab("week VPD amplitude") +
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


# RH
e <- ggplot(plot.dat, aes(x = TFC, y = (reh_stable_szn),
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
               label.x = "left",
               label.y = "top",
               #col = 2, coordinates
               size = 5.5)+
  labs(fill = "FAST Proportion") +       
  xlab(" ") +
  ylab("season RH stability") +
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

e


# Temp
f <- ggplot(plot.dat, aes(x = TFC, y = (temp_stable_szn),
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
               label.x = "left",
               label.y = "top",
               #col = 2, coordinates
               size = 5.5)+
  labs(fill = "FAST Proportion") +       
  xlab(" ") +
  ylab("season temp stability") +
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

f

# Combine graphs
#1000 x 1400

fig <- ggarrange(a,b,
                 c,d,
                 e,f,
                 ncol = 2, nrow = 3,
                 common.legend= TRUE,
                 legend = "top"#,
                 #labels = c("A","B","C","D")
)

annotate_figure(
  fig,
  bottom = text_grob("Transformed fractional cover", size = 18
  )
)

