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
library(RColorBrewer)

# Read data
setwd("C:/Users/maria/Desktop/Research/2024/processed_df/")
plot.dat <- read.csv("month.dat.3.12.26.csv")

#-------------------------------------------------------------------------------
# TFC Variable
plot.dat$TFC <- -sqrt(1-plot.dat$FC)

# LiDAR dates
lidar.dat <- plot.dat %>%
  filter(month.x %in% c(5, 6, 7))

# Distinct plots for whole season x diversity compariso
dat <- distinct(plot.dat, plot, .keep_all = TRUE)



# Test if seasonal stability metrics work in lieu of vpd_amp

mod <- lm(vpd_amp ~ vpd_stable_month, data = plot.dat)
summary(mod)

mod <- lm(vpd_stable_month ~ FC + month.y, data = plot.dat)
summary(mod)

mod <- lm(vpd_stable_month ~ TFC + month.y, data = plot.dat)
summary(mod)

mod <- lm(vpd_stable_month ~ FD_PSV + month.y, data = plot.dat)
summary(mod)



#-------------------------------------------------------------------------------
# Plotting

gui <- guides(fill = guide_colourbar(barwidth = 15, 
                                     barheight = 0.7, 
                                     title.position = "top",
                                     title.hjust = 0.5))

#Color Palette
colour_PA <- scale_fill_viridis("Proportion FAST species",
                                option = "D",
                                direction = -1,
                                begin = 0,
                                end = 0.95,
                                limits = c(0, 1),
                                breaks = c(0.0, 0.5, 1.0))
# 
# # VPD mean ~ TFC 
# a <- ggplot(plot.dat, aes(x = TFC, y = (vpd_stable_month),
#                           fill = month.y)) +
#   geom_point(size=5, shape = 21, colour = "black")+
#   scale_color_brewer(palette = "RdYlBu")+
#   #ylim(0.24, 1.32)+
#   geom_smooth(method = "lm",
#               formula = y ~ x,
#               colour = "black") +
#   stat_poly_eq(aes(label = paste(..rr.label..,
#                                  ..p.value.label.., sep = "~~~")),
#                method = "lm",
#                formula = y ~ x,#log(x),
#                label.x = "right",
#                label.y = "top",
#                #col = 2, coordinates
#                size = 5.5)+
#   labs(fill = "FAST Proportion") +       
#   xlab(" ") +
#   ylab("season VPD mean") +
#   theme_classic()+
#   gui+
#   theme(axis.title.y = element_text(size = 18, margin = margin(r = 35)))+
#   theme(axis.title.x = element_text(size = 18, margin = margin(t = 30)))+
#   theme(
#     axis.text.x = element_text(size = 16),  # Change x-axis text size
#     axis.text.y = element_text(size = 16),   # Change y-axis text size
#     legend.text = element_text(size = 15),  # Change legend text size
#     legend.title = element_text(size = 16)  # Change legend title size
#   )
# 
# a

#-------------------------------------------------------------------------------
# Multi-equation
# Stability

a <- ggplot(lidar.dat, aes(x = TFC, y = vpd_stable_month)) +
  
  geom_point(aes(color = month.x), size = 4) +
  
  stat_smooth(
    aes(color = month.x, group = month.x),
    method = "lm",
    formula = y ~ x,
    se = TRUE
  ) +
  
  stat_poly_eq(
    aes(color = month.x,
        group = month.x,
        label = paste(..rr.label.., ..p.value.label.., sep = "~~~")),
    method = "lm",
    formula = y ~ x,
    label.x = "left",
    label.y = "top",
    size = 4
  ) +
  
  theme_classic() +
  xlab(" ") +
  ylab("VPD stability") +
  labs(color = "Month")+
  scale_color_viridis()+
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 16, face = "italic")
  )

a

#-------------------------------------------------------------------------------
b <- ggplot(lidar.dat, aes(x = TFC, y = vpd_mean_month)) +
  
  geom_point(aes(color = month.x), size = 4) +
  
  stat_smooth(
    aes(color = month.x, group = month.x),
    method = "lm",
    formula = y ~ x,
    se = TRUE
  ) +
  
  stat_poly_eq(
    aes(color = month.x,
        group = month.x,
        label = paste(..rr.label.., ..p.value.label.., sep = "~~~")),
    method = "lm",
    formula = y ~ x,
    label.x = "right",
    label.y = "top",
    size = 4
  ) +
  
  theme_classic() +
  xlab(" ") +
  ylab("VPD mean") +
  labs(color = "Month")+
  scale_color_viridis()+
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 16, face = "italic")
  )

b

#-------------------------------------------------------------------------------
# sd
c <- ggplot(lidar.dat, aes(x = TFC, y = vpd_sd_month)) +
  
  geom_point(aes(color = month.x), size = 4) +
  
  stat_smooth(
    aes(color = month.x, group = month.x),
    method = "lm",
    formula = y ~ x,
    se = TRUE
  ) +
  
  stat_poly_eq(
    aes(color = month.x,
        group = month.x,
        label = paste(..rr.label.., ..p.value.label.., sep = "~~~")),
    method = "lm",
    formula = y ~ x,
    label.x = "right",
    label.y = "top",
    size = 4
  ) +
  
  theme_classic() +
  xlab(" ") +
  ylab("VPD sd") +
  labs(color = "Month")+
  scale_color_viridis()+
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 16, face = "italic")
  )

c

#-------------------------------------------------------------------------------
# diversity
d <- ggplot(dat, aes(x = faith.no.root.PD, y = vpd_mean_szn)) +
  
  geom_point(aes(color = fges_prop), size = 4) +
  
  stat_smooth(
    #aes(color = month.x, group = month.x),
    method = "lm",
    formula = y ~ x,
    se = TRUE
  ) +
  
  stat_poly_eq(
    aes(#color = month.x,
        #group = month.x,
        label = paste(..rr.label.., ..p.value.label.., sep = "~~~")),
    method = "lm",
    formula = y ~ x,
    label.x = "right",
    label.y = "top",
    size = 4
  ) +
  
  theme_classic() +
  xlab("TFC") +
  ylab("VPD mean") +
  labs(color = "FAST Proportion")+
  scale_color_viridis()+
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 16, face = "italic")
  )

d


#-------------------------------------------------------------------------------
# 1 equation
a <- ggplot(plot.dat,aes(x=TFC, y=vpd_stable_month))+
  geom_point(aes(color = month.y), size=4)+
  theme_classic()+
  #theme(legend.position = "none") +
  xlab(" ")+
  ylab("VPD stability")+
  #ylab(expression("Avg g"[sw]*" (mol m"^-2*" s"^-1*")"))+
  #labs(color = "Species")+
  stat_smooth(data = plot.dat,# %>% filter(species_code.x == "Acer rubrum"),
              aes(group=month.y, color=month.y),
              method = "lm",
              formula = y ~ x, se = T,
              geom = "smooth")+
  stat_poly_eq(aes(label = paste(..rr.label..,
                                 ..p.value.label.., sep = "~~~")),
               method = "lm",
               formula = y ~ x,
               label.x = "left",
               label.y = "top",
               size = 4)+
  scale_color_viridis(option = "magma")+
  #scale_colour_manual(values=Palette)+
  theme(axis.title = element_text(size=14),
        axis.text = element_text(size=12),
        legend.title = element_text(size=18),
        legend.text = element_text(size=16, face = "italic"))

a



# VPD sd ~ TFC 
b <- ggplot(plot.dat, aes(x = TFC, y = (vpd_sd),
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
  theme(axis.title.y = element_text(size = 18, margin = margin(r = 35)))+
  theme(axis.title.x = element_text(size = 18, margin = margin(t = 30)))+
  theme(
    axis.text.x = element_text(size = 16),  # Change x-axis text size
    axis.text.y = element_text(size = 16),   # Change y-axis text size
    legend.text = element_text(size = 15),  # Change legend text size
    legend.title = element_text(size = 16)  # Change legend title size
  )

b


# VPD sd ~ TFC 
c <- ggplot(plot.dat, aes(x = TFC, y = (vpd_stable),
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
  theme(axis.title.y = element_text(size = 18, margin = margin(r = 35)))+
  theme(axis.title.x = element_text(size = 18, margin = margin(t = 30)))+
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
  theme(axis.title.y = element_text(size = 18, margin = margin(r = 35)))+
  theme(axis.title.x = element_text(size = 18, margin = margin(t = 30)))+
  theme(
    axis.text.x = element_text(size = 16),  # Change x-axis text size
    axis.text.y = element_text(size = 16),   # Change y-axis text size
    legend.text = element_text(size = 15),  # Change legend text size
    legend.title = element_text(size = 16)  # Change legend title size
  )

d


# Combine graphs

fig <- ggarrange(a,b,c,
                 ncol = 3, nrow = 1,
                 common.legend= TRUE,
                 legend = "top"#,
                 #labels = c("A","B","C","D")
)

annotate_figure(
  fig,
  bottom = text_grob("Transformed fractional cover", size = 18
  )
)

