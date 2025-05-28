# Figure 3 - part 2
# Relationships between VPD, light, and NBE
rm(list=ls())

library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(ggpmisc)
library(tidyverse)
library(ggpubr)

#-------------------------------------------------------------------------------
setwd("C:/Users/maria/Desktop/Research/2024/processed_df/")
plot.dat <- read.csv("plot.dat.5.26.25.csv")

mod <- lm(light_ratio_mid_to_open ~ vpd_amp, data = plot.dat)
summary(mod)

#updated data
#prod.dat <- read.csv("small_plot_NBE_2024 (2025-4-22).csv")
#fges.dat <- read.csv("gymno.fges.calcs.4.22.25.csv")

#plot.dat <- merge(plot.dat, fges.dat, by = "plot_new")
#plot.dat <- merge(plot.dat, prod.dat, by = "plot_new")

#-------------------------------------------------------------------------------

# Monoculture and polyculture categories
plot.dat$Treatment <- ifelse(plot.dat$SR.y == 1, "Monoculture", "Polyculture")

# Split data
# Filter for just monocultures SR == 1
mono <- plot.dat %>% filter(SR.y == 1)

# Filter for just polycultures SR != 1
poly <- plot.dat %>% filter(SR.y != 1 )


#-------------------------------------------------------------------------------
# Stats
# light and VPD
mod <- lm(light_ratio_mid_to_open ~ vpd_amp, data = plot.dat)
summary(mod)
AIC(mod)

# light and FC
mod <- lm(light_ratio_mid_to_open ~ FC, data = plot.dat)
summary(mod)
AIC(mod)

# FC strengthens the model
mod <- lm(light_ratio_mid_to_open ~ vpd_amp + FC, data = plot.dat)
summary(mod)
AIC(mod)

# VPD and NBE (overyielding)
mod <- lm(NE.sm ~ vpd_amp, data = poly)
summary(mod)

# NBE and FC
mod <- lm(NE.sm ~ FC, data = poly)
summary(mod)

# FC doesn't strengthen the model
mod <- lm(NE.sm ~ vpd_amp + FC, data = poly)
summary(mod)

#-------------------------------------------------------------------------------
# Plotting

gui <- guides(fill = guide_colourbar(barwidth = 15, 
                                     barheight = 0.7, 
                                     title.position = "top",
                                     title.hjust = 0.5))

colour_PA <- scale_fill_gradientn("FC",
                                  colours = c("#fc8d59","#ffffbf","#91bfdb"),
                                  values = scales::rescale(c(0,0.5,1)),
                                  limits = c(0,1),
                                  breaks = c(0,0.5,1))


# NBE ~ VPDamp

a.0 <- ggplot(poly, aes(x = vpd_amp, 
                        y = NE.sm)) +
  geom_point(aes(x = vpd_amp, 
                 y = NE.sm,
                 fill = FC),
             size = 5, 
             shape = 21, 
             colour = "black") + 
  xlim(0.4,1.25)+
  ylim(-1,25)+
  colour_PA +
  theme_classic(base_size=14) +
  xlab(" ") +
  ylab(expression("Overyielding (m"^3*" y"^-1*" ha"^-1*")"))+
  stat_smooth(method = "lm",
              formula = y ~ x,
              colour = "black") + 
  stat_poly_eq(aes(label = paste(..rr.label..,
                                 ..p.value.label.., sep = "~~~")),
               method = "lm",
               formula = y ~ x,
               label.x = "right",
               label.y = "top",
               size = 5.5)+
  gui+
  theme(axis.title.y = element_text(size = 18, margin = margin(r = 35)))+
  theme(axis.title.x = element_text(size = 18, margin = margin(t = 30)))+
  theme(
    axis.text.x = element_text(size = 16),  # Change x-axis text size
    axis.text.y = element_text(size = 16),   # Change y-axis text size
    legend.text = element_text(size = 15),  # Change legend text size
    legend.title = element_text(size = 16)  # Change legend title size
  )
a.0

# Light ~ VPDamp

a.1 <- ggplot(plot.dat,aes(x=vpd_amp, y=light_ratio_mid_to_open,
                           fill = FC))+
  geom_point(size=5, shape = 21, colour = "black")+
  xlim(0.4,1.25)+
  ylim(-0.04,1.1)+
  colour_PA+       
  theme_classic(base_size=14)+
  xlab(" ")+
  ylab("PAR transmission") +
  stat_smooth(method = "lm",
              formula = y ~ (x),
              geom = "smooth",
              colour = "black")+
  stat_poly_eq(aes(label = paste(..rr.label..,
                                 ..p.value.label.., sep = "~~~")),
               method = "lm",
               formula = y ~ (x),
               label.x = "left",
               label.y = "top",
               size = 5.5)+
  gui+
  theme(axis.title.y = element_text(size = 18, margin = margin(r = 35)))+
  theme(axis.title.x = element_text(size = 18, margin = margin(t = 30)))+
  theme(
    axis.text.x = element_text(size = 16),  # Change x-axis text size
    axis.text.y = element_text(size = 16),   # Change y-axis text size
    legend.text = element_text(size = 15),  # Change legend text size
    legend.title = element_text(size = 16)  # Change legend title size
  )

a.1


# Combine figures

fig <- ggarrange(a.1, a.0,
                 ncol = 2, nrow = 1,
                 common.legend= TRUE,
                 legend = "top",
                 labels = c("C","D")
)

annotate_figure(
  fig,
  bottom = text_grob("VPD amplitude (kPa)", size = 18
  )
)

