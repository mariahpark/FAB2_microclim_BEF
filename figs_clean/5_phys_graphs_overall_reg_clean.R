# Figure 5
# Physiology graphs

rm(list=ls())

library(ggplot2)
library(dplyr)
library(data.table)
library(ggpmisc)
library(tidyverse)
library(broom)
library(conflicted)
library(ggpubr)

conflict_prefer("mutate", "dplyr")
conflict_prefer("summarise", "dplyr")
conflicts_prefer(dplyr::filter)

# Read data
setwd("C:/Users/maria/Desktop/Research/2024/processed_df/")
phys.dat <- read.csv("phys.dat.10.9.25.csv")

# Set color palette
Palette <- c( "#CC79A7", "#56B4E9", "#D55E00","#009E73")

#-------------------------------------------------------------------------------
# Filter for only plots in which physiology measurements were taken
phys.only <- phys.dat %>% filter(species_code.x ==  "TIAM"|
                                   species_code.x == "ACRU"|
                                   species_code.x == "QUAL"|
                                   species_code.x == "QURU"|
                                   !is.na(avg.gsw.plot))

# Species code -> species name
species_names <- c("TIAM" = "Tilia americana","QURU" = "Quercus rubra",
                   "QUAL" = "Quercus alba", "ACRU" = "Acer rubrum")

phys.only <- phys.only %>%
  mutate(species_code.x = str_replace_all(species_code.x, species_names))

# Change VWC range from decimal to %
phys.only$vwc_range <- phys.only$vwc_range*100

#-------------------------------------------------------------------------------
# VWC --> stomatal conductance

vwc.gsw <- ggplot(phys.only,aes(x=vwc_range, y=avg.gsw.plot))+
  geom_point(aes(color = species_code.x), size=4)+
  theme_classic()+
  theme(legend.position = "none") +
  xlab("Range of soil VWC (%)")+
  ylab(expression("Avg g"[sw]*" (mol m"^-2*" s"^-1*")"))+
  labs(color = "Species")+
  stat_smooth(data = phys.only %>% filter(species_code.x == "Acer rubrum"),
              aes(group=species_code.x, color=species_code.x),
              method = "lm",
              formula = y ~ x, se = F,
              geom = "smooth")+
  scale_colour_manual(values=Palette)+
  theme(axis.title = element_text(size=14),
        axis.text = element_text(size=12),
        legend.title = element_text(size=18),
        legend.text = element_text(size=16, face = "italic"))

vwc.gsw

#-------------------------------------------------------------------------------
# VWC -> leaf water content

vwc.wc <- ggplot(phys.only,aes(x=vwc_range, y=avg.wc))+
  geom_point(aes(color = species_code.x), size=4)+
  theme_classic()+
  theme(legend.position = "none") +
  xlab(" ")+
  ylab("Avg leaf WC (%)")+
  labs(color = "Species")+
  scale_colour_manual(values=Palette)+
  theme(axis.title = element_text(size=14),
        axis.text = element_text(size=12),
        legend.title = element_text(size=18),
        legend.text = element_text(size=16, face = "italic"))
vwc.wc

#-------------------------------------------------------------------------------
# VWC -> LMA

vwc.lma <- ggplot(phys.only,aes(x=vwc_range, y=avg.lma))+
  geom_point(aes(color = species_code.x), size=4)+
  theme_classic()+
  theme(legend.position = "none") +
  xlab(" ")+
  ylab(expression("Avg LMA (g m"^-2*")"))+
  labs(color = "Species")+
  scale_colour_manual(values=Palette)+
  theme(axis.title = element_text(size=14),
        axis.text = element_text(size=12),
        legend.title = element_text(size=18),
        legend.text = element_text(size=16, face = "italic"))
vwc.lma

#-------------------------------------------------------------------------------
# VPD -> stomatal conductance

vpd.gsw <- ggplot(phys.only,aes(x=vpd_amp, y=avg.gsw.plot))+
  geom_point(aes(color = species_code.x), size=4)+
  theme_classic()+
  theme(legend.position = "none") +
  xlab("VPD amplitude (kPa)")+
  ylab(expression("Avg g"[sw]*" (mol m"^-2*" s"^-1*")"))+
  labs(color = "Species")+
  scale_colour_manual(values=Palette)+
  theme(axis.title = element_text(size=14),
        axis.text = element_text(size=12),
        legend.title = element_text(size=18),
        legend.text = element_text(size=16, face = "italic"))
vpd.gsw

#-------------------------------------------------------------------------------
# VPD -> leaf water content

vpd.wc <- ggplot(phys.only,aes(x=vpd_amp, y=avg.wc))+
  geom_point(aes(color = species_code.x), size=4)+
  theme_classic()+
  theme(legend.position = "none") +
  xlab(" ")+
  ylab("Avg leaf WC (%)")+
  labs(color = "Species")+
  stat_smooth(method = "lm", color = "black", linewidth = 2)+
  stat_smooth(aes(group=species_code.x, color=species_code.x),
              method = "lm",
              formula = y ~ x, se = F,
              geom = "smooth")+
  stat_poly_eq(aes(label = paste(..rr.label..,
                                 ..p.value.label.., sep = "~~~")),
               method = "lm",
               formula = y ~ x,
               label.x = "right",
               label.y = "top",
               size = 4)+
  scale_colour_manual(values=Palette)+
  theme(axis.title = element_text(size=14),
        axis.text = element_text(size=12),
        legend.title = element_text(size=18),
        legend.text = element_text(size=16, face = "italic"))
vpd.wc

#-------------------------------------------------------------------------------
# VPD -> LMA

vpd.lma <- ggplot(phys.only,aes(x=vpd_amp, y=avg.lma))+
  geom_point(aes(color = species_code.x), size=4)+
  theme_classic()+
  theme(legend.position = "none") +
  xlab(" ")+
  ylab(expression("Avg LMA (g m"^-2*")"))+
  labs(color = "Species")+
  stat_smooth(method = "lm", color = "black", linewidth = 2)+
  stat_smooth(data = phys.only %>% filter(species_code.x != "Quercus alba"),
              aes(group=species_code.x, color=species_code.x),
              method = "lm",
              formula = y ~ x, se = F,
              geom = "smooth")+
  stat_poly_eq(aes(label = paste(..rr.label..,
                                 ..p.value.label.., sep = "~~~")),
               method = "lm",
               formula = y ~ x,
               label.x = "left",
               label.y = "top",
               size = 4)+
  scale_colour_manual(values=Palette)+
  theme(axis.title = element_text(size=14),
        axis.text = element_text(size=12),
        legend.title = element_text(size=18),
        legend.text = element_text(size=16, face = "italic"))
vpd.lma

#-------------------------------------------------------------------------------
# LMA -> Lignin

lig.lma <- ggplot(phys.only,aes(x=avg.lma, y=avg.lignin))+
  geom_point(aes(color = species_code.x, size=4))+
  labs(color = "Species")+
  theme_classic()+
  xlab(expression("Avg LMA (g m"^-2*")"))+
  ylab("Avg leaf lignin (%)")+
  stat_smooth(method = "lm", color = "black", linewidth = 2)+
  stat_smooth(data = phys.only %>% filter(species_code.x != "Quercus rubra" & species_code.x != "Acer rubrum"),
              aes(group=species_code.x, color=species_code.x),
              method = "lm",
              formula = y ~ x, se = F,
              geom = "smooth")+
  stat_poly_eq(aes(label = paste(..rr.label..,
                                 ..p.value.label.., sep = "~~~")),
               method = "lm",
               formula = y ~ x,
               label.x = "left",
               label.y = "top",
               size = 4)+
  scale_colour_manual(values=Palette)+
  theme(axis.title = element_text(size=14),
        axis.text = element_text(size=12),
        legend.title = element_text(size=18),
        legend.text = element_text(size=16, face = "italic"))
lig.lma

#-------------------------------------------------------------------------------
# Leaf water content -> Carter Miller Stress index

wc.cms <- ggplot(phys.only,aes(x=avg.wc, y=avg.cms))+
  geom_point(aes(color = species_code.x, size=4))+
  labs(color = "Species")+
  theme_classic()+
  xlab("Avg leaf WC (%)")+
  ylab("Avg leaf CMS")+
  stat_smooth(method = "lm", color = "black", linewidth = 2)+
  stat_smooth(aes(group=species_code.x, color=species_code.x),
              method = "lm",
              formula = y ~ x, se = F,
              geom = "smooth")+
  stat_poly_eq(aes(label = paste(..rr.label..,
                                 ..p.value.label.., sep = "~~~")),
               method = "lm",
               formula = y ~ x,
               label.x = "right",
               label.y = "top",
               size = 4)+
  scale_colour_manual(values=Palette)+
  theme(axis.title = element_text(size=14),
        axis.text = element_text(size=12),
        legend.title = element_text(size=18),
        legend.text = element_text(size=16, face = "italic"))
wc.cms

#-------------------------------------------------------------------------------
# Stomatal conductance -> Photosynthesis

gsw.c <- ggplot(phys.only,aes(x=avg.gsw.plot, y=avg.a.plot))+
  geom_point(aes(color = species_code.x, size=4))+
  labs(color = "Species")+
  theme_classic()+
  xlab(expression("Avg g"[sw]*" (mol m"^-2*" s"^-1*")"))+
  ylab(expression("Avg A (µmol m"^-2*" s"^-1*")"))+
  stat_smooth(method = "lm", color = "black", linewidth = 2)+
  stat_smooth(data = phys.only %>% filter(species_code.x == "Tilia americana"),
              aes(group=species_code.x, color=species_code.x),
              method = "lm",
              formula = y ~ x, se = F,
              geom = "smooth")+
  scale_fill_viridis_c()+
  stat_poly_eq(aes(label = paste(..rr.label..,
                                 ..p.value.label.., sep = "~~~")),
               method = "lm",
               formula = y ~ x,
               label.x = "left",
               label.y = "top",
               size = 4)+
  scale_colour_manual(values=Palette)+
  theme(axis.title = element_text(size=14),
        axis.text = element_text(size=12),
        legend.title = element_text(size=18),
        legend.text = element_text(size=16, face = "italic"))
gsw.c

#-------------------------------------------------------------------------------
# Combine graphs and export
# Export dimensions: 1095 x 1000 pixels

fig <- ggarrange(vpd.wc, vwc.wc, wc.cms,
                 vpd.lma, vwc.lma, lig.lma,
                 vpd.gsw, vwc.gsw, gsw.c,
                 ncol = 3, nrow = 3,
                 common.legend= TRUE,
                 legend = "top")

annotate_figure(
  fig
)

