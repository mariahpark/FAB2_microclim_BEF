# Supplementary Figure 3
# Stats and plotting of FAST relationships with
## complementarity, selection, net biodiversity effects (CE, SE, NBE)

rm(list=ls())
#libraries
library(ggplot2)
library(dplyr)
library(data.table)
library(ggpmisc)
library(tidyverse)
library(conflicted)
library(ggpubr)

conflict_prefer("mutate", "dplyr")
conflict_prefer("summarise", "dplyr")
conflicts_prefer(dplyr::filter)

#-------------------------------------------------------------------------------
# Read and merge dataframes

setwd("C:/Users/maria/Desktop/Research/2024/processed_df/")
plot.dat <- read.csv("species.dat.5.26.25.csv")

#-------------------------------------------------------------------------------
# Split data by FAST proportion (previously called FGES)
plot.dat$fges_50 <- ifelse(plot.dat$fges_prop >= 0.5, ">= 50%", "< 50%")

plot.dat.merged <- plot.dat

# Filter for just monocultures SR == 1
mono <- plot.dat.merged %>% filter(SR.y == 1)

# Filter for just polycultures SR != 1
poly <- plot.dat.merged %>% filter(SR.y != 1)

# Label
mono$Treatment <- "Monoculture"
poly$Treatment <- "Polyculture"

#-------------------------------------------------------------------------------
# Models comparing FGES proportion effect on CE, SE, NBE
mod <- lm(CE.sm ~ fges_prop, data = poly)
summary(mod)

# FGES proportion as factor
poly$fges_50 <- as.factor(poly$fges_50)

anova.mod <- aov(CE.sm ~ fges_50, data = poly)
summary(anova.mod)
TukeyHSD(anova.mod)

anova.mod <- aov(SE.sm ~ fges_50, data = poly)
summary(anova.mod)
TukeyHSD(anova.mod)

anova.mod <- aov(NE.sm ~ fges_50, data = poly)
summary(anova.mod)
TukeyHSD(anova.mod)

factors.mod <- lm(NE.sm ~ CE.sm + SE.sm, data = poly)
summary(factors.mod)

anova.mod <- aov(NE.sm ~ CE.sm, data = poly)
summary(anova.mod)

#-------------------------------------------------------------------------------
# Boxplots comparing FAST proportion influence on CE, SE, NBE

ce <- ggplot(poly, aes(x=fges_50, y=CE.sm, fill=fges_50))+
  geom_boxplot()+
  theme_classic()+
  xlab(" ") +
  ylab(expression("Complementary effect (m"^3*" year"^-1*" ha"^-1*")"))+
  theme_classic()+
  theme(legend.position="none",
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18),
        axis.title.y = element_text(size = 20),
        legend.text = element_text(size = 18),      # Legend labels
        legend.title = element_text(size = 20))
ce

se <- ggplot(poly, aes(x=fges_50, y=SE.sm, fill=fges_50))+
  geom_boxplot()+
  theme_classic()+
  xlab(" ") +
  ylab(expression("Selection effect (m"^3*" year"^-1*" ha"^-1*")"))+
  theme_classic()+
  theme(legend.position="none",
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18),
        axis.title.y = element_text(size = 20),
        legend.text = element_text(size = 18),      # Legend labels
        legend.title = element_text(size = 20))
se

ne <- ggplot(poly, aes(x=fges_50, y=NE.sm, fill=fges_50))+
  geom_boxplot()+
  theme_classic()+
  xlab(" ") +
  ylab(expression("Net biodiversity effect (m"^3*" year"^-1*" ha"^-1*")"))+
  theme_classic()+
  theme(legend.position="none",
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18),
        axis.title.y = element_text(size = 20),
        legend.text = element_text(size = 18),      # Legend labels
        legend.title = element_text(size = 20))
ne

#-------------------------------------------------------------------------------
# Combine figures and export
#1650 x 550 pixel export

fig <- ggarrange(ce,se,ne,
                 ncol = 3, nrow = 1,
                 common.legend= TRUE,
                 legend = "none"
)

annotate_figure(
  fig,
  bottom = text_grob("FAST proportion", size = 20
  )
)
