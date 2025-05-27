# Supplementary Figure 2
# Make model to predict VPD_amp from FC and other known variables to use in
## VPD_amp ~ individual species difference_AWP predictions

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
library(purrr)

conflict_prefer("mutate", "dplyr")
conflict_prefer("summarise", "dplyr")
conflicts_prefer(dplyr::filter)

#-------------------------------------------------------------------------------
# Read data

setwd("C:/Users/maria/Desktop/Research/2024/processed_df/")
plot.dat <- read.csv("plot.dat.5.26.25.csv")
prod.dat <- read.csv("productivity.small.2024.4.22.25.csv")

#make a new variable: transformed FC (TFC) = sqrt_FC
plot.dat$sqrt_FC <- -sqrt(1-plot.dat$FC)

#-------------------------------------------------------------------------------
# Develop model to predict VPDamp for plots without sensors installed

# Make column: yes/no observed or predicted data
plot.dat <- plot.dat %>%
  mutate(pred_obs = if_else(is.na(vpd_amp), "predicted", "observed"))

# Training data:
train_data <- plot.dat %>% filter(!is.na(vpd_amp))

# Test data:
test_data <- plot.dat %>% filter(is.na(vpd_amp))

# Model to predict VPDamp from TFC and FAST proportion
# Multiple R-squared:  0.8446,	Adjusted R-squared:  0.8399 
mod <- lm(vpd_amp ~ sqrt_FC + fges_prop, data = train_data)
summary(mod)
AIC(mod)

# Predict missing values
pred_vals <- predict(mod, newdata = test_data)

# Fill in missing VPDamp values
plot.dat$vpd_amp[is.na(plot.dat$vpd_amp)] <- pred_vals

# Plot to visualize model fit
test <- ggplot(plot.dat, aes(x = sqrt_FC, y = vpd_amp,
                             color = fges_prop)) +
  scale_colour_viridis_c(direction = -1)+ 
  geom_point(size=3)+
  geom_smooth(
    method = "lm",
    formula = y ~ x)+
  stat_poly_eq(method = "lm",
               formula = y ~ x,
               label.x = "right",
               label.y = "top",
               #col = 2, coordinates
               size = 4)+
  xlab("Transformed fractional cover") +
  ylab("Combined training and testing VPDamp") +
  labs(color = "FAST proportion")+
  theme_classic()
test

#-------------------------------------------------------------------------------
# Use VPDamp filled-in data to better understand VPD ~ AWP relationships

#species codes
species_codes <- c("Tilia americana" = "TIAM",
                   "Quercus macrocarpa" = "QUMA",
                   "Quercus rubra" = "QURU",
                   "Acer negundo" = "ACNE",
                   "Quercus alba" = "QUAL",
                   "Quercus ellipsoidalis" = "QUEL",
                   "Quercus ellipsodalis" = "QUEL",
                   "Acer rubrum" = "ACRU",
                   "Betula papyrifera" = "BEPA",
                   "Juniperus virginiana" = "JUVI",
                   "Pinus resinosa" = "PIRE",
                   "Pinus banksiana" = "PIBA",
                   "Pinus strobus" = "PIST")

prod.dat$species_code.x <- prod.dat$species
prod.dat <- prod.dat %>%
  mutate(species_code.x = str_replace_all(species_code.x, species_codes))

# Rename plot
colnames(prod.dat)[2] <- "plot"
colnames(prod.dat)[1] <- "species.y"

# Delete repeated variables (slightest rounding difference at 10th decimal place between datasets)
plot.dat$observed_AWP <- NULL
plot.dat$expected_AWP <- NULL
plot.dat$difference_AWP <- NULL
plot.dat$monocultures_AWP <- NULL


# Combine datasets
combo.dat <- merge(prod.dat, plot.dat, by = c("plot",
                                              "species_code.x",
                                              "species.y"), all = T)

# Make each species is associated with plot-level variables
filled.dat <- combo.dat %>%
  group_by(plot) %>%
  fill(SR.y, .direction = "downup") %>%
  fill(PSVs, .direction = "downup") %>%
  fill(faith.no.root.PD, .direction = "downup") %>%
  fill(FD_faith.no.root_PD, .direction = "downup") %>%
  fill(FD_PSV, .direction = "downup") %>%
  fill(fges_prop, .direction = "downup") %>%
  fill(FC, .direction = "downup") %>%
  fill(vpd_amp, .direction = "downup") %>%
  fill(block, .direction = "downup") %>%
  fill(pred_obs, .direction = "downup") %>%
  fill(vwc_range, .direction = "downup") %>%
  fill(vwc_max, .direction = "downup") %>%
  fill(vwc_min, .direction = "downup")

# Difference in AWP (AWP O-E); monocultures = 0
filled.dat <- filled.dat %>% mutate(difference_AWP =
                                      ifelse(SR.y == 1, 0, difference_AWP))

# Filter each species
tiam <- filter(filled.dat, species.y == "Tilia americana")
acne <- filter(filled.dat, species.y == "Acer negundo")
acru <- filter(filled.dat, species.y == "Acer rubrum")
qual <- filter(filled.dat, species.y == "Quercus alba")
quru <- filter(filled.dat, species.y == "Quercus rubra")
quel <- filter(filled.dat, species.y == "Quercus ellipsoidalis")
quma <- filter(filled.dat, species.y == "Quercus macrocarpa")
bepa <- filter(filled.dat, species.y == "Betula papyrifera")
juvi <- filter(filled.dat, species.y == "Juniperus virginiana")
piba <- filter(filled.dat, species.y == "Pinus banksiana")
pire <- filter(filled.dat, species.y == "Pinus resinosa")
pist <- filter(filled.dat, species.y == "Pinus strobus")

# Make a list of the species dataframes
species_list <- list(tiam = tiam, acne = acne, acru = acru, qual = qual,
                     quru = quru, quel = quel, quma = quma, bepa = bepa,
                     juvi = juvi, piba = piba, pire = pire, pist = pist)

#-------------------------------------------------------------------------------
# Stats
vpd_lm_stats <- function(df, formula = difference_AWP ~  vpd_amp) {
  model <- lm(formula, data = df)
  tidy(model) %>%
    select(term, estimate, std.error, statistic, p.value)
}

vpd.results <- map_df(species_list, vpd_lm_stats, .id = "dataset")  

# Export stats as csv file
fwrite(vpd.results, "vpd.infilled.stats.5.26.25.csv")

#-------------------------------------------------------------------------------
# Plotting

#put legend on top
gui <- guides(fill = guide_colourbar(barwidth = 15, 
                                     barheight = 0.7, 
                                     title.position = "top",
                                     title.hjust = 0.5))

# Color palette
colour_PA <- scale_fill_gradientn("FC",
                                  colours = c("#fc8d59","#ffffbf","#91bfdb"),
                                  values = scales::rescale(c(0,0.5,1)),
                                  limits = c(0,1),
                                  breaks = c(0,0.5,1))

# Loop to create plots for each species
plot_regression <- function(df, index) {
  ggplot(df, aes(x = vpd_amp, y = difference_AWP))+
    geom_point(shape = 21, size = 4,
               aes(fill = FC, colour = pred_obs, stroke = 1.25)) +
    scale_colour_manual(" " , values =
                          c("observed" = "grey", "predicted" = "black")) +
    colour_PA +
    ggtitle(paste(df$species.y)) +
    geom_smooth(method = "lm",
                formula = y ~ poly(x),
                color = "black", se = F)+
    stat_poly_eq(aes(x =  vpd_amp, y = difference_AWP,
                     label = paste(..rr.label..,
                                   ..p.value.label.., sep = "~~~")),
                 data = df,
                 formula = y ~ x,
                 parse = TRUE,
                 label.x = "middle",
                 label.y = "top",
                 color = "black",
                 inherit.aes = FALSE,
                 size = 4.5
    )+
    xlim(0.4,1.3)+
    ylab(" ")+
    xlab(" ")+
    scale_y_continuous(labels = label_number(accuracy = 0.01),
                       breaks = breaks_pretty(n = 3))+
    theme_classic()+
    theme(legend.position="top",
          axis.text.x = element_text(size = 16),
          axis.text.y = element_text(size = 16),
          legend.text = element_text(size = 16),
          legend.title = element_text(size = 17),
          plot.title = element_text(size = 19, face = "italic"))+
    gui
}

#-------------------------------------------------------------------------------
# Generate plots for each dataframe
plots <- lapply(seq_along(species_list), function(i) plot_regression(species_list[[i]], i))

# Customize order
fig <- ggarrange(plots[[1]],plots[[2]],plots[[3]], plots[[8]],plots[[5]],plots[[6]],plots[[7]],
                 plots[[4]],plots[[9]],plots[[10]],plots[[11]],plots[[12]],
                 ncol = 4,
                 nrow = 3,
                 common.legend=TRUE,
                 legend = "top"
                 
)  

# Annotate the figure
# Export: 2000 x 1110 pixels
annotate_figure(
  fig,
  bottom = text_grob(expression("VPD"[amp] ~ " (kPa)"), size = 20),
  left = text_grob(expression("AWP"[O-E] ~ " (m"^3* ~ "y"^-1*")"),
                   size = 20, rot = 90, vjust = 1
  )
)

fig
