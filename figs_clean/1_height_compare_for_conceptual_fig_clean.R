# Graph of heights for conceptual figure (Fig. 1)

rm(list=ls())
#libraries
library(ggplot2)
library(dplyr)
library(ggpmisc)
library(tidyverse)
library(conflicted)
conflict_prefer("mutate", "dplyr")
conflict_prefer("summarise", "dplyr")
conflicts_prefer(dplyr::filter)

#read growth and diversity data
setwd("C:/Users/maria/Desktop/Research/2024/biomass/")
dat <- read.csv("fab2_growth_clean_4_22_25.csv")

setwd("C:/Users/maria/Desktop/Research/2024/processed_df/clean.data")
div.dat <- read.csv("combined.div.metrics.csv")

dat.24 <- merge(dat, div.dat, by = "plot")

# Make sure height is numeric
dat.24$height <- as.numeric(dat.24$height)

#-------------------------------------------------------------------------------
# Filter for just monocultures SR == 1
mono <- dat.24 %>% filter(measurement_year == 2024 & plot_area == 100 &
                           deadmissing == "No" & SR == 1)


# Remove non-finite data entries and NA
filter_finite_column <- function(df, column_name) {
  df <- df[is.finite(df[[column_name]]), ]
  return(df)
}

mono <- filter_finite_column(mono, "height")

#treatment name
mono$Treatment <- "Monoculture"

#-------------------------------------------------------------------------------
# Calculate avg height for each species
height.avg <- function(dat){
  dat %>%
    group_by(species_code, Treatment) %>%
    summarise(avg.height = mean(height))%>%
    ungroup()
}

height.1 <- height.avg(mono)
# Convert cm into meters
height.1$avg.height.m <- height.1$avg.height/100

print(height.1)

#-------------------------------------------------------------------------------
# Angiosperm / Gymnosperm categories
categorize_species <- function(df, species_col) {
  df$Clade <- ifelse(df[[species_col]] %in% c("TIAM", "QUMA", "QURU",
                                                 "ACNE", "QUAL", "QUEL",
                                                 "ACRU", "BEPA"), "Angiosperm",
                        ifelse(df[[species_col]] %in% c("JUVI", "PIRE", "PIBA",
                                                        "PIST"), "Gymnosperm", NA))
  return(df)
}

height.mono <- categorize_species(height.1, "species_code")

# Species code -> species name for labeling purposes
species_names <- c("TIAM" = "Tilia americana",
                   "QUMA" = "Quercus macrocarpa",
                   "QURU" = "Quercus rubra",
                   "ACNE" =  "Acer negundo",
                   "QUAL" = "Quercus alba",
                   "QUEL" = "Quercus ellipsoidalis",
                   "ACRU" = "Acer rubrum",
                   "BEPA" = "Betula papyrifera",
                   "JUVI" = "Juniperus virginiana",
                   "PIRE" = "Pinus resinosa",
                   "PIBA" = "Pinus banksiana",
                   "PIST"  = "Pinus strobus")

height.mono <- height.mono %>%
  mutate(species_code = str_replace_all(species_code, species_names))

#horizontal height graph
#840 x 690 pixel dimensions for export

height <- height.mono %>%
  mutate(species_code = fct_reorder(species_code, avg.height.m, .na_rm = TRUE)) %>%
  ggplot( aes(x=species_code, y=avg.height.m, fill = Clade))+
  geom_bar(stat="identity", width = 0.75)+
  xlab(" ") +
  ylab("Average tree height in monocultures (m)") +
  theme_classic()+
  theme(axis.title = element_text(size=20),
        axis.text.y = element_text(size=17, face = "italic"),
        axis.text = element_text(size=16),
        legend.title = element_text(size=18),
        legend.text = element_text(size=16),
        legend.position = "right")+
  coord_flip()
height

