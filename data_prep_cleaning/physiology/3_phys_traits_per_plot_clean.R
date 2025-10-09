# Make sure each plot and species has appropriate plot-level and species-level metrics

rm(list=ls())

library(dplyr)
library(data.table)
library(conflicted)
conflict_prefer("mutate", "dplyr")
conflict_prefer("summarise", "dplyr")

setwd("C:/Users/maria/Desktop/Research/2024/processed_df/")
dat <- read.csv("combo.dat.2024.10.9.25.csv")

#-------------------------------------------------------------------------------
# Make sure all plots have plot level metrics
# Fill missing values using aggregate
for (metric in c(89:157)) {
  dat[metric] <- ave(dat[[metric]], dat$plot, FUN = function(x) {
    ifelse(is.na(x), mean(x, na.rm = TRUE), x)
  })
}

# Physiology traits by species per plot
phys.plot <- function(dat){
  dat %>%
    group_by(plot, species_code.x) %>%
    summarise(avg.a.plot = mean(Avg_A),
              avg.gsw.plot = mean(Avg_gsw),
              avg.E.plot = mean(Avg_E),
    ) %>%
    ungroup()
}

avg.phys.plot <- phys.plot(dat)
print(avg.phys.plot)

# Leaf traits
leaf.traits.plot <- function(dat){
  dat %>%
    group_by(plot, species_code.x) %>%
    summarise(avg.lma = mean(lma),
              avg.cms = mean(cms.mean),
              avg.lignin = mean(lignin.mean),
              avg.wc = mean(WC.mean)) %>%
    ungroup()
}

avg.traits.plot <- leaf.traits.plot(dat)
print(avg.traits.plot)

#-------------------------------------------------------------------------------
# Merge data and export
dat.merged <- merge(dat, avg.phys.plot, by = c("plot","species_code.x"))
dat.merged <- merge(dat.merged, avg.traits.plot, by = c("plot","species_code.x"))

fwrite(dat.merged, "combo.dat.2024.all.plots.trait.means.10.9.25.csv")
