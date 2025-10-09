# Clean and merge raw microclimate data accordingly

rm(list=ls())
library(openair)
library(lubridate)
library(ggplot2)
library(dplyr)
library(plyr)
library(data.table)
library(purrr)
library(RColorBrewer)
library(gridExtra)
library(ggiraphExtra)
library(grid)
library(tidyverse)
library(tidyr)

# To do:

# P102 - remove week 8/7 - 8/14
# P146: merge first P146 until 5/22 12:00 AM and 146_NEW starting 6/26 11:00 AM
# P117: merge first P117 until 5/23 12:00 AM and 117_NEW starting 7/25 10:00 AM
# P27: remove fully from analyses - not sure when sensor fell
# From all plots - remove 5/29 (spray solution wasp removal day)

#-------------------------------------------------------------------------------
# P102 - remove week 8/7 - 8/14
setwd("c:/Users/maria/Desktop/Research/2024/microclimate/og_dat_05")
p102 <- read.csv("P102-starts-2024-04-10-00-05-00-ends-2024-09-03-00-05-00.csv")

# Filter out 8/7 - 8/14
p102.f <- p102 %>% filter(!(Timestamp.for.sample.frequency.every.30.min >=
                              ymd_hms("2024-08-07 12:01:00") &
                              Timestamp.for.sample.frequency.every.30.min <=
                              ymd_hms("2024-08-15 12:01:00")))
# Export csv
write.csv(p102.f, file = "P102_merged.csv", row.names = FALSE)

#-------------------------------------------------------------------------------
# P146: merge first P146 until 5/22 12:00 AM and 146_NEW starting 6/26 11:00 AM

p146 <- read.csv("P146-starts-2024-04-10-00-05-00-ends-2024-09-03-00-05-00.csv")
p146.new <- read.csv("146_NEW-starts-2024-04-10-00-05-00-ends-2024-09-03-00-05-00.csv")

# Filter OG 146
p146.f <- p146 %>% filter(Timestamp.for.sample.frequency.every.30.min <=
                            ymd_hms("2024-05-22 12:00:00"))

# Filter new 146
p146.new.f <- p146.new %>% filter(Timestamp.for.sample.frequency.every.30.min >=
                                    ymd_hms("2024-06-26 11:00:00"))                          

# Merge 146 data
p146.stacked <- rbind(p146.f, p146.new.f)

# Export csv
write.csv(p146.stacked, file = "P146_merged.csv", row.names = FALSE)

#-------------------------------------------------------------------------------
# P117: merge first P117 until 5/23 12:00 AM and 117_NEW starting 7/25 10:00 AM
p117 <- read.csv("P117-starts-2024-04-10-00-05-00-ends-2024-09-03-00-05-00.csv")
p117.new <- read.csv("117_NEW-starts-2024-04-10-00-05-00-ends-2024-09-03-00-05-00.csv")

# Filter OG
p117.f <- p117 %>% filter(Timestamp.for.sample.frequency.every.30.min <=
                            ymd_hms("2024-05-23 12:00:00"))

# Filter new
p117.new.f <- p117.new %>% filter(Timestamp.for.sample.frequency.every.30.min >
                                    ymd_hms("2024-07-25 10:00:00"))      

# Merge 117 data
p117.stacked <- rbind(p117.f, p117.new.f)

# Export csv
write.csv(p117.stacked, file = "P117_merged.csv", row.names = FALSE)
