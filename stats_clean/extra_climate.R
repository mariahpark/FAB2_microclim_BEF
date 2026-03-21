# Climate patterns

rm(list=ls())
#libraries
library(ggplot2)
library(dplyr)
library(scales)
library(ggpp)
library(ggpmisc)
library(tidyverse)
library(ggpubr)
library(viridis)
library(lubridate)
conflicts_prefer(lubridate::month)

df_july <- df %>%
  mutate(date = mdy(date)) %>%   # convert character to Date
  filter(month(date) == 7)
#-------------------------------------------------------------------------------
# Read data
setwd("C:/Users/maria/Desktop/Research/2024/raw_df/")
plot.dat <- read.csv("e80_Daily_Climate.csv")

#-------------------------------------------------------------------------------
df_july <- plot.dat %>%
  mutate(Date = mdy(Date)) %>%   # convert character to Date
  filter(month(Date) == 7)

#-------------------------------------------------------------------------------
df_sample <- df_july %>%
  #mutate(Date = mdy(Date)) %>%   # convert character to Date
  filter(Date >= "2024-07-16" & Date < "2024-07-20")

df_month <- df_july %>%
  #mutate(Date = mdy(Date)) %>%   # convert character to Date
  filter(Date >= "2024-07-1" & Date <= "2024-07-31")


# overall temp
mean(df_july$MaxTemp.degC.)
mean(df_july$MinTemp.degC.)

# overall precip
mean(df_july$Precip.mm.)


# sampling temp
max(df_sample$MaxTemp.degC.)
mean(df_sample$MinTemp.degC.)

# sampling precip
mean(df_sample$Precip.mm.)

# month precip
mean(df_month$Precip.mm.)
