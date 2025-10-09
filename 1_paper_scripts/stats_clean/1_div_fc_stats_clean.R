# Stats for how tree diversity influences fractional cover (FC)

rm(list=ls())
#libraries
library(ggplot2)
library(dplyr)
library(data.table)
library(tidyr)
library(xts)
library(broom)
library(ggpmisc)
library(lmtest)
library(conflicted)
library(quantreg)
library(mgcv)

conflict_prefer("mutate", "dplyr")
conflict_prefer("summarise", "dplyr")
conflicts_prefer(dplyr::filter)
conflicts_prefer(EnvStats::predict)

#-------------------------------------------------------------------------------
setwd("C:/Users/maria/Desktop/Research/2024/processed_df/")
# plot.dat <- read.csv("plot.dat.2.8.25.csv")
plot.dat <- read.csv("plot.dat.5.26.25.csv")

# Quantile regression------------------------------------------------------------


# Fit quantile regression for different quantiles
taus <- c(0.10, 0.25, 0.50, 0.75, 0.90)
models <- lapply(taus, function(tau) {
  model <- rq(FC ~ PSVs, data = plot.dat, tau = tau)
  data.frame(PSVs = plot.dat$PSVs, PSVs = predict(model, newdata = plot.dat), quantile = tau)
})

# Combine into one dataframe
df_pred <- do.call(rbind, models)
df_pred$quantile <- as.factor(df_pred$quantile)

# Quantile regression stats
mod.psv <- rq(FC ~ PSVs, data = plot.dat, tau = 0.25)
summary(mod.psv)

mod.sr <- rq(FC ~ SR.y, data = plot.dat, tau = 0.25)
summary(mod.sr)

mod.fd <- rq(FC ~ FD_faith.no.root_PD, data = plot.dat, tau = 0.25)
summary(mod.fd)

mod.pd <- rq(FC ~ faith.no.root.PD, data = plot.dat, tau = 0.25)
summary(mod.pd)


# Summary with p-values
summary(mod.psv, se = "boot")  # Use bootstrapped standard errors
summary(mod.sr, se = "boot")
summary(mod.fd, se = "boot")
summary(mod.pd, se = "boot")

# pseudo r-squared
r.psv <- 1 - mod.psv$rho / rq(FC ~ 1, data = plot.dat, tau = 0.25)$rho
print(r.psv)  # Pseudo R-squared

r.sr <- 1 - mod.sr$rho / rq(FC ~ 1, data = plot.dat, tau = 0.25)$rho
print(r.sr)  # Pseudo R-squared

r.fd <- 1 - mod.fd$rho / rq(FC ~ 1, data = plot.dat, tau = 0.25)$rho
print(r.fd)  # Pseudo R-squared

r.pd <- 1 - mod.pd$rho / rq(FC ~ 1, data = plot.dat, tau = 0.25)$rho
print(r.pd)  # Pseudo R-squared


#-------------------------------------------------------------------------------
# Model variance changes using GAM and fitting residuals

var.dat <- plot.dat[order(plot.dat$SR.y), ]
var.dat <- var.dat[!is.na(var.dat$FC), ] #plot 141 is NA for FC; too short for clean LiDAR detection

# Fit a smooth model
mod <- gam(FC ~ s(PSVs), data = var.dat)

# Get residuals
resids <- resid(mod)

# Model the variance of residuals as a function of x
var_model <- gam(I(resids^2) ~ s(var.dat$PSVs))
plot(var_model, main = "Modeled Variance of Residuals")
summary(var_model)

#---------PD
# Fit a smooth model
mod <- gam(FC ~ s(faith.no.root.PD), data = var.dat)

# Get residuals
resids <- resid(mod)

# Model the variance of residuals as a function of x
var_model <- gam(I(resids^2) ~ s(var.dat$faith.no.root.PD))
plot(var_model, main = "Modeled Variance of Residuals")
summary(var_model)

#----------FD
# Fit a smooth model
mod <- gam(FC ~ s(FD_faith.no.root_PD), data = var.dat)

# Get residuals
resids <- resid(mod)

# Model the variance of residuals as a function of x
var_model <- gam(I(resids^2) ~ s(var.dat$FD_faith.no.root_PD))
plot(var_model, main = "Modeled Variance of Residuals")
summary(var_model)

#----------SR
length(unique(var.dat$SR.y))
var.dat$SR <- as.numeric(var.dat$SR.y)
# Fit a smooth model
mod <- gam(FC ~ SR, data = var.dat)

# Get residuals
resids <- resid(mod)

# Model the variance of residuals as a function of x
var_model <- gam(I(resids^2) ~ var.dat$SR)
summary(var_model)




