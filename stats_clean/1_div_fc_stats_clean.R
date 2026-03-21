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
library(performance)
library(splines)

conflict_prefer("mutate", "dplyr")
conflict_prefer("summarise", "dplyr")
conflicts_prefer(dplyr::filter)
conflicts_prefer(EnvStats::predict)

#-------------------------------------------------------------------------------
setwd("C:/Users/maria/Desktop/Research/2024/processed_df/")
# plot.dat <- read.csv("plot.dat.2.8.25.csv")
plot.dat <- read.csv("plot.dat.3.19.26.csv")


#-------------------------------------------------------------------------------
# Non-linearity checks
# Visualization

ggplot(plot.dat, aes(x = PSVs, y = FC)) +
  geom_point() +
  geom_smooth(method = "loess") +
  theme_classic()


mod <- lm(FC ~ PSVs, data = plot.dat)

plot(fitted(mod), resid(mod))
abline(h = 0, col = "red")

# Polynomial test
mod_lin  <- lm(FC ~ PSVs, data = plot.dat)
mod_quad <- lm(FC ~ PSVs + I(PSVs^2), data = plot.dat)

anova(mod_lin, mod_quad)

# Spline

# PSVs
mod_spline <- lm(FC ~ ns(PSVs, df = 3), data = plot.dat)
summary(mod_spline)

anova(mod_lin, mod_spline) # not sig

# SR

mod_lin  <- lm(FC ~ SR.x, data = plot.dat)
mod_spline <- lm(FC ~ ns(SR.x, df = 3), data = plot.dat)
summary(mod_spline)

anova(mod_lin, mod_spline)

# PD

mod_lin  <- lm(FC ~ faith.no.root.PD, data = plot.dat)
mod_spline <- lm(FC ~ ns(faith.no.root.PD, df = 3), data = plot.dat)
summary(mod_spline)

anova(mod_lin, mod_spline)

# FD

mod_lin  <- lm(FC ~ FD_faith.no.root_PD, data = plot.dat)
mod_spline <- lm(FC ~ ns(FD_faith.no.root_PD, df = 3), data = plot.dat)
summary(mod_spline)

anova(mod_lin, mod_spline)

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
# Model summaries and diagnostics for export


rq_diagnostics <- function(df, formula, taus = c(0.25, 0.5, 0.75), 
                           R = 500, model_name = "model") {
  library(quantreg)
  library(dplyr)
  library(ggplot2)
  
  # 1. Fit models across taus
  mods <- lapply(taus, function(t) rq(formula, data = df, tau = t))
  names(mods) <- paste0("tau_", taus)
  
  # 2. Extract coefficients + pseudo R²
  results <- lapply(seq_along(mods), function(i) {
    mod <- mods[[i]]
    tau <- taus[i]
    
    s <- summary(mod, se = "boot", R = R)
    
    coef_df <- as.data.frame(s$coefficients)
    coef_df$term <- rownames(coef_df)
    colnames(coef_df) <- c("estimate", "std.error", "statistic", "p.value", "term")
    
    # pseudo R²
    y <- mod$model[[1]]
    pseudo_r2 <- 1 - mod$rho / sum(abs(y - median(y)))
    
    coef_df %>%
      mutate(
        tau = tau,
        model = model_name,
        pseudo_r2 = pseudo_r2
      ) %>%
      select(model, tau, term, estimate, std.error, statistic, p.value, pseudo_r2)
  })
  
  coef_table <- bind_rows(results)
  
  # 3. Residual plot (for median model)
  med_index <- which.min(abs(taus - 0.5))
  med_mod <- mods[[med_index]]
  
  resid_plot <- ggplot(
    data.frame(fitted = fitted(med_mod), resid = resid(med_mod)),
    aes(fitted, resid)
  ) +
    geom_point() +
    geom_hline(yintercept = 0) +
    theme_classic() +
    ggtitle(paste(model_name, "- Residuals vs Fitted (tau =", taus[med_index], ")"))
  
  # 4. Coefficient vs tau plot
  coef_plot <- ggplot(coef_table %>% filter(term != "(Intercept)"),
                      aes(tau, estimate)) +
    geom_point() +
    geom_line() +
    facet_wrap(~term, scales = "free_y") +
    theme_classic() +
    ggtitle(paste(model_name, "- Coefficient vs Tau"))
  
  # 5. Return everything
  list(
    models = mods,
    coefficients = coef_table,
    residual_plot = resid_plot,
    coef_plot = coef_plot
  )
}


# Use
diag.psv <- rq_diagnostics(plot.dat, FC ~ PSVs, model_name = "PSV")
diag.sr  <- rq_diagnostics(plot.dat, FC ~ SR.x,   model_name = "SR")
diag.fd  <- rq_diagnostics(plot.dat, FC ~ FD_faith.no.root_PD,   model_name = "FD")
diag.pd  <- rq_diagnostics(plot.dat, FC ~ faith.no.root.PD,   model_name = "PD")

# Combine into a table

all_results <- bind_rows(
  diag.psv$coefficients,
  diag.sr$coefficients,
  diag.fd$coefficients,
  diag.pd$coefficients
)

# View diagnostics
diag.psv$residual_plot
diag.psv$coef_plot

fwrite(all_results, "quantile.reg.model.3.19.26.csv")

#-------------------------------------------------------------------------------
# Model variance changes using GAM and fitting residuals

var.dat <- plot.dat[order(plot.dat$SR.x), ]
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




