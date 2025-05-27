rm(list=ls())
################################################################################
##### Model predict
################################################################################

#' @description A script to predict PLSR models based on coefficients

#-------------------------------------------------------------------------------
# Libraries

library(data.table)

#-------------------------------------------------------------------------------
#' Arguments

#' @param path_coefficients A path to a folder with the coefficients
#' @param files_coefficients The name of the files to use
#' @param spectra # Leaf reflectance spectra (tipically at 5 nm)
#' @param transform # A logic vector of length equalt to file_coefficients to back
#' transform the predictions (i.e., log10).

#-------------------------------------------------------------------------------
#' Function
predict_traits <- function(path_coefficients, 
                           files_coefficients,
                           spectra,
                           transform) {
  
  if(length(files_coefficients) != length(transform)) {
    stop("The number files needs to match with the length of the logict vector of transform")
  }
  
  traits <- strsplit(files_coefficients, split = "-")
  
  #Collector
  results <- data.table()
  
  # create progress bar
  pb <- txtProgressBar(min = 0, max = length(files_coefficients), style = 3)
  
  for(i in 1:length(files_coefficients)) {
    
    # update progress bar
    setTxtProgressBar(pb, i)
    
    coefficients <- fread(paste0(path_coefficients, "/", files_coefficients[i]))
    intercept <- coefficients$Intercept
    bands <- colnames(coefficients)[-1]
    
    if(i == 1) {
      meta <- spectra[, 1:7]
      results <- cbind(results, meta)
    }
    
    spectra_oi <- spectra[, ..bands]
    coefficients <- coefficients[, ..bands]
    
    predicted_iterations <- data.table()
    
    #Predict values
    for(ii in 1:nrow(coefficients)) {
      
      predicted <- as.matrix(spectra_oi) %*% as.numeric(coefficients[ii,])
      predicted <- predicted + intercept[ii]
      predicted_iterations <- cbind(predicted_iterations, predicted)
      
    }
    
    if(transform[i] == TRUE) {
      predicted_iterations <- 10^predicted_iterations
    }
    
    #average model outputs, and look at 
    predicted_mean <- rowMeans(predicted_iterations)
    predicted_sd <- predicted_iterations[, .("sd" = sd(.SD)), by = seq_along(1:nrow(predicted_iterations))]
    predicted_sd <- predicted_sd[,2]
    predicted <- cbind("mean" = predicted_mean, predicted_sd)
    colnames(predicted) <- paste0(traits[[i]][1], "-", colnames(predicted))
    
    results <- cbind(results, predicted)
    
  }
  
  return(results)
  
}




#-------------------------------------------------------------------------------
#' @example
#' ##look at batch file for notes
setwd("c:/Users/maria/Desktop/Research/2024/psr")
root_path <- "c:/Users/maria/Desktop/Research/2024/psr"
path_coefficients <- paste0(root_path, "/paper_coefficients")
files_coefficients <- dir(path_coefficients)
# Manually removed extraneous columns in the spectra file
spectra <- fread(paste0("ID_only_Jul_2024_spectra_5nm_resample_for_PLSR.csv"), header = TRUE)

# Look at batch file - if transform = TRUE, it was logged, say TRUE to transform_fresh
transform_fresh <- c(FALSE,FALSE)

results <- predict_traits(path_coefficients, 
                          files_coefficients,
                          spectra,
                          transform = transform_fresh)

fwrite(results, paste0("c:/Users/maria/Desktop/Research/2024/psr/predicted_traits/fab_jul_2024_traits_PLSR_5_10_25.csv"))

