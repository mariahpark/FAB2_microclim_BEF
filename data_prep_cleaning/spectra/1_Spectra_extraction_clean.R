#  Read and clean up raw spectra for analysis (PLSR and spectral indices)

rm(list=ls())
library(devtools)
library(spectrolab)
library(dplyr)

dir <- ("C:/Users/maria/Desktop/Research/2024/psr")
# Sampling Folder currently working on
Sampling <- 'M2' 
# File with metadata linking spectra files to corresponding Plant IDs and Treatments
sp.log2018 <- read.csv(file.path(dir,'spectra_metadata.csv'))

Species <- 'All'

# Get rid of NA rows in dataframe
sp.log2018 <- sp.log2018[!is.na(sp.log2018$ID), ]

#-------------------------------------------------------------------------------

# Loop to match file names

for (i in 1: length(sp.log2018$ID)) {
  
  if (sp.log2018$ID[i] < 10) {
    
    sp.log2018$Full_ID[i] <- paste('1566060_0000',sp.log2018$ID[i],'.sed',sep = '')
    
  } else if (sp.log2018$ID[i] >= 10 && sp.log2018$ID[i] < 100) {
    
    sp.log2018$Full_ID[i] <- paste('1566060_000',sp.log2018$ID[i],'.sed',sep = '')
    
  } else if (sp.log2018$ID[i] >= 100 && sp.log2018$ID[i] < 119) {
    
    sp.log2018$Full_ID[i] <- paste('1566060_00',sp.log2018$ID[i],'.sed',sep = '')
    
  } else if (sp.log2018$ID[i] >= 119 && sp.log2018$ID[i] < 1000) {
    
    sp.log2018$Full_ID[i] <- paste('1566060_00',sp.log2018$ID[i],'.sed',sep = '')
    
  } else if (sp.log2018$ID[i] >= 1000) {
    
    sp.log2018$Full_ID[i] <- paste('1566060_0',sp.log2018$ID[i],'.sed',sep = '')
    
  }
}


# path output analysis where csv file will be created
dirOut<-paste(dir,"/Spectra/", Species,"/", Sampling,sep='')

# path reflectance files
dirspec<-paste(dir,"/Spectra/", Species,"/", Sampling,sep='')


par(mar = c (4,4,1,.5))

#-------------------------------------------------------------------------------
# Start of loop for reading in files

lf = list.files(dirspec)[grep('.sed',list.files(dirspec))]

spec<- list()

#replace column names

for(i in 1:length(lf)){
  
  dirin = paste(dirspec, lf[i], sep = "/")
  
#-------------------------------------------------------------------------------  
# Importing spectra
  
  spec[i]  = read_spectra(dirin, "sed", type = "target_reflectance")
}

max_len <- max(sapply(spec, length))  # Find the longest element

df <- data.frame(matrix(unlist(spec), nrow=length(lf), byrow=T),stringsAsFactors=FALSE) #enter length of lf in nrow

colnames(df)<-colnames(as.data.frame(read_spectra(dirin, "sed")))[-1]

df$Full_ID <- lf
df<-df[,c(ncol(df),1:(ncol(df)-1))] #placing file name at first column

spec <- as_spectra(df,name_idx = 1)

#-------------------------------------------------------------------------------
#### option 1. FOR PLSR MODELS
# for resampling to 5 nm for spectraltraitmodel PLSR model application
spec = resample(spec, seq(450, 2400, by = 5))
plot(spec)

#-------------------------------------------------------------------------------
#### option 2. FOR SPECTRAL INDICES (don't cut for PLSR)
spec <- spec[,-970:-1000] #eliminate zones of sensor match
spec <- spec[,-1900:-1910]

#-------------------------------------------------------------------------------
# Make into dataframe
dati <- as.data.frame(spec)
colnames(dati)[1]<- 'Full_ID'

plot(spec, main = paste("all data", lf[i], sep = " "))
Mspectra<-merge(sp.log2018,dati,by = 'Full_ID')

#-------------------------------------------------------------------------------
# Export spectra
## REMEMBER: check outlier spectra by hand (remove error measurements)

# For PLSR
write.csv(Mspectra, paste(dirOut, paste("Jul_2024_spectra_5nm_resample_for_PLSR",Sampling,".csv",sep=''), sep = "/"), row.names = FALSE)

# For Spectral indices
# write.csv(Mspectra, paste(dirOut, paste("Jul_2024_spectra_cut_no_smooth",Sampling,".csv",sep=''), sep = "/"), row.names = FALSE)
