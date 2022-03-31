# Bastiaen Boekelo, June 2021

rm(list=ls())  # Clean script <- clean mind

# Set Script and Data wd
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # Works if you have a recent version of RStudio.

library(raster)
library(colorRamps)
library(rts) # raster time series
library(geoTS)
library(rgdal)
source("functions/aggregator.R")

countries <- readOGR("../Data/1_Input", "Countries")

# Load rasters
rasters <- list.files("../Data/2_Intermediate", pattern= "Fobs_", full.names = T)
rasters <- rasters[grepl(".tif$", rasters)]
ras     <- round(stack(rasters) / 10000) # ha

writeRaster(ras, "../Data/3_Output/05_Forobs_classes.tif", "GTiff", overwrite=T)


color_levels <- 10 # the number of colors to use
colors       <- colorRampPalette(c('black', "orange", "red", "darkred", "#770000"))  #create color ramp starting from blue to red

i <- 1

# File
pdf("../Data/3_Output/05_Allrasters_ForestObs_TMF.pdf", height=7, width=7)

# Per year
for(i in 1:nlayers(ras)){
  plot(ras[[i]], col=colors(n=color_levels), breaks=c(0, 1, seq(250, 2500, length.out=color_levels)), 
       main= paste("Class:", names(ras[[i]]) ))
  plot(countries, add=T, border="#999999", lwd=0.1)}

dev.off()





