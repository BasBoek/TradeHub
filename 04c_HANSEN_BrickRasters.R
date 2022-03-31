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
rasters <- list.files("../Data/2_Intermediate", pattern= "LossY_", full.names = T)
rasters <- rasters[grepl(".tif$", rasters)]
ras     <- round(stack(rasters) / 10000) # ha
writeRaster(ras, "../Data/3_Output/04_HANSSEN_Lossyear.tif", "GTiff", overwrite=T)


color_levels <- 10 # the number of colors to use
colors       <- colorRampPalette(c('#014d00', "orange", "red", "darkred", "#770000"))  # create color ramp 


# File unsmoothed
pdf("../Data/3_Output/04_Allrasters_ForestLoss.pdf", height=7, width=7)

# Median
ras_median <- calc(ras, function(x) median(x))
plot(ras_median, col=colors(n=color_levels), breaks=c(0, 1, 10, 25, 50, 75, 100, maxValue(ras_median), 117),  main= paste("Median Forest Loss (ha)"))
plot(countries, add=T, border="#999999", lwd=0.1)

# Per year
for(i in 1:nlayers(ras)){
  plot(ras[[i]], col=colors(n=color_levels), breaks=c(0, 1, 10, 25, 50, 75, 100, 250, maxValue(ras[[i]])), 
       main= paste("Forest loss:", substr( names(ras[[i]]), 7, 10 )))
  plot(countries, add=T, border="#999999", lwd=0.1)}

dev.off()


# File smoothed

w1 <- 0.50
w2 <- 0.25

for(i in 1:length(names(ras))){
  if(i == 1 ){
    r_smooth <- (w1+w2)*ras[[i]] + w2*ras[[i+1]]
  } else {
    if(i != length(names(ras))){
      r_new <- w2*ras[[i-1]] + w1*ras[[i]] + w2*ras[[i+1]]
      r_smooth <- stack(r_smooth, r_new)
    } else {
      r_new <- (w1+w2)*ras[[i]] + w2*ras[[i-1]]
      r_smooth <- stack(r_smooth, r_new)
    }
  }
}
names(r_smooth) <- names(ras)
writeRaster(r_smooth, "../Data/3_Output/04_HANSSEN_Lossyear_smoothed.tif", "GTiff", overwrite=T)

# Median
ras_median <- calc(r_smooth, function(x) median(x))
plot(ras_median, col=colors(n=color_levels), breaks=c(0, 1, 10, 25, 50, 75, 100, maxValue(ras_median), 117),  main= paste("Median Forest Loss (ha)"))
plot(countries, add=T, border="#999999", lwd=0.1)

pdf("../Data/3_Output/04_Allrasters_ForestLoss_smoothed.pdf", height=7, width=7)

# Per year
for(i in 1:nlayers(r_smooth)){
  plot(r_smooth[[i]], col=colors(n=color_levels), breaks=c(0, 1, 10, 25, 50, 75, 100, 250, maxValue(r_smooth[[i]])), 
       main= paste("Forest loss:", substr( names(r_smooth[[i]]), 7, 10 )))
  plot(countries, add=T, border="#999999", lwd=0.1)}

dev.off()

r_burns[200,200]



# Initialize raster for output of HANTS
hants_ras                 <- ras
hants_ras[hants_ras > -1] <- 0
hants_ras                 <- stack(hants_ras[[1]], hants_ras[[1]], hants_ras[[1]], 
                                   hants_ras[[1]], hants_ras[[1]], hants_ras[[1]], 
                                   hants_ras[[1]], hants_ras[[1]], hants_ras[[1]])
names(hants_ras)          <- c("peak_month",  "max_peak",   "non_0_avg", 
                               "total_burns", "trend",      "amplitude_avg", 
                               "phase_avg",   "a_coef_avg", "b_coef_avg" )

# dates <- paste0(gsub("_", "-", substr(names(ras),4,10)),"-28")
# dates <- as.Date(dates) # or d <- as.POSIXct(d) 
# rt <- rts(ras, dates) # creating a RasterStackTS object
# burns <- as.vector(ras[1, 1])
# nlayers(ras)
# length(dates)
# names(ras)
# burns <- as.vector(ras[200, 250])

# HANTS parameters
METHOD           <- "hants"
NUMFREQ          <- 20
TS               <- 1:nlayers(ras)
LENBASEPERIOD    <- nlayers(ras)
LOW              <- 0
HIGH             <- 2500
FITERRORTOL      <- 0
DEGREESOVERDETER <- 1
DELTA            <- 0.1


ras_m2 <- round(ras * 10000) # m2

VALUES <- as.vector(ras[220, 271])
plot(VALUES)
par(mfrow=c(1,1))
for(COL in 1:ncol(ras_m2)){
  
  print(paste0(round(COL / ncol(ras_m2) * 100,2), "%"))
  
  for(ROW in 1:nrow(ras_m2)){
    
    VALUES <- as.vector(ras_m2[ROW, COL])
    
    if(any(is.na(VALUES)) == F){
      
      if(sum(VALUES) == 0){
        
        all_values          <- rep(0,9)
        
        hants_ras[ROW, COL] <- all_values
        
        # hants_ras[[1]][ROW, COL] <- 0
        # hants_ras[[2]][ROW, COL] <- 0
        # hants_ras[[3]][ROW, COL] <- 0
        # hants_ras[[4]][ROW, COL] <- 0
        # hants_ras[[5]][ROW, COL] <- 0
        # hants_ras[[6]][ROW, COL] <- 0
        # hants_ras[[7]][ROW, COL] <- 0
        # hants_ras[[8]][ROW, COL] <- 0
        # hants_ras[[9]][ROW, COL] <- 0
        
      } else {
        
        fitLo_hants_missing <- haRmonics(y=VALUES, 
                                         method          = METHOD,
                                         numFreq         = NUMFREQ, 
                                         ts              = TS,
                                         lenBasePeriod   = LENBASEPERIOD,
                                         HiLo            = "Lo", low=LOW, high=HIGH,
                                         fitErrorTol     = FITERRORTOL, 
                                         degreeOverDeter = DEGREESOVERDETER, 
                                         delta           = DELTA)
        
        # Parameter avgs
        lm         <- lm(1:length(fitLo_hants_missing$fitted) ~ fitLo_hants_missing$fitted)
        trend      <- round(as.numeric(lm$coefficients[2]) * 1000)
        ampl_avg   <- round(mean(fitLo_hants_missing$amplitude))
        phase_avg  <- round(mean(fitLo_hants_missing$phase))
        a_coef_avg <- round(mean(fitLo_hants_missing$a.coef))
        b_coef_avg <- round(mean(fitLo_hants_missing$b.coef))
        
        # Average burned area for fire month
        non_0_avg   <- mean( VALUES[VALUES > 0] )
        total_burns <- length( VALUES[VALUES > 0] )
        
        # Peak month
        fit_avg    <- as.data.frame(cbind(rep(1:12,20), fitLo_hants_missing$fitted))
        fit_avg    <- aggregator(fit_avg, "V2", "V1", "mean")
        peak_month <- fit_avg$V1[fit_avg$V2 == max(fit_avg$V2)]
        if(length(peak_month) > 1){
          peak_month <- 0
        }
        max_peak            <- round(max(fit_avg$V2))
        
        # Fill in 
        all_values          <- c(peak_month,  max_peak,   non_0_avg, 
                                 total_burns, trend,      ampl_avg, 
                                 phase_avg,   a_coef_avg, b_coef_avg)
        
        hants_ras[ROW, COL] <- all_values
        
        
      }
      
      
    }
  }
}

plot(hants_ras)
plot(countries, add=T)
writeRaster(hants_ras, "../Data/3_Output/HANTS_ForestLoss.tiff", "GTiff")

# fitHi_hants_missing <- haRmonics(y=burns, 
#                                  method          = METHOD,
#                                  numFreq         = NUMFREQ, 
#                                  ts              = TS,
#                                  lenBasePeriod   = LENBASEPERIOD,
#                                  HiLo            = "Hi", low=LOW, high=HIGH,
#                                  fitErrorTol     = FITERRORTOL, 
#                                  degreeOverDeter = DEGREESOVERDETER, 
#                                  delta           = DELTA)
# 
# 
# fit_harmR_missing <- haRmonics(y       = burns,
#                                numFreq = NUMFREQ,
#                                delta   = DELTA)


plot(burns, pch = 16, main = "Example HANTS fit")
lines(fitLo_hants_missing$fitted, lty = 4, col = "darkred")
lines(fitHi_hants_missing$fitted, lty = 2, col = "darkgreen")
lines(fit_harmR_missing$fitted,   lty = 4, col = "black")

fitLo_hants_missing$phase

length







