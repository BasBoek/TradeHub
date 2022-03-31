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

countries    <- readOGR("../Data/1_Input", "Countries")

rasters      <- list.files("../Data/2_Intermediate", pattern= "Bu_", full.names = T)
rasters      <- rasters[grepl(".tif$", rasters)]

ras          <- stack(rasters)
#writeRaster(ras, "../Data/3_Output/BU_rasters.tiff", format="GTiff", overwrite=T)

# Per year
colors       <- colorRampPalette(c('#003300', 'darkred', 'red', "darkorange", "orange", "yellow", "lightyellow"))
color_levels <- 10 # The number of colors to use

pdf("../Data/3_Output/03_Yearsum_rasters_BurnedArea.pdf", height=7, width=7)

# Sum entire period
ras_sum <- sum(ras)
plot(ras_sum, col=colors(n=color_levels), breaks=c(0, 1, 2500, 5000, 10000, 20000, 30000, 40000, 50000, maxValue(ras_sum)),  main= paste0( "Sum of Burned area: 2001-2020 (ha)"))
plot(countries, add=T, border="#999999", lwd=0.1)


years <- seq(1,240,12)-1
for(i in 1:length(years)){
  print(2000 + years[i]/12+1)
  ras_year <- sum(ras[[(years[i]+1):(years[i] + 12)]])
  plot(ras_year, col=colors(n=color_levels), breaks=c(0, 1, 500, 1000, 1500, 2000, 2500, maxValue(ras_year), 117),  main= paste0(2000 + years[i]/12+1, ": Burned area (ha)"))
  plot(countries, add=T, border="#999999", lwd=0.1)
  
  if(i == 1){
    ras_allyears <- ras_year
  } else {
    ras_allyears <- stack(ras_allyears, sum(ras[[(years[i]+1):(years[i] + 12)]]) )
  }
}
dev.off()

ras_allyears <- stack(ras_allyears, ras_sum)
writeRaster(ras_allyears, "../Data/3_Output/03_BU_year_rasters.tif", "GTiff", overwrite=T)




# Per month
colors       <- colorRampPalette(c('#003300', 'darkred', 'red', "darkorange", "orange", "yellow", "lightyellow"))
color_levels <- 10 # The number of colors to use



i <- 1
pdf("../Data/3_Output/03_Allrasters_BurnedArea.pdf", height=7, width=7)
for(i in 1:nlayers(ras)){
    plot(ras[[i]], col=colors(n=color_levels), breaks=seq(0, 2500, length.out=color_levels+1), 
       main= paste("Burned Area:", substr( names(ras[[i]]), 4, 10 )))
  plot(countries, add=T, border="#777777", lwd=0.1)
  
}
dev.off()


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


burns <- as.vector(ras[200, 270])

for(COL in 1:ncol(ras)){
  for(ROW in 1:nrow(ras)){
    
    print(COL)
    
    burns <- as.vector(ras[ROW, COL])
    
    if(any(is.na(burns)) == F){
      
      if(sum(burns) == 0){
        
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
        
        fitLo_hants_missing <- haRmonics(y=burns, 
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
        non_0_avg   <- mean( burns[burns > 0] )
        total_burns <- length( burns[burns > 0] )
        
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
        
        # hants_ras[[1]][ROW, COL] <- peak_month
        # hants_ras[[2]][ROW, COL] <- max_peak
        # hants_ras[[3]][ROW, COL] <- non_0_avg
        # hants_ras[[4]][ROW, COL] <- total_burns
        # hants_ras[[5]][ROW, COL] <- trend
        # hants_ras[[6]][ROW, COL] <- ampl_avg
        # hants_ras[[7]][ROW, COL] <- phase_avg
        # hants_ras[[8]][ROW, COL] <- a_coef_avg
        # hants_ras[[9]][ROW, COL] <- b_coef_avg
        
      }

    }
  }
}

plot(hants_ras)
plot(countries, add=T)
writeRaster(hants_ras, "../Data/3_Output/03_HANTS_BurnedArea.tiff", "GTiff")

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







