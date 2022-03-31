# Bastiaen Boekelo, 19 october 2021
# Calculate zonal statistics for PCA results

rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # Works if you have a recent version of RStudio.

library(raster)
library(rgdal)
library(RStoolbox)
library(sf)
library(dplyr)

# sort_df: Putting a series of columns in front of the table
sort_df <- function(data, colnames){
  for(i in length(colnames):1){
    if(i == length(colnames)){
      new_df <- data[,c(which(colnames(data)==colnames[i]), which(colnames(data)!=colnames[i]))]
    } else {      new_df <- new_df[,c(which(colnames(new_df)==colnames[i]), which(colnames(new_df)!=colnames[i]))]    }}
  return(new_df)
}

# Reading data
PCA_burns <- stack("../Data/3_Output/30_PCA_Burns.tif")
PCA_forob <- stack("../Data/3_Output/30_PCA_Forob.tif")

vals      <- as.data.frame(PCA_forob)

# Component fractions explained (with "0." in front of the numbers)
#    burns: "55191" "16154" "13302" "09595" "04578" "00851"
#    forob: "8901"  "07976" "01447" "00537" "00285" "00226"

OP_map    <- raster("../Data/2_Intermediate/OP_area.tif")
OP_map    <- OP_map / 25000000 * 100
OP_map    <- sqrt(OP_map)

# countries   <- readOGR("../Data/1_Input", "Countries")
# countries   <- spTransform(countries, crs(PCA_burns_r))
# PCA_burns   <- mask(PCA_burns_r, countries)
# PCA_forob   <- mask(PCA_forob_r, countries)

OP_shp      <- readOGR("../Data/1_Input/potential_oil_palm_areas", "potentials")
OP_shp      <- spTransform(OP_shp, crs(PCA_burns))
# OP_shp      <- OP_shp[OP_shp$id == 0,] Makes not much of a difference if eastern oil palms are excluded

fein_shp    <- readOGR("../Data/1_Input/potential_oil_palm_areas", "feintrenie_approximation")
fein_shp    <- spTransform(fein_shp, crs(PCA_burns))

# Create zones around the earlier mapped oil palm areas
foc_mat     <- matrix(c(1,2,3,4,3,2,1), 7, 7)
temp        <- c(1/4,1/3,1/2,1,1/2,1/3,1/4)
for(i in 1:ncol(foc_mat)){  foc_mat[,i] <- sqrt(foc_mat[,i] * temp[i])}

OP_foc      <- focal(OP_map, foc_mat, fun="mean")
OP_foc[OP_foc > 0.5] <- 0.5
hist(OP_foc[OP_foc > 0 & OP_foc < 1], breaks=300)
OP_foc      <- (OP_foc + cellStats(OP_foc, "max")) / cellStats(OP_foc, "max")
# OP_foc      <- OP_foc / cellStats(OP_foc, "max") * 100
# OP_foc      <- sqrt(OP_foc + 1)
# OP_foc      <- (OP_foc + 1.5* OP_foc) / OP_foc
plot(OP_foc)

# Mask and store values
forob_vals  <- na.omit(as.data.frame(mask(PCA_forob, OP_shp)))
burns_vals  <- na.omit(as.data.frame(mask(PCA_burns, OP_shp)))

train_locs          <- as.data.frame(rasterToPoints(mask(PCA_forob, OP_shp)))[,c("x","y")]
train_locs$stratum1 <- 1

forob_vals$clust1 <- 1
forob_vals$clust2 <- kmeans(forob_vals[,1:6], centers = 2, nstart = 100)[[1]]
forob_vals$clust3 <- kmeans(forob_vals[,1:6], centers = 3, nstart = 100)[[1]]
forob_vals$clust4 <- kmeans(forob_vals[,1:6], centers = 4, nstart = 100)[[1]]
forob_vals$clust5 <- kmeans(forob_vals[,1:6], centers = 5, nstart = 100)[[1]]

plot(forob_vals[,1:6], forob_vals[,1:6], col=forob_vals$clust2)
plot(forob_vals[,1:6], forob_vals[,1:6], col=forob_vals$clust3)
plot(forob_vals[,1:6], forob_vals[,1:6], col=forob_vals$clust4)
plot(forob_vals[,1:6], forob_vals[,1:6], col=forob_vals$clust5)

determine_stats <- function(df_values, what_cols){
  for(i in what_cols){
    avg    <- mean(df_values[,i])
    sd     <- sd(df_values[,i])
    
    vals   <- df_values[,i] - min(df_values[,i])
    wght   <- 1 / (sd(vals) / mean(vals) )
    
    if(i == 1){ 
      stats <- c(avg, sd, wght)
    } else  {   
      stats <- rbind(stats, c(avg, sd, wght))
    }
  }
  stats <- as.data.frame(stats)
  names(stats) <- c("avg", "std", "wgt")
  return(stats)
}

forob_stats  <- determine_stats(forob_vals[forob_vals$clust1 ==1,], 1:6)
forob_stats1 <- determine_stats(forob_vals[forob_vals$clust2 ==1,], 1:6)
forob_stats2 <- determine_stats(forob_vals[forob_vals$clust2 ==2,], 1:6)

burns_stats <- determine_stats(burns_vals, 1:6)

# Calculate number of sd's from the calculated average per band and weigh them by the inverse of the CoV
############################################

# Non-clustered
dist        <- abs((PCA_forob - forob_stats$avg) / forob_stats$std) * forob_stats$wgt
dist_forob  <- 1 / ( dist[[1]] + dist[[2]] + dist[[3]] + dist[[4]] + dist[[5]] + dist[[6]] )

# Clustered
dist1       <- abs((PCA_forob - forob_stats1$avg) / forob_stats1$std) * forob_stats1$wgt
dist2       <- abs((PCA_forob - forob_stats2$avg) / forob_stats2$std) * forob_stats2$wgt
dist1_forob <- 1 / ( dist1[[1]] + dist1[[2]] + dist1[[3]] + dist1[[4]] + dist1[[5]] + dist1[[6]] )
dist2_forob <- 1 / ( dist2[[1]] + dist2[[2]] + dist2[[3]] + dist2[[4]] + dist2[[5]] + dist2[[6]] )


# Extra thing
high_prob  <- dist_forob
high_prob[high_prob < 0.12] <- 0

dist       <- abs((PCA_burns - burns_stats$avg) / burns_stats$std) * burns_stats$wgt
dist_burns <- 1 / ( dist[[1]] + dist[[2]] + dist[[3]] + dist[[4]] + dist[[5]] + dist[[6]] )

# Some stats
quantile(dist_forob, 0.4)
quantile(dist_burns, 0.4)
hist(dist_forob[dist_forob < 0.25], breaks=300)
hist(dist_burns[dist_burns < 0.25], breaks=300)

# Potential area high when forob distance is both are high & there is distance to already mapped OP
potentials <- ((dist_forob * dist_burns) + high_prob) * OP_foc
potentials <- potentials / cellStats(potentials, "max") * 100


# Do something with the OP_map in order to take existing oil palm plantations into consideration.
# Potential area high when forob distance is both are high & there is distance to already mapped OP
potentials2                    <- dist_forob * OP_foc

hist(potentials2[potentials2 < 0.2], breaks=200, col="#77BBFF", xlab="Relative likelihood", main="Congo Basin - Likelihood density")
quantile(potentials2[potentials2 > 0.08],seq(0,1,0.01))

# STRATIFICATION

values             <- as.data.frame(rasterToPoints(potentials2))
values$stratum     <- NA

values$stratum[values$layer >= 0.125]                        <- 1
values$stratum[values$layer >= 0.100 & values$layer < 0.125] <- 2
values$stratum[values$layer >= 0.075 & values$layer < 0.100] <- 3
values$stratum[values$layer >= 0.000 & values$layer < 0.075] <- 4

values                               <- merge(values, train_locs, by=c("x","y"), all.x=T)
values$stratum[values$stratum1 == 1] <- 1
values$stratum1                      <- NULL
values$x                             <- values$x - 2500
values$y                             <- values$y - 2500

# Mask
fein_ras                       <- mask(potentials, fein_shp)
fein_ras[fein_ras > 0]         <- 1
fein_ras[is.na(fein_ras) == T] <- 0
potentials_mask                <- potentials  * fein_ras
potentials2_mask               <- potentials2 * fein_ras

ggRGB(potentials, 1, 1, 1, stretch="sqrt", q=0)
plot(potentials2, PCA_forob[[1:6]])

writeRaster(potentials,       "../Data/2_Intermediate/31_Potential_oilpalm_v16.tif",      "GTiff", overwrite=T)
writeRaster(potentials2,      "../Data/2_Intermediate/31_Potential_forob_OP_v16.tif",     "GTiff", overwrite=T)
writeRaster(potentials2_mask, "../Data/2_Intermediate/31_Potential_forob_OP_v16_mask.tif","GTiff", overwrite=T)
writeRaster(potentials_mask,  "../Data/2_Intermediate/31_Potential_oilpalm_v16_mask.tif", "GTiff", overwrite=T)
writeRaster(dist_forob,       "../Data/2_Intermediate/31_Potential_forob_v16.tif",        "GTiff", overwrite=T)
writeRaster(dist_burns,       "../Data/2_Intermediate/31_Potential_burns_v16.tif",        "GTiff", overwrite=T)

# Define strata based on potentials2
strata                                   <- potentials2
strata[strata > 0.125]                   <- 1
strata[strata >= 0.100 & strata < 0.125] <- 2
strata[strata >= 0.075 & strata < 0.100] <- 3
strata[strata >= 0.000 & strata < 0.075] <- 4
temp                                     <- mask(strata, OP_shp)
temp[temp > 0]                           <- 10
temp[is.na(temp) == T]                   <- 0
strata                                   <- strata + temp
strata[strata > 10]                      <- 1

writeRaster(strata,       "../Data/2_Intermediate/31_strata.tif",      "GTiff", overwrite=F)

# Classify combi OP_map
ras <- raster("../Data/2_Intermediate/OP_area.tif")
ras <- ras / 10000 # hectare
ras[ras > 10 ]           <- 30 # max is 2081
ras[ras > 1 & ras <= 10] <- 20 # quantile 70  %
ras[ras > 0 & ras <= 1]  <- 10 # quantile 28  %

combi <- strata + ras
table(as.matrix(combi))
writeRaster(combi, "../Data/0_Temp/Strata_OP_area_class.tif", overwrite=T)


# Correlate

test <- stack(potentials2, strata)

plot(strata, PCA_forob)





