# Bastiaen Boekelo, 2021
# Goal: Make model of variables created by GEE


rm(list=ls())  # Clean script <- clean mind

# Set Script and Data wd
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # Works if you have a recent version of RStudio.

library(dplyr)
library(data.table)
library(rgdal)
library(raster)
library(RStoolbox)
library(gridExtra)
library(remotes)

source("functions/merge_dfs_30_Classification.R")

# sort_df: Putting a series of columns in front of the table
sort_df <- function(data, colnames){
  for(i in length(colnames):1){
    if(i == length(colnames)){
      new_df <- data[,c(which(colnames(data)==colnames[i]), which(colnames(data)!=colnames[i]))]
    } else {      new_df <- new_df[,c(which(colnames(new_df)==colnames[i]), which(colnames(new_df)!=colnames[i]))]    }}
  return(new_df)
}


# Read grid data
r_burns <- stack("../Data/3_Output/03_HANTS_BurnedArea.tif")
r_lossy <- stack("../Data/3_Output/04_HANSSEN_Lossyear.tif")
r_forob <- stack("../Data/3_Output/05_Forobs_classes.tif")
r_forob <- r_forob[[c(1:34,36:38)]]



matr1 <- matrix(1/9, 3, 3)

r_num <- r_burns[[3]]
r_amp <- r_burns[[4]]
r_avg <- focal(r_amp, matr1)
r_std <- sqrt((r_avg - r_amp)^2)
r_cov <- (r_std+1) / (r_avg+1)
r_aoi <- r_cov / r_num
r_aoi <- sqrt(r_aoi)
r_aoi[is.infinite(r_aoi)==T] <- 0
plot(r_aoi)


colors       <- colorRampPalette(c('black',"lightyellow","yellow","orange","darkorange",'red', 'darkred'))
color_levels <- 10 # The number of colors to use

plot(r_aoi, col=colors(n=color_levels), breaks=c(0, 0.2, 0.4, 0.6, 0.8, 1.0, 1.2, 1.4, 1.6,  maxValue(r_aoi)))
#plot(countries, add=T, border="#999999", lwd=0.1)
#writeRaster(r_aoi, "../Data/0_Temp/covampl_divby_numburns.tif", "GTiff", overwrite=T)



# Discretize first layer of burns (month -> make stack of 12)
for(i in 1:12){
  r_month <- r_burns[[1]]
  r_month[r_month != i] <- 0
  r_month[r_month == i] <- 1
  if(i == 1){   new_months <- r_month   } else {   new_months <- stack(new_months, r_month)   }
}
r_burns <- r_burns[[c(2:4, 6:9)]] # Remove layer 5 and replace peak_month (layer 1) by 12 created rasters (next line)
#r_burns <- stack(r_burns, new_months)

ras     <- stack (r_burns, r_lossy, r_forob)

lossy_pca <- rasterPCA(r_lossy, nSamples=10000, nComp=6, spca = F)
burns_pca <- rasterPCA(r_burns, nSamples=10000, nComp=6, spca = T)
forob_pca <- rasterPCA(r_forob, nSamples=10000, nComp=6, spca = F)

# Add percentages to names of raster
lossy_comp        <- lossy_pca$map
pc_percs          <- as.vector(round((summary(lossy_pca$model)$sdev)^2 / sum((summary(lossy_pca$model)$sdev)^2), 5))
names(lossy_comp) <- paste0(names(lossy_comp), "__", pc_percs[1:6])

burns_comp        <- burns_pca$map
pc_percs          <- as.vector(round((summary(burns_pca$model)$sdev)^2 / sum((summary(burns_pca$model)$sdev)^2), 5))
names(burns_comp) <- paste0(names(burns_comp), "__", pc_percs[1:6])

forob_comp        <- forob_pca$map
pc_percs          <- as.vector(round((summary(forob_pca$model)$sdev)^2 / sum((summary(forob_pca$model)$sdev)^2), 5))
names(forob_comp) <- paste0(names(forob_comp), "__", pc_percs[1:6])

writeRaster(burns_comp, "../Data/3_Output/30_PCA_Burns.tif", "GTiff", overwrite=T)
writeRaster(lossy_comp, "../Data/3_Output/30_PCA_Lossy.tif", "GTiff", overwrite=T)
writeRaster(forob_comp, "../Data/3_Output/30_PCA_Forob.tif", "GTiff", overwrite=T)

ggRGB(lossy_comp, 1, 2, 2, stretch="hist", q=0)
ggRGB(burns_comp, 1, 2, 3, stretch="hist", q=0)
ggRGB(forob_comp, 1, 2, 3, stretch="hist", q=0)

# "Classification" suggestion:
# - Normalize all PCA result bands and multiply them using the PCA weights (forobs and burns)
# - Zonal statistics over these bands for regions of interest creating multidimensional centerpoint
# - Use euclidean distance in order 



test <- stack(lossy_comp[[1]], burns_comp[[1]], forob_comp[[1]])
ggRGB(test, 1, 3, 2, stretch="hist", q=0)

PCA_object <- burns_pca
plots      <- lapply(1:6, function(x) ggR(PCA_object$map, x, geom_raster = TRUE, stretch = "hist"))
plot(grid.arrange(plots[[1]], plots[[2]], plots[[3]], plots[[4]], plots[[5]], plots[[6]], ncol=3))
plot(grid.arrange(plots[[1]], ncol=1))



r_comps <- stack(plots[[1]], plots[[2]], plots[[3]])

blub <- stack(test2$map)
flip <- (blub[[1]] + 0 ) / blub[[3]]
plot(flip)
test$model
############################################################


# Load and merge pixel statistics for burned data / forest loss / forest observations for the intact forest areas & burned areas
opconc_burn <- merge_dfs("../Data/2_Intermediate/Zonals_nongrid", "OPCON__Burned_",  "../Data/2_Intermediate", "OilPalm_concessions_BurnedStats.csv",         "id",  "sum")
intfor_burn <- merge_dfs("../Data/2_Intermediate/Zonals_nongrid", "INTFOR__Burned_",   "../Data/2_Intermediate", "Intact_Forest_BurnedStats.csv",             "fid", "sum")
opconc_fobs <- merge_dfs("../Data/2_Intermediate/Zonals_nongrid", "OPCON__forobs_",  "../Data/2_Intermediate", "OilPalm_concessions_forest_observations.csv", "id",  "sum")
intfor_fobs <- merge_dfs("../Data/2_Intermediate/Zonals_nongrid", "INTFOR__forobs_",  "../Data/2_Intermediate", "Intact_Forests_forest_observations.csv",     "fid", "sum")
opconc_losy <- merge_dfs("../Data/2_Intermediate/Zonals_nongrid", "OPCON__Lossyear_", "../Data/2_Intermediate", "OilPalm_concessions_Hanssen_lossyear.csv",   "id",  "sum")
intfor_losy <- merge_dfs("../Data/2_Intermediate/Zonals_nongrid", "INTFOR__Lossyear_",  "../Data/2_Intermediate", "Intact_Forests_Hanssen_lossyear.csv",      "fid", "sum")

opconc_burn[,2:ncol(opconc_burn)] <- opconc_burn[,2:ncol(opconc_burn)]/opconc_burn$OPCON__Burned_ALL_SUM
intfor_burn[,2:ncol(intfor_burn)] <- intfor_burn[,2:ncol(intfor_burn)]/intfor_burn$INTFOR__Burned_ALL_SUM
opconc_fobs[,2:ncol(opconc_fobs)] <- opconc_fobs[,2:ncol(opconc_fobs)]/opconc_fobs$OPCON__forobs_ALL_SUM
intfor_fobs[,2:ncol(intfor_fobs)] <- intfor_fobs[,2:ncol(intfor_fobs)]/intfor_fobs$INTFOR__forobs_ALL_SUM
opconc_losy[,2:ncol(opconc_losy)] <- opconc_losy[,2:ncol(opconc_losy)]/opconc_losy$OPCON__Lossyear_ALL_SUM
intfor_losy[,2:ncol(intfor_losy)] <- intfor_losy[,2:ncol(intfor_losy)]/intfor_losy$INTFOR__Lossyear_ALL_SUM


names(opconc_burn)



# 
# bt <- burns[[1]]
# # bt[ bt > (cellStats(bt, "mean") - cellStats(bt, "sd")) & bt < (cellStats(bt, "mean") + cellStats(bt, "sd")) ] <- NA
# bt[bt < (cellStats(bt, "mean") + cellStats(bt, "sd")) ] <- NA
# lt <- lossy[[1]]
# # lt[ lt > (cellStats(lt, "mean") - cellStats(lt, "sd")) & lt < (cellStats(lt, "mean") + cellStats(lt, "sd")) ] <- NA
# lt[lt < (cellStats(lt, "mean") + cellStats(lt, "sd")) ] <- NA
# test <- bt + lt
# plot(test)
# length(test[is.na(test) == F])
# plot(bt, lt, pch=9)
# Lossy <- Lossy[[1:20]]


