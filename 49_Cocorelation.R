
# load libraries
rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(biomod2)
library(raster)
library(rgdal)
library(corrplot)

#source("R/Species_Modelling/Write_SDM_Rasters.R")

SP_name      <- "oilpalm"

####################################################
### 1 - LOAD DATA & PRE-PROCESS
####################################################


# 1) Prediction rasters & study area
##############################################

pred_loc01   <- "../Data/1_Input/Predictors_250m/Stacked/3rd export/CB_preds_01_10.tif"
pred_loc02   <- "../Data/1_Input/Predictors_250m/Stacked/3rd export/CB_preds_11_20.tif"
pred_loc03   <- "../Data/1_Input/Predictors_250m/Stacked/3rd export/CB_preds_21_30.tif"
pred_loc04   <- "../Data/1_Input/Predictors_250m/Stacked/3rd export/CB_preds_31_40.tif"
pred_loc05   <- "../Data/1_Input/Predictors_250m/Stacked/3rd export/CB_preds_41_50.tif"

pred_ras <- stack(stack(pred_loc01), 
                  stack(pred_loc02),
                  stack(pred_loc03),
                  stack(pred_loc04),
                  stack(pred_loc05))

mask_area   <- extent(pred_ras)

# 2) Point data
##############################################

#GS_all       <- readOGR("../Data/1_Input/GeoSurvey/Exports/shp", "export-drc-oil-palm_2022_03_23")
GS_all   <- readOGR("../Data/1_Input/GeoSurvey/Exports/shp/For_predictor_var_extraction", "OP_2022_03_23_single")
GS       <- crop(GS_all, mask_area) # CHOOSE WHAT AREA

# Preprocess GeoSurvey points
GS           <- GS[GS$present != "Unsure",]               # Remove the entries classified as unsure
GS           <- GS[!duplicated(GS@data[c("lat","lon")]),] # Remove (for now) coordinates with double entry points

GS$present   <- gsub("No",  0,  GS$present)
GS$present   <- gsub("Yes", 1,  GS$present)
GS$org_pres[GS$org_pres != "-" & GS$org_pres != "0"] <- 1
GS$org_pres  <- gsub("-",   0, GS$org_pres)

GS$uno_pres[GS$uno_pres != "-" & GS$uno_pres != "0"] <- 1
GS$uno_pres  <- gsub("-",   0, GS$uno_pres)

GS$present   <- as.numeric(GS$present)
GS$org_pres  <- as.numeric(GS$org_pres)
GS$uno_pres  <- as.numeric(GS$uno_pres)

# Focus on unorganized systems only (FOR NOW, ADJUST LATER)
GS <- GS[(GS$uno_pres == 1) | GS$present == 0,]

# Rename columns
GS$ORIG_FID <- NULL
colnames(GS@data)[(ncol(GS)-49):ncol(GS)] <- names( pred_ras )
GS@data[(ncol(GS)-49):ncol(GS)]           <- as.data.frame(sapply(GS@data[(ncol(GS)-49):ncol(GS)], as.numeric))

# 3) Pseudo-absences
##############################################

PA          <- readOGR("../Data/1_Input/PA_regions", "PAs_CB_100000_clipped_single") # almost 80,000 points
PA$id       <- NULL
PA$ORIG_FID <- NULL
PA@data     <- as.data.frame(sapply(PA@data, as.numeric))
names(PA)   <- names(pred_ras)

# 4) Combine (for PA selection there are automatically NA assigned for GS columns)
points <- bind(GS, PA)

preds  <- points@data[,(ncol(points)-49):ncol(points)]
preds[,c(
  "bio02_Worldclim",
  "bio03_Worldclim",
  "bio05_Worldclim",
  "bio06_Worldclim",
  "bio08_Worldclim",
  "bio09_Worldclim",
  "bio10_Worldclim",
  "bio11_Worldclim",
  "bio15_Worldclim",
  "bio16_Worldclim",
  "bio17_Worldclim",
  "bio18_Worldclim",
  "bio19_Worldclim",
  "TMF_42",
  "TMF_60",
  "TMF_70",
  "ESA_LU_70",
  "ESA_LU_100",
  "lossyear_AVG",
  "occurrences",
  "cnfd_builtup2000"
)] <- NULL

names(preds)

# No ESA_70 (snow and ice), no ESA_100 (moss and lichen)
# Wordclim not 3, 5, 6, 8-11, 16-19


cors   <- cor(preds, method = "pearson")
r2 <- cors^2
r2_07 <- r2
r2_07[r2_07 >= 0.7] <- 1
r2_07[r2_07 < 0.7] <- 0

corrplot(cors, type="upper")
corrplot(r2_07, type="upper")

d
d
write.csv(cors, "../Data/0_Temp/correlations.csv", row.names=T)









