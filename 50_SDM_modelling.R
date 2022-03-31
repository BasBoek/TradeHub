
# load libraries
rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(biomod2)
library(raster)
library(rgdal)
#source("R/Species_Modelling/Write_SDM_Rasters.R")

SP_name       <- "oilpalm"

rasname       <- "CMR1"

####################################################
### 1 - LOAD DATA & PRE-PROCESS
####################################################

# What variables not to include in the model (cocorrelated r2 > 0.7)
VARS_TO_EXCLUDE <- c(
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
)

# 1) Prediction rasters & study area
##############################################

pred_loc01   <- "../Data/1_Input/Predictors_250m/Stacked/3rd export/CB_preds_01_10.tif"
pred_loc02   <- "../Data/1_Input/Predictors_250m/Stacked/3rd export/CB_preds_11_20.tif"
pred_loc03   <- "../Data/1_Input/Predictors_250m/Stacked/3rd export/CB_preds_21_30.tif"
pred_loc04   <- "../Data/1_Input/Predictors_250m/Stacked/3rd export/CB_preds_31_40.tif"
pred_loc05   <- "../Data/1_Input/Predictors_250m/Stacked/3rd export/CB_preds_41_50.tif"

pred_ras     <- stack(stack(pred_loc01), 
                  stack(pred_loc02),
                  stack(pred_loc03),
                  stack(pred_loc04),
                  stack(pred_loc05))

ALL_VARS <- names(pred_ras)

mask_area    <- extent(pred_ras)

# Projection raster layer, subarea
ras_proj        <- stack(paste0("../Data/1_Input/Predictors_250m/Stacked/3rd export/CB_50preds_", rasname, ".tif"))
VARS_TO_INCLUDE <- setdiff(ALL_VARS, VARS_TO_EXCLUDE)
ras_proj        <- subset(ras_proj, VARS_TO_INCLUDE)
VARS_TO_INCLUDE

# # Entire area
# ras_proj        <- stack(paste0("../Data/1_Input/Predictors_250m/Stacked/3rd export/", rasname, ".tif"))
# VARS_TO_INCLUDE <- setdiff(ALL_VARS, VARS_TO_EXCLUDE)
# names(ras_proj) <- VARS_TO_INCLUDE

# 2) Point data (GeoSurvey + Pseudo-absences)
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

# 5) Remove co-correlated variables 
##############################################################

points@data[, VARS_TO_EXCLUDE] <- NULL
# 
# df_eval      <- points@data[complete.cases(points@data), ]
# df_eval_expl <- df_eval[,(ncol(df_eval)-28):ncol(df_eval)]

###################################################
### 2 - CONVERT DATA INTO BIOMOD FORMAT
###################################################

Sys.time()
myBiomodData <- BIOMOD_FormatingData(expl.var       = points[,(ncol(points)-28):ncol(points)],
                                     
                                     resp.var       = points$uno_pres,
                                     resp.name      = SP_name,
                                     # 
                                     # eval.resp.var = df_eval$uno_pres,
                                     # eval.expl.var = df_eval_expl,
                                     # 
                                     PA.strategy    = 'random',
                                     PA.nb.absences = 10000, # 10000 points per time, to cover region
                                     PA.nb.rep      = 6, 

                                     )
Sys.time()


# ###################################################
# ### 3 - MODELLING
# ###################################################

Sys.time()
myBiomodOption   <- BIOMOD_ModelingOptions()

myBiomodModelOut <- BIOMOD_Modeling( 
  myBiomodData, 
  # models = c('GLM', 'GAM', 'GBM', 'ANN', 'MAXENT.Phillips.2', 'RF'), 
  # models = c('GLM', 'RF'), 
  models = c('RF'), 
  models.options = myBiomodOption, # Default is a quadratic model
  NbRunEval=1, 
  DataSplit=50,   # CHECK! 75% included for model construction, 25% for evaluation
  Prevalence=0.5, # CHECK! So presences are as important as absences
  VarImport=3,
  models.eval.meth = c('TSS', "ROC"),
  # models.eval.meth = c("ROC"), # Here must be at least the ones listed that will be used for the ensemble models as well..
  SaveObj = TRUE,
  rescal.all.models = TRUE,
  do.full.models = FALSE)
modeling.id = paste0("Model_",SP_name)
Sys.time()


# Variable importance $ evaluation metrices

var_imp <- get_variables_importance(myBiomodModelOut)
var_imp <- as.data.frame(var_imp)
var_imp$Species <- SP_name


###################################################
### 4 - PROJECTION   ############
###################################################


myBiomodProj <- BIOMOD_Projection(
  modeling.output = myBiomodModelOut,
  new.env = ras_proj, # myExpl_completearea,
  proj.name = 'current',
  selected.models = 'all',
  binary.meth = c(NULL, 'ROC'),
  compress = 'xz',
  build.clamping.mask = F,
  do.stack=T)

plot(myBiomodProj@proj@val)

######################################################################
####################### Building ensemble-models #####################
######################################################################

# How does binary transformation ROC work? Answer: http://www.sciencedirect.com/science/article/pii/S1146609X07000288
myBiomodEM <- BIOMOD_EnsembleModeling( 
  modeling.output = myBiomodModelOut,
  chosen.models = 'all',      
  em.by= 'all',                 
  # eval.metric = c('ROC', 'TSS'),         # For me, (A) to make the binary transformation needed for committee averaging computation. Also: (B) to test (and/or evaluate) your ensemble-models forecasting ability (at this step, each ensemble-model (ensemble will be evaluated according to each evaluation metric)
  # models.eval.meth = c('ROC','TSS'),
  eval.metric = c('ROC'),
  models.eval.meth = c('ROC'),
  eval.metric.quality.threshold = c(),
  prob.mean = F,
  prob.cv = T,
  prob.ci = F,  
  prob.ci.alpha = 0.05,
  prob.median = T,                      # Very similar to mean, at least for nomad_albugutta 
  committee.averaging = T,              # Average of binary predictions
  prob.mean.weight = F,
  VarImport = 0)                        # To be checked for evaluation of the variable importance, takes long time


eval <- get_evaluations(myBiomodEM)
eval <- as.data.frame(eval)
eval <- as.data.frame(t(eval))
eval$Species <- SP_name


####################################################################
###### Make ensemble-models projections on current variable ########
####################################################################

myBiomodEF <- BIOMOD_EnsembleForecasting( 
  EM.output = myBiomodEM,
  projection.output = myBiomodProj,
  binary.meth = c(NULL, 'ROC'), # Writing extra Rasters stacks. If ROC is chosen, this will contain multiple rasters (e.g. from mean, median, commitee etc) times the number of evaluation metrices chosen in myBiomodEM. Only ROC will be chosen, since optimizing ROC would be the same as optimizing the TSS (Liu et. al, 2013)
  compress = 'xz',
  clamping.mask = F,
  do.stack=T)
plot(myBiomodEF)

removeTmpFiles()

#####################################################################################
### Export ###
#####################################################################################


write.csv(var_imp, paste0("../Data/3_Output/50_SDM/SDM_varimp_", rasname, ".csv"))
write.csv(eval, paste0("../Data/3_Output/50_SDM/SDM_evaluation_", rasname, ".csv"))

writeRaster(myBiomodEF@proj@val, paste0("../Data/3_Output/50_SDM/SDM_predictions_", rasname, ".tif"))
Sys.time()
