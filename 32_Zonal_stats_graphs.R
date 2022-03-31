rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # Works if you have a recent version of RStudio.

library(rgdal)
library(raster)
library(rgeos)

# Read statistics
t1 <- read.csv("../Data/3_Output/Forlos_Hansen.csv", stringsAsFactors = F)
t2 <- read.csv("../Data/3_Output/Burned_Area.csv", stringsAsFactors = F)
t3 <- read.csv("../Data/3_Output/Forobs_TMF.csv", stringsAsFactors = F)
t4 <- read.csv("../Data/3_Output/Area_1km_to_road.csv", stringsAsFactors = F)
t4$Road_area <- t4$POLY_AREA 
t4 <- t4[,c("id", "Road_area")]
t5 <- read.csv("../Data/3_Output/Average_distance_to_1km_road_area.csv", stringsAsFactors = F)
t5$Road_area_dist <- t5$MEAN
t5$id <- t5$ID
t5 <- t5[,c("id", "Road_area_dist")]

df <- merge(t1, t2, all=T)
df <- merge(df, t3, all=T)
df <- merge(df, t4, all=T)
df <- merge(df, t5, all=T)

# Make spatial again
grid    <- readOGR("../Data/1_Input", "Grid_5000_chunks")
grid_df <- merge(grid, df, by="id", all.x=T)
centers <- as.data.frame(gCentroid(grid,byid=TRUE))
grid_df <- cbind(grid_df, centers)

# Read likelihood
ras               <- raster("../Data/2_Intermediate/31_Potential_forob_OP_v16.tif")
points            <- as.data.frame(rasterToPoints(ras, fun=NULL, spatial=FALSE))
points            <- SpatialPointsDataFrame(coords = points[,c("x", "y")], data=points)
points$likelihood <- points$X31_Potential_forob_OP_v16
points            <- points[,(names(points) %in% c("x", "y", "likelihood"))]

# Combine likelihood with spatial variables
data              <- merge(grid_df@data, points@data, by=c("x" ,"y"))

# GRAPHS!

plot(data$Road_area_dist, data$likelihood)

test <- data[data$likelihood > 0.35,]
plot(test$Road_area, test$likelihood)





