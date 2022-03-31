# Mapping Oil Palm - Trade Hub
# Create a sampling plan based for the Congo Basin, based on a stratification
# Bastiaen Boekelo, November 2021

rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # Works if you have a recent version of RStudio.

library(raster)
library(rgdal)
library(RStoolbox)
library(sf)
library(dplyr)

set.seed(1000000)

# Load relative likelihood approximation
ras                <- raster("https://drive.google.com/uc?id=1le3kKgkJ6VktlQ-I69bvBsnUNLrd3MHY&export=download")
values             <- as.data.frame(rasterToPoints(ras))
names(values)      <- c("x", "y", "layer")
values$stratum     <- NA

# Create strata based on the relative likelihood of finding oil palm
values$stratum[values$layer >= 0.125]                        <- 1
values$stratum[values$layer >= 0.100 & values$layer < 0.125] <- 2
values$stratum[values$layer >= 0.075 & values$layer < 0.100] <- 3
values$stratum[values$layer >= 0.000 & values$layer < 0.075] <- 4
strata_size                                                  <- as.vector(table(values$stratum))

# Set the point coordinates to South-West corner of the 5*5 km grids
values                               <- merge(values, train_locs, by=c("x","y"), all.x=T)
values$stratum[values$stratum1 == 1] <- 1
values$stratum1                      <- NULL
values$x                             <- values$x - 2500
values$y                             <- values$y - 2500

# Create function that randomly assigns centerpoint in a 5000*5000 square
create_coordinates <- function(df, STR_NR, REPS, COOR_BAG){
  
  COOR_BAG             <- expand.grid(a = 1:20, b = 1:20) * 250 - 125
  
  stratum              <- df[df$stratum == STR_NR,]
  val_reps             <- do.call("rbind", replicate(REPS, stratum, simplify = FALSE))
  val_reps             <- val_reps[order(val_reps$layer),]
  
  for(i in 0:nrow(stratum)){
    ROWS <- (i*REPS+1):((i+1)*REPS)
    
    SAMP <- sample_n(COOR_BAG, REPS, replace = F)
    
    val_reps[ROWS,c("x","y")] <- val_reps[ROWS,c("x","y")] + SAMP
    print(i)
  }
  return(val_reps)
}

# How many coordinates to create per stratum
nr_points1 <- 40000
nr_points2 <- 20000
nr_points3 <- 20000
nr_points4 <- 20000

# Create and combine coordinates
coords_1 <- create_coordinates(values, 1, ceiling(nr_points1 / strata_size[1]),  COOR_BAG)
coords_2 <- create_coordinates(values, 2, ceiling(nr_points2 / strata_size[2]),  COOR_BAG)
coords_3 <- create_coordinates(values, 3, ceiling(nr_points3 / strata_size[3]),  COOR_BAG)
sel_str4 <- sample_n(values[values$stratum==4,], 15000, replace = F) # Create sample of 5*5km grid to reduce calculating time
coords_4 <- create_coordinates(sel_str4, 4, 2,  COOR_BAG)

coords   <- rbind(
  sample_n(coords_1, nr_points1, replace = F),
  sample_n(coords_2, nr_points2, replace = F),
  sample_n(coords_3, nr_points3, replace = F),
  sample_n(coords_4, nr_points4, replace = F)
)
coords <- coords[sample(nrow(coords)),]     # randomize order of coordinates
coords <- coords[complete.cases(coords), ]  # remove doubles

# Convert and write to file
points <- SpatialPointsDataFrame(coords = coords[,c("x","y")], data = coords[,c("stratum", "layer")], proj4string = CRS("+proj=utm +zone=35 +south +datum=WGS84 +units=m +no_defs"))
writeOGR(points, "sample_points", "sample_points20", driver="ESRI Shapefile", overwrite_layer=TRUE)


