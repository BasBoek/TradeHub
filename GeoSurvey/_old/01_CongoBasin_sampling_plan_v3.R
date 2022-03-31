# Mapping Oil Palm - Trade Hub
# Balanced Acceptance Sampling plan for oil palm classification in the Congo Basin, based on unequal probability stratification (oil palm suspected areas sampled more)
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
ras                <- raster("https://drive.google.com/uc?id=1qifFZeqzvplrgbzwGRtZ-z03k48qfpmv&export=download")
values             <- as.data.frame(rasterToPoints(ras))
names(values)      <- c("x", "y", "stratum")
strata_size        <- as.vector(table(values$stratum))
strata_size

# Print strata size per country
countries    <- readOGR("../../Data/1_Input", "Countries")
ISOs         <- countries$ISO
for(i in 1:length(ISOs)){
  ISO        <- ISOs[i]
  country    <- countries[countries$ISO == ISO,]
  ras_iso    <- mask(ras, country)
  print(ISO)
  print(freq(ras_iso))
}


# Set the point coordinates to South-West corner of the 5*5 km grids
values$x                             <- values$x - 2500
values$y                             <- values$y - 2500


# Create function that randomly assigns centerpoint in a 5000*5000 square
create_coordinates <- function(df, STRATUM_NR, REPS){
  
  COOR_BAG       <- expand.grid(a = 1:20, b = 1:20) * 250 - 125
  
  stratum        <- df[df$stratum == STRATUM_NR,]
  val_reps       <- do.call("rbind", replicate(REPS, stratum, simplify = FALSE))
  val_reps$xy    <- paste(val_reps$x, val_reps$y)
  val_reps       <- val_reps[ order( val_reps[,"xy"] ), ]
  val_reps$xy    <- NULL

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
coords_1 <- create_coordinates(values, 1, ceiling(nr_points1 / strata_size[1]))
coords_2 <- create_coordinates(values, 2, ceiling(nr_points2 / strata_size[2]))
coords_3 <- create_coordinates(values, 3, ceiling(nr_points3 / strata_size[3]))
sel_str4 <- sample_n(values[values$stratum==4,], 15000, replace = F) # Create sample of 5*5km grid to reduce calculating time
coords_4 <- create_coordinates(sel_str4, 4, 2)

coords   <- rbind(
  sample_n(coords_1, nr_points1, replace = F),
  sample_n(coords_2, nr_points2, replace = F),
  sample_n(coords_3, nr_points3, replace = F),
  sample_n(coords_4, nr_points4, replace = F)
)
coords <- coords[sample(nrow(coords)),]     # randomize order of coordinates
coords <- coords[complete.cases(coords), ]  # remove doubles

# Convert and write to file
points <- SpatialPointsDataFrame(coords = coords[,c("x","y")], data = coords[,c("x", "y", "stratum")], proj4string = CRS("+proj=utm +zone=35 +south +datum=WGS84 +units=m +no_defs"))
writeOGR(points, ".", "sample_points23", driver="ESRI Shapefile", overwrite_layer=TRUE)

pointsWGS <- spTransform(points, "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
writeOGR(pointsWGS, ".", "sample_points23_WGS84", driver="ESRI Shapefile", overwrite_layer=TRUE)


