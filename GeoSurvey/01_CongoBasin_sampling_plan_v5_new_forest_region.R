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
ras                <- raster("Strata2.tif")
ras[ras < 41]      <- 0
ras[ras == 45]      <- 0
plot(ras)
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
nr_points1 <- 20275 # over 4055   grids cells =  points / cell 50%
nr_points2 <- 8500  # over 3399   grids cells =  points / cell 25%
nr_points3 <- 4120  # over 2753   grids cells =  points / cell 15%
nr_points4 <- 1835  # over 1831   grids cells =  points / cell 10%

# Nu 22% van 116000 = 25520
# plus 10% van 33760 =  3376

# Create and combine coordinates
coords_1 <- create_coordinates(values, 41, ceiling(nr_points1 / strata_size[2]))
coords_2 <- create_coordinates(values, 42, ceiling(nr_points2 / strata_size[3]))
coords_3 <- create_coordinates(values, 43, ceiling(nr_points3 / strata_size[4]))
coords_4 <- create_coordinates(values, 44, ceiling(nr_points4 / strata_size[5]))

coords   <- rbind(
  sample_n(coords_1, nr_points1, replace = F),
  sample_n(coords_2, nr_points2, replace = F),
  sample_n(coords_3, nr_points3, replace = F),
  sample_n(coords_4, nr_points4, replace = F)
)
coords <- coords[sample(nrow(coords)),]     # randomize order of coordinates
coords <- coords[complete.cases(coords), ]  # remove doubles



# Convert to points
points <- SpatialPointsDataFrame(coords = coords[,c("x","y")], data = coords[,c("x", "y", "stratum")], proj4string = CRS("+proj=utm +zone=35 +south +datum=WGS84 +units=m +no_defs"))

# Convert to GEE suitable  
epsg_3857  <- "+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext  +no_defs"
points3857 <- spTransform(points, epsg_3857)

# Round the coordinates, assign them to the points and remove doubles
new_coords        <- as.data.frame(points3857@coords)
new_coords$x      <- round(new_coords$x / 250) * 250 - 125
new_coords$y      <- round(new_coords$y / 250) * 250 - 125
new_coords        <- as.matrix(new_coords)
points3857@coords <- new_coords
points3857        <- points3857[!duplicated(points3857@coords),]
#points3857        <- points3857[sample(nrow(points3857), 100000), ]

# Write points
writeOGR(points3857, ".", "sample_points32_new_strata", driver="ESRI Shapefile", overwrite_layer=TRUE)

# Also to WGS84 for GeoSurvey
pointsWGS         <- spTransform(points3857, "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
writeOGR(pointsWGS, ".", "sample_points32_WGS84_new_strata", driver="ESRI Shapefile", overwrite_layer=TRUE)

# Write csv file of coordinates
csv_coords     <- as.data.frame(pointsWGS@coords)
csv_coords$lat <- csv_coords[,2]
csv_coords$lon <- csv_coords[,1]
csv_coords[,c("x","y")] <- NULL
write.csv(csv_coords, "sample_points_32_WGS_new_strata.csv", row.names = F, col.names = F)

