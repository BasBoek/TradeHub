# Bastiaen Boekelo, June 2021
# Goal: merge output results of GEE and merge with Congo Basin grid

rm(list=ls())  # Clean script <- clean mind

# Set Script and Data wd
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # Works if you have a recent version of RStudio.

library(data.table)
library(rgdal)
library(raster)
source("functions/merge_dfs_30_Classification.R")

# sort_df: Putting a series of columns in front of the table
sort_df <- function(data, colnames){
  for(i in length(colnames):1){
    if(i == length(colnames)){
      new_df <- data[,c(which(colnames(data)==colnames[i]), which(colnames(data)!=colnames[i]))]
    } else {      new_df <- new_df[,c(which(colnames(new_df)==colnames[i]), which(colnames(new_df)!=colnames[i]))]    }}
  return(new_df)
}


# Load all pixel stats
sums         <- read.csv("../Data/2_Intermediate/OilPalmMap/Pixel_sum.csv",stringsAsFactors = F)
sums         <- sums[,c("id", "sum")]
names(sums)  <- c("id", "sum")

# Load grid
grid <- readOGR("../Data/1_Input", "Grid_5000_chunks")

# Load and merge pixel statistics created by GEE
df   <- merge_dfs("../Data/2_Intermediate/OilPalmMap", "OilPalmClass", "../Data/3_Output", "OilPalmClass.csv", "id", "sum")

# Making consistent (and shapefile readable )
allnames        <- names(df)
allnames        <- gsub("OilPalmClass", "OPClass", allnames)
names(df)       <- allnames

df              <- df[ , order(names(df))]
df              <- merge(df, sums, by="id")
df              <- sort_df(df, c("id", "sum"))


# Calculate area in hectare for every grid cell
df[,3:ncol(df)] <- round(df[,3:ncol(df)] / df$sum * 25000000, 0) # Pixel area in m2
df[,3:ncol(df)] <- sapply(df[3:ncol(df)], as.integer)                   # Store as integer
df$OP_area <- df$OPClass_1 + df$OPClass_2

# Merge and write to file
shp <- merge(grid, df, by="id", all.x=T)
writeOGR(shp, "../Data/3_Output", "OilPalmMap_m2", driver = "ESRI Shapefile", overwrite_layer = TRUE)



