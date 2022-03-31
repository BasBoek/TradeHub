# Bastiaen Boekelo, June 2021
# Goal: merge output results of GEE and merge with Congo Basin grid

rm(list=ls())  # Clean script <- clean mind

# Set Script and Data wd
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # Works if you have a recent version of RStudio.

library(data.table)
library(rgdal)
library(raster)
source("functions/merge_dfs.R")

# sort_df: Putting a series of columns in front of the table
sort_df <- function(data, colnames){
  for(i in length(colnames):1){
    if(i == length(colnames)){
      new_df <- data[,c(which(colnames(data)==colnames[i]), which(colnames(data)!=colnames[i]))]
    } else {      new_df <- new_df[,c(which(colnames(new_df)==colnames[i]), which(colnames(new_df)!=colnames[i]))]    }}
  return(new_df)
}


# Load all pixel stats
sums         <- read.csv("../Data/2_Intermediate/Burned_Area/Pixel_sum.csv",stringsAsFactors = F)
sums         <- sums[,c("id", "pix_x1000")]
names(sums)  <- c("id", "sums_x1000")

# Load grid
grid <- readOGR("D:/TRADE/1_Input/1_Raw/Grids", "Grid_5000_chunks")

# Load and merge pixel statistics created by GEE
df <- merge_dfs("../Data/2_Intermediate/Burned_Area", "Burned_", "../Data/3_Output", "Burned_Area.csv")

# Making consistant (and shapefile readable )
allnames        <- names(df)
allnames        <- gsub("Burned_", "Bu_", allnames)

split1          <- substr(allnames, 1,8)
split2          <- substr(allnames, 9,10)
split2[nchar(split2) == 1] <- paste0("0", split2[nchar(split2) == 1])
allnames        <- paste0(split1, split2)
names(df)       <- allnames

df              <- df[ , order(names(df))]
df              <- merge(df, sums, by="id")
df              <- sort_df(df, c("id", "sums_x1000"))

# Calculate area in hectare for every grid cell
df[,3:ncol(df)] <- round(df[,3:ncol(df)] / df$sums_x1000 * 2500, 0) # Pixel area in ha
df[,1:ncol(df)] <- sapply(df[1:ncol(df)], as.integer)               # Store as integer


# Merge and write to file
shp <- merge(grid, df, by="id", all.x=T)
writeOGR(shp, "../Data/3_Output", "Burned_Areas_ha", driver = "ESRI Shapefile", overwrite_layer = TRUE)



