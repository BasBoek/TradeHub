import geopandas as pd
import rtree
from datetime import datetime

wd = "C:\\D\\STACK\Admin\\Werk\\2018-XXXX PPS\\PROJECTS\\TRADE\\Data\\"

# Load data
print(str(datetime.now().time()) + " Load shapes")
grids          = pd.read_file(wd + "1_Input\\Grid_5000_chunks.shp")
grids          = grids.to_crs("EPSG:4326") 
#congo_basin    = pd.read_file(wd + "Congo_Basin.shp")
#congo_basin    = congo_basin.to_crs("EPSG:4326") 

intact_forest1 = pd.read_file(wd + "1_Input\\ifl_change_2000_2013.gpkg")

# Clip
#print(str(datetime.now().time()) + " Perform clip")
#intact_forest2 = pd.clip(intact_forest1, congo_basin)
#intact_forest2.to_file(wd + "0_Temp\\06_intact_forest_clip.shp")

# Intersect & dissolve
print(str(datetime.now().time()) + " Perform intersect")
intact_forest3 = pd.overlay(grids, intact_forest1, how='intersection')
intact_forest4 = intact_forest3.dissolve(by="id")

# Reproject
print(str(datetime.now().time()) + " Perform reproject")
intact_forest5 = intact_forest4.to_crs("EPSG:32735")

intact_forest5["IntFor_m2"] = intact_forest5['geometry'].area


# Write data
print(str(datetime.now().time()) + " Write intersection")
intact_forest5.to_file(wd + "3_Output\\06_Grid_intact_forest.shp")

