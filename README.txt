Bastiaen Boekelo, 
March 2022

The scripts in these folders show the workflow performed to derive different results. The scripts can be divided into different parts:


	PART 1: FROM GOOGLE EARTH ENGINE TO STRATIFICATION


02):
Export datasets as .csv from the Google Earth Engine to a Google Drive folder:
	- MODIS Burned Area product
	- HANSEN forest loss datasets
	- TMF Forest observations
	
03 - 06):
Preprocess GEE products:
	- Combine (manually) downloaded csv's and store into 5000m grid shapefile
	- Convert the shapefile into raster
	- Brick the rasters and store in 3_Output folders

20):
NOT USED

30 - 32):
Calculating (PCA) statistics from rasters and define relative likelihood and stratification.
Script 31 is very important, since it creates the likelihood raster used for strata determination:
	Relative likelihood: 	variable = potentials2, written to "../Data/2_Intermediate/31_Potential_forob_OP_v16.tif"
	Strata: 				"../Data/2_Intermediate/31_strata.tif"


	PART 2: PREPARING VARIABLES FOR GEOSURVEY


40):
NOT USED. 
We want to resample chosen-to-be-relevant environmental variables to 250m resolution scale (for which GeoSurvey was performed).
For pragmatic reasons it was chosen to do this in the GEE environment itself (see 41).

41): 
Copy of the GEE script creating the Congo Basin environmental variables at a 250m scale.
Scripts and Assets can be shared if necessary via a share link.

49):
Creating correlation matrix of a sample of the 50 variables created in the GEE. 
Variables were considered cocorrelated with r2 > 0.7
Remaining 29 variables listed here.

50):
Script performing the modelling and prediction procedures using species distribution modelling approach.


 