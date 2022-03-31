import ee
from ee import batch
import numpy as np

print("initialize")
#ee.Authenticate() # Run first time!
ee.Initialize()

# Read and create data

Congo_Basin = ee.FeatureCollection('users/BasBoek90/Congo_Basin')
grid_250m   = ee.Image('users/BasBoek90/CB_250m')

forobs1 = ee.Image('UMD/hansen/global_forest_change_2019_v1_7').select('lossyear')     # Multiple images (collection)
#forobs2 = forobs1.gt(-1) # Unmask pixels: all pixels set to 1

#classes = [10,11,12,21,22,23,24,25,26,31,32,33,41,42,51,52,53,54,61,62,63,64,65,66,67,71,72,73,74,81,82,83,84,85,86,91,92,93,94]
#classes = [10,11]#CLASS   = 10

lijst = np.arange(1, 1001000, 5000).tolist()
#lijst = [1, 5000]
region_big    = ee.Geometry.Polygon([  [-60,40],[-60,-40],[60,-40],[60,40]  ])
#region_manual = ee.Geometry.Polygon([  [8.4001,4.6650],[8.4001,4.6752],[8.4203,4.6752],[8.4203,4.6650] ] )


#############################################
# Calculate the total pixels per grid cell 
#############################################



pixels = ee.FeatureCollection('users/BasBoek90/CB_250m')


for i in range(len(lijst)-1):

    # Make selection of pixels
    start = lijst[i]
    stop  = lijst[i+1]
    sel1  = grid_250m.gt(start).And(grid_250m.lt(stop));
    sel2  = grid_250m.updateMask(sel1)
    sel3  = sel2.selfMask()

    # Convert to points and create bounding box
    points = sel3.sample(region = region_big, geometries=True)
    bbox   = points.geometry().bounds().buffer(500)

    # Create input for vector conversion and perform it
    integers  = sel3.toInt32()
    red_in    = integers.addBands(sel3)
    vectors   = red_in.reduceToVectors(geometry = bbox, geometryType = 'polygon', labelProperty = 'b1', reducer = ee.Reducer.mean() )

    # Shrink vectors with 1 cm in order to prevent reprojection / export problems
    def shrink_features(a_feature):
        shrunk = a_feature.buffer(-0.01)
        return shrunk

    vectors_shrunk = vectors.map(shrink_features);

    # Zonal statistics & back to raster
    zonal_vec = forobs1.reduceRegions(collection = vectors_shrunk, reducer = ee.Reducer.mean())
    zonal_ras = zonal_vec.reduceToImage(properties = ['mean'], reducer = ee.Reducer.first() )

    if i == 0:
        all_pixels = zonal_ras
    else:
        all_pixels = ee.ImageCollection([all_pixels, zonal_ras]).mosaic()
       

## For completeness, this is the iterate() way:
##def mergeBands(image, previous):
##    new = ee.Image(previous).addBands(image)
##    return new
##
##img = all_pixels.iterate(mergeBands, ee.Image([]))
##PROJ = all_pixels.projection()

# Reproject before export prevents wrong projections in export
all_pixels_prj = all_pixels.reproject(crs = ee.Projection('EPSG:3857'), scale = 250)


# Export to Google Drive folder
print('export')
ExportTableTask = ee.batch.Export.image.toDrive(
        image       = all_pixels_prj,             
        description = 'test_image31_100fold_big_region',
        scale       = 250,
        folder      = 'TEST',
        fileFormat  = 'GeoTIFF',
        #crs         = ee.Projection('EPSG:3857'),
        region      = Congo_Basin.geometry(),
        maxPixels   = 200000000
        )
ExportTableTask.start()


###############################################
### Calculate per class / chunk the number of pixels inside a 5km * 5km area
###############################################
##
##for CLASS in classes:
##
##    # Select a class
##    forobs3 = forobs2.eq(CLASS) 
##
##    grids = ee.FeatureCollection('users/BasBoek90/Grid_5000')
##
##    for i in range(len(chunk_list)):
##
##        CHUNK = chunk_list[i]
##
##        # Select grid chunk
##        sel       = ee.Filter.inList('id_350km', [CHUNK])
##        grids_sel = grids.filter(sel)
##
##        # Zonal statistics
##        sums = forobs3.reduceRegions(collection = grids_sel, reducer = ee.Reducer.sum())
##
##        if i == 0:
##            all_grids = sums
##        else:
##            all_grids = all_grids.merge(sums)
##
##    # Select only grids with > 0 pixels and make it integer
##    all_grids_forobs = all_grids.filter(ee.Filter.gt('sum', 0))
##
##    def multiplier(feature):
##        pix_x1000 = ee.Number(feature.get('sum')).multiply(1000).round()
##        return feature.set('pix_x1000', pix_x1000)
##
##    forobs1000 = all_grids_forobs.map(multiplier)
##
##    # Export to Google Drive folder
##    print('export ' + str(CLASS))
##    ExportTableTask = ee.batch.Export.table.toDrive(
##            collection = forobs1000,             
##            description = 'Forobs_' + str(CLASS),
##            folder = 'TMF_Forobs',
##            fileFormat = 'CSV',
##            selectors = ['id, id_350km, id_50km, pix_x1000']
##            )
##    ExportTableTask.start()

    












































