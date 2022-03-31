import ee
from ee import batch

print("initialize")
#ee.Authenticate() # Run first time!
ee.Initialize()

proj = ee.Projection('EPSG:32735')

palm1 = ee.Image('users/BasBoek90/OilPalm_Map')

chunk_list = [3,4,5,6,11,12,13,14,15,16,19,20,21,22,23,24,25,29,30,31,32,33,34,35,38,39,40,41,42,43,44,47,48,49,50,51,52,53,54,57,58,59,60,61,62,63,67,68,69,70,71,72]

classes = [1,2]
CLASS   = 1

#############################################
# Calculate the total pixels per grid cell 
#############################################


# Unmask pixels
palm2 = palm1.gt(-1) # All pixels set to 1

grids = ee.FeatureCollection('users/BasBoek90/Grid_5000')

for i in range(len(chunk_list)):

    CHUNK = chunk_list[i]

    # Select grid chunk
    sel       = ee.Filter.inList('id_350km', [CHUNK])
    grids_sel = grids.filter(sel)

    # Zonal statistics
    sums = palm2.reduceRegions(collection = grids_sel, reducer = ee.Reducer.sum())

    if i == 0:
        all_grids = sums
    else:
        all_grids = all_grids.merge(sums)

##def multiplier(feature):
##    pix_x1000 = ee.Number(feature.get('sum')).multiply(1000).round()
##    return feature.set('pix_x1000', pix_x1000)
##
##forobs1000 = all_grids.map(multiplier)

# Export to Google Drive folder
print('export')
ExportTableTask = ee.batch.Export.table.toDrive(
        collection = all_grids,             
        description = 'Pixel_sum',
        folder = 'OilPalm',
        fileFormat = 'CSV',
        selectors = ['id, id_350km, id_50km, sum']
        )
ExportTableTask.start()


#############################################
# Calculate per class / chunk the number of pixels inside a 5km * 5km area
#############################################

for CLASS in classes:

    # Select a class
    palm2 = palm1.eq(CLASS) 

    grids = ee.FeatureCollection('users/BasBoek90/Grid_5000')

    for i in range(len(chunk_list)):

        CHUNK = chunk_list[i]

        # Select grid chunk
        sel       = ee.Filter.inList('id_350km', [CHUNK])
        grids_sel = grids.filter(sel)

        # Zonal statistics
        sums = palm2.reduceRegions(collection = grids_sel, reducer = ee.Reducer.sum())

        if i == 0:
            all_grids = sums
        else:
            all_grids = all_grids.merge(sums)

    # Select only grids with > 0 pixels and make it integer
    all_grids_palms = all_grids.filter(ee.Filter.gt('sum', 0))

##    def multiplier(feature):
##        pix_x1000 = ee.Number(feature.get('sum')).multiply(1000).round()
##        return feature.set('pix_x1000', pix_x1000)
##
##    forobs1000 = all_grids_forobs.map(multiplier)

    # Export to Google Drive folder
    print('export ' + str(CLASS))
    ExportTableTask = ee.batch.Export.table.toDrive(
            collection = all_grids_palms,             
            description = 'OilPalmClass_' + str(CLASS),
            folder = 'OilPalm',
            fileFormat = 'CSV',
            selectors = ['id, id_350km, id_50km, sum']
            )
    ExportTableTask.start()

    












































