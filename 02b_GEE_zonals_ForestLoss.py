import ee
from ee import batch

print("initialize")
#ee.Authenticate() # Run first time!
ee.Initialize()

forloss1 = ee.Image('UMD/hansen/global_forest_change_2017_v1_5')       # Multiple images (collection)

chunk_list = [3,4,5,6,11,12,13,14,15,16,19,20,21,22,23,24,25,29,30,31,32,33,34,35,38,39,40,41,42,43,44,47,48,49,50,51,52,53,54,57,58,59,60,61,62,63,67,68,69,70,71,72]

years = list(range(1,18))
YEAR = 1

#############################################
# Calculate the total pixels per grid cell 
#############################################


YR = str(YEAR + 2000)

# Select Year and band 'BurnDate' from Image
forloss2 = forloss1.select('lossyear')   # Select band

# Unmask pixels
forloss3 = forloss2.unmask(0) # NULL pixels set to 0
forloss4 = forloss3.gt(-1)    # All  pixels set to 1

grids = ee.FeatureCollection('users/BasBoek90/Grid_5000')

for i in range(len(chunk_list)):

    CHUNK = chunk_list[i]

    # Select grid chunk
    sel = ee.Filter.inList('id_350km', [CHUNK])
    grids_sel = grids.filter(sel)

    # Zonal statistics
    sums = forloss4.reduceRegions(collection = grids_sel, reducer = ee.Reducer.sum())

    if i == 0:
        all_grids = sums
    else:
        all_grids = all_grids.merge(sums)

def multiplier(feature):
    pix_x1000 = ee.Number(feature.get('sum')).multiply(1000).round()
    return feature.set('pix_x1000', pix_x1000)

forloss1000 = all_grids.map(multiplier)

# Export to Google Drive folder
print('export')
ExportTableTask = ee.batch.Export.table.toDrive(
        collection = forloss1000,             
        description = 'Pixel_sum',
        folder = 'LossyearPixels',
        fileFormat = 'CSV',
        selectors = ['id, id_350km, id_50km, pix_x1000']
        )
ExportTableTask.start()


#############################################
# Calculate per year / chunk the number of pixels inside a 5km*5km area
#############################################

##for YEAR in years:
##
##    YR = str(YEAR + 2000)
##
##    # Select Year and band 'BurnDate' from Image
##    forloss2 = forloss1.select('lossyear')   # Select band
##
##    # Select only pixels in a given year
##    forloss3 = forloss2.eq(YEAR)             # e.g. 5 means 2005
##
##    grids = ee.FeatureCollection('users/BasBoek90/Grid_5000')
##
##    for i in range(len(chunk_list)):
##
##        CHUNK = chunk_list[i]
##
##        # Select grid chunk
##        sel = ee.Filter.inList('id_350km', [CHUNK])
##        grids_sel = grids.filter(sel)
##
##        # Zonal statistics
##        sums = forloss3.reduceRegions(collection = grids_sel, reducer = ee.Reducer.sum())
##
##        if i == 0:
##            all_grids = sums
##        else:
##            all_grids = all_grids.merge(sums)
##
##    # Select only grids containing forest loss pixels and make it integer
##    all_grids_forloss = all_grids.filter(ee.Filter.gt('sum', 0))
##
##    def multiplier(feature):
##        pix_x1000 = ee.Number(feature.get('sum')).multiply(1000).round()
##        return feature.set('pix_x1000', pix_x1000)
##
##    forloss1000 = all_grids_forloss.map(multiplier)
##
##    # Export to Google Drive folder
##    print('export ' + YR)
##    ExportTableTask = ee.batch.Export.table.toDrive(
##            collection = forloss1000,             
##            description = 'Lossyear_' + YR,
##            folder = 'LossyearPixels',
##            fileFormat = 'CSV',
##            selectors = ['id, id_350km, id_50km, pix_x1000']
##            )
##    ExportTableTask.start()

    












































