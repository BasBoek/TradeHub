import ee
from ee import batch

print("initialize")
#ee.Authenticate() # Run first time!
ee.Initialize()

burns0     = ee.ImageCollection('MODIS/006/MCD64A1')       # Multiple images (collection)

chunk_list = [3,4,5,6,11,12,13,14,15,16,19,20,21,22,23,24,25,29,30,31,32,33,34,35,38,39,40,41,42,43,44,47,48,49,50,51,52,53,54,57,58,59,60,61,62,63,67,68,69,70,71,72]

#############################################
# Calculate the total pixels per grid cell 
#############################################

# Select Year and band 'BurnDate' from ImageCollection
burns1   = burns0.filterDate('2001-01-01', '2001-12-31')   # Random year
burns2   = burns1.select('BurnDate')                       # Select band

# Select Single image from ImageCollection
img_list = burns2.toList(burns2.size())

burns3   = ee.Image(img_list.get(0)) # Random month (here January)

burns4   = burns3.unmask(0) # NULL pixels set to 0
burns5   = burns4.gt(-1)    # All  pixels set to 1

grids    = ee.FeatureCollection('users/BasBoek90/Grid_5000')

for i in range(len(chunk_list)):

    CHUNK = chunk_list[i]

    # Select grid chunk
    sel       = ee.Filter.inList('id_350km', [CHUNK])
    grids_sel = grids.filter(sel)

    # Zonal statistics
    sums = burns5.reduceRegions(collection = grids_sel, reducer = ee.Reducer.sum())

    if i == 0:
        all_grids = sums
    else:
        all_grids = all_grids.merge(sums)

def multiplier(feature):
    pix_x1000 = ee.Number(feature.get('sum')).multiply(1000).round()
    return feature.set('pix_x1000', pix_x1000)

burned1000 = all_grids.map(multiplier)

# Export to Google Drive folder
print('export')
ExportTableTask = ee.batch.Export.table.toDrive(
        collection = burned1000,             
        description = 'Pixel_sum',
        folder = 'BurnedPixels',
        fileFormat = 'CSV',
        selectors = ['id, id_350km, id_50km, pix_x1000']
        )
ExportTableTask.start()


#############################################
# Calculate per month / year / chunk the number of pixels inside a 5km*5km area
#############################################

years  = list(range(2001,2021))
months = list(range(0,12))

for YEAR in years:
    for MONTH in months:

        YR = str(YEAR)
        MT = str(MONTH + 1)

        # Select Year and band 'BurnDate' from ImageCollection
        burns1 = burns0.filterDate(YR + '-01-01', YR + '-12-31') # Define time range
        burns2 = burns1.select('BurnDate')                       # Select band

        # Select Single image from ImageCollection
        img_list = burns2.toList(burns2.size())

        burns3 = ee.Image(img_list.get(MONTH)) # Should be 12 layers of 12 months, 0 = January
        burns4 = burns3.gt(0) # Transform to binary

        grids = ee.FeatureCollection('users/BasBoek90/Grid_5000')

        for i in range(len(chunk_list)):

            CHUNK = chunk_list[i]

            # Select grid chunk
            sel = ee.Filter.inList('id_350km', [CHUNK])
            grids_sel = grids.filter(sel)

            # Zonal statistics
            sums = burns4.reduceRegions(collection = grids_sel, reducer = ee.Reducer.sum())

            if i == 0:
                all_grids = sums
            else:
                all_grids = all_grids.merge(sums)

        # Select only grids containing burned area pixels and make it integer
        all_grids_burned = all_grids.filter(ee.Filter.gt('sum', 0))

        def multiplier(feature):
            pix_x1000 = ee.Number(feature.get('sum')).multiply(1000).round()
            return feature.set('pix_x1000', pix_x1000)

        burned1000 = all_grids_burned.map(multiplier)

        # Export to Google Drive folder
        print('export' + YR + '-' + MT)
        ExportTableTask = ee.batch.Export.table.toDrive(
                collection = burned1000,             
                description = 'Burned_' + YR + '_' + MT,
                folder = 'BurnedPixels',
                fileFormat = 'CSV',
                selectors = ['id, id_350km, id_50km, pix_x1000']
                )
        ExportTableTask.start()














































