import ee
from ee import batch

print("initialize")
#ee.Authenticate() # Run first time!
ee.Initialize()

INTACT_FORESTS = ee.FeatureCollection('users/BasBoek90/Intact_Forest')
OP_CONCESSIONS = ee.FeatureCollection('users/BasBoek90/Oil_Palm_Concessions')


# BURNS 
#############################################

burns0 = ee.ImageCollection('MODIS/006/MCD64A1')       # Multiple images (collection)

years  = list(range(2001,2021))
months = list(range(0,12))
##years  = list(range(2001,2002))
##months = list(range(0,2))

i = 0
for YEAR in years:
    for MONTH in months:

        YR = str(YEAR)
        MT = str(MONTH + 1)


        # Select Year and band 'BurnDate' from ImageCollection
        burns1 = burns0.filterDate(YR + '-01-01', YR + '-12-31') # Define time range
        burns2 = burns1.select('BurnDate')                       # Select band

        # Select Single image from ImageCollection
        img_list = burns2.toList(burns2.size())

        burns3 = ee.Image(img_list.get(MONTH)) # Should be 12 layers of 12 months, 0 = January, 11 = December
        burns4 = burns3.gt(0)                  # Transform to binary

        # Zonal statistics
        INTACT_FORESTS_burn_sums = burns4.reduceRegions(collection = INTACT_FORESTS, reducer = ee.Reducer.sum())
        OP_CONCESSIONS_burn_sums = burns4.reduceRegions(collection = OP_CONCESSIONS, reducer = ee.Reducer.sum())

        # Export to Google Drive folder
        print('export' + YR + '-' + MT)
        
        ExportTableTask = ee.batch.Export.table.toDrive(
                collection = INTACT_FORESTS_burn_sums,             
                description = 'INTFOR__Burned_' + YR + '_' + MT,
                folder = 'NonGrid_Zonals',
                fileFormat = 'CSV',
                selectors = ['fid, sum']
                )
        ExportTableTask.start()

        ExportTableTask = ee.batch.Export.table.toDrive(
                collection = OP_CONCESSIONS_burn_sums,             
                description = 'OPCON__Burned_' + YR + '_' + MT,
                folder = 'NonGrid_Zonals',
                fileFormat = 'CSV',
                selectors = ['id, sum']
                )
        ExportTableTask.start()


        # Total pixels
        ###############
        
        i = i + 1
        if i == 1: 
            
            # Select Year and band 'BurnDate' from ImageCollection
            burns1 = burns0.filterDate(YR + '-01-01', YR + '-12-31') # Define time range
            burns2 = burns1.select('BurnDate')                       # Select band

            # Select Single image from ImageCollection
            img_list = burns2.toList(burns2.size())

            burns3   = ee.Image(img_list.get(0)) # Random month (here January)

            burns4   = burns3.unmask(0) # NULL pixels set to 0
            burns5   = burns4.gt(-1)    # All  pixels set to 1

            # Zonal statistics
            INTACT_FORESTS_allsums = burns5.reduceRegions(collection = INTACT_FORESTS, reducer = ee.Reducer.sum())
            OP_CONCESSIONS_allsums = burns5.reduceRegions(collection = OP_CONCESSIONS, reducer = ee.Reducer.sum())

            # Export to Google Drive folder
            print('export burns all pixels')
            
            ExportTableTask = ee.batch.Export.table.toDrive(
                    collection = INTACT_FORESTS_allsums,             
                    description = 'INTFOR__Burned_ALL_SUM',
                    folder = 'NonGrid_Zonals',
                    fileFormat = 'CSV',
                    selectors = ['fid, sum']
                    )
            ExportTableTask.start()

            ExportTableTask = ee.batch.Export.table.toDrive(
                    collection = OP_CONCESSIONS_allsums,             
                    description = 'OPCON__Burned_ALL_SUM',
                    folder = 'NonGrid_Zonals',
                    fileFormat = 'CSV',
                    selectors = ['id, sum']
                    )
            ExportTableTask.start()





# FOREST LOSS 
#############################################

forloss1 = ee.Image('UMD/hansen/global_forest_change_2017_v1_5')       # Multiple images (collection)

# Select Year and band 'BurnDate' from Image
forloss2 = forloss1.select('lossyear')   # Select band

# Unmask pixels
forloss3 = forloss2.unmask(0) # NULL pixels set to 0
forloss4 = forloss3.gt(-1)    # All  pixels set to 1

# Zonal statistics
INTACT_FORESTS_forloss_allsums = forloss4.reduceRegions(collection = INTACT_FORESTS, reducer = ee.Reducer.sum())
OP_CONCESSIONS_forloss_allsums = forloss4.reduceRegions(collection = OP_CONCESSIONS, reducer = ee.Reducer.sum())

# Export to Google Drive folder
print('export forloss all pixels')

ExportTableTask = ee.batch.Export.table.toDrive(
        collection = INTACT_FORESTS_forloss_allsums,             
        description = 'INTFOR__Lossyear_ALL_SUM',
        folder = 'NonGrid_Zonals',
        fileFormat = 'CSV',
        selectors = ['fid, sum']
        )
ExportTableTask.start()

ExportTableTask = ee.batch.Export.table.toDrive(
        collection = OP_CONCESSIONS_forloss_allsums,             
        description = 'OPCON___ALL_SUM',
        folder = 'NonGrid_Zonals',
        fileFormat = 'CSV',
        selectors = ['id, sum']
        )
ExportTableTask.start()


years = list(range(1,18))
##years = list(range(1,2))
##YEAR = 1
##YR = str(YEAR + 2000)

for YEAR in years:

    YR = str(YEAR + 2000)

    # Select Year and band 'BurnDate' from Image
    forloss2 = forloss1.select('lossyear')   # Select band

    # Select only pixels in a given year
    forloss3 = forloss2.eq(YEAR)             # e.g. 5 means 2005

    # Zonal statistics
    INTACT_FORESTS_forloss_sums = forloss3.reduceRegions(collection = INTACT_FORESTS, reducer = ee.Reducer.sum())
    OP_CONCESSIONS_forloss_sums = forloss3.reduceRegions(collection = OP_CONCESSIONS, reducer = ee.Reducer.sum())


    # Export to Google Drive folder
    print('export forloss' + YR)
    
    ExportTableTask = ee.batch.Export.table.toDrive(
            collection = INTACT_FORESTS_forloss_sums,             
            description = 'INTFOR__Lossyear_' + YR,
            folder = 'NonGrid_Zonals',
            fileFormat = 'CSV',
            selectors = ['fid, sum']
            )
    ExportTableTask.start()

    ExportTableTask = ee.batch.Export.table.toDrive(
            collection = OP_CONCESSIONS_forloss_sums,             
            description = 'OPCON__Lossyear_' + YR,
            folder = 'NonGrid_Zonals',
            fileFormat = 'CSV',
            selectors = ['id, sum']
            )
    ExportTableTask.start()



# FOROBS 
#############################################

proj = ee.Projection('EPSG:32735')

forobs1 = ee.ImageCollection('projects/JRC/TMF/v1_2020/TransitionMap_Subtypes')     # Multiple images (collection)
forobs2 = forobs1.reduce(ee.Reducer.median()).reproject(proj, None, 20)

# Unmask pixels
forobs3 = forobs2.gt(-1) # All pixels set to 1

# Zonal statistics
INTACT_FORESTS_forobs_sums = forobs3.reduceRegions(collection = INTACT_FORESTS, reducer = ee.Reducer.sum())
OP_CONCESSIONS_forobs_sums = forobs3.reduceRegions(collection = OP_CONCESSIONS, reducer = ee.Reducer.sum())


# Export to Google Drive folder
print('export forobs all sum')
ExportTableTask = ee.batch.Export.table.toDrive(
        collection = INTACT_FORESTS_forobs_sums,             
        description = 'INTFOR__forobs_ALL_SUM',
        folder = 'NonGrid_Zonals',
        fileFormat = 'CSV',
        selectors = ['fid, sum']
        )
ExportTableTask.start()

ExportTableTask = ee.batch.Export.table.toDrive(
        collection = OP_CONCESSIONS_forobs_sums,             
        description = 'OPCON__forobs_ALL_SUM',
        folder = 'NonGrid_Zonals',
        fileFormat = 'CSV',
        selectors = ['id, sum']
        )
ExportTableTask.start()


#############################################
# Calculate per class / chunk the number of pixels inside a 5km * 5km area
#############################################

classes = [10,11,12,21,22,23,24,25,26,31,32,33,41,42,51,52,53,54,61,62,63,64,65,66,67,71,72,73,74,81,82,83,84,85,86,91,92,93,94]
##classes = [41,42]

for CLASS in classes:

    # Select a class
    forobs3 = forobs2.eq(CLASS) 

    # Zonal statistics
    INTACT_FORESTS_forobs_class = forobs3.reduceRegions(collection = INTACT_FORESTS, reducer = ee.Reducer.sum())
    OP_CONCESSIONS_forobs_class = forobs3.reduceRegions(collection = OP_CONCESSIONS, reducer = ee.Reducer.sum())


    # Export to Google Drive folder
    
    print('export forobs class ' + str(CLASS))
    ExportTableTask = ee.batch.Export.table.toDrive(
            collection = INTACT_FORESTS_forobs_class,             
            description = 'INTFOR__forobs_' + str(CLASS),
            folder = 'NonGrid_Zonals',
            fileFormat = 'CSV',
            selectors = ['fid, sum']
            )
    ExportTableTask.start()

    ExportTableTask = ee.batch.Export.table.toDrive(
            collection = OP_CONCESSIONS_forobs_class,             
            description = 'OPCON__forobs_' + str(CLASS),
            folder = 'NonGrid_Zonals',
            fileFormat = 'CSV',
            selectors = ['id, sum']
            )
    ExportTableTask.start()


















