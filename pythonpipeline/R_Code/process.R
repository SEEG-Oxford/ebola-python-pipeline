# load required libraries (will probably be unnecessary at some point)
require(raster)
require(SDMTools)
require(rgdal)
require(foreach)
require(doParallel)
require(aqfig)
require(abind)

# load required helper functions
print("Loading helper functions")
source('process_movement_data.R')
source('plotFunctions.R')
source('palettes.R')

# load shapefiles
print("Loading Shapefiles")
districts <- shapefile('../data/shapefiles/ad2_FINAL.shp')
countries <- shapefile('../data/shapefiles/countries_wa.shp')
country_borders <- shapefile('../data/shapefiles/country_borders_wa.shp')
informCountries <- shapefile('../data/shapefiles/infoRM_countries.shp')
allCountries <- shapefile('../data/shapefiles/admin2013_0.shp')

# fix an error in districts where CIV has 2 regions named BELIER. One should be MORONOU
dnames <- districts$NAME
dnames[105] <- "MORONOU"
dnames <- gsub("GBÊKE", "GBEKE", dnames)
dnames <- gsub("GBÔKLE", "GBOKLE", dnames)
dnames <- gsub("GÔH", "GOH", dnames)
dnames <- gsub("LÔH_DJIBOUA","LOH_DJIBOUA",  dnames)
districts$NAME <- dnames

# read in the existing movement predictions (based on europe and west africa cdr data)
print("Loading movement predictions")
all_cdr_europe <- read.csv('../data/all_cdr_europe.csv')
west_africa_gravity <- read.csv('../data/gravity.csv')

# read in the EVD case data (obtained from WHO)
print("Loading case data")
evdcasedata <- read.csv('../data/EVD_conf_prob_.csv')
# this must be exactly the same format as evdcasedata and will also need curating
# when cases move from the sitrep to the patientdb
additionalcasedata <- read.csv('../data/EVD_conf_prob_additional.csv')

# combine the real case data with the additional case data
allcasedata <- evdcasedata + additionalcasedata

## prepare the region names in the case data files
districtNames <- names(allcasedata)
districtNames <- gsub("[.]", "_", districtNames)
districtNames <- gsub("\\s", "_", districtNames)
districtNames <- gsub("'", "_", districtNames)
# Correct names of certain regions which are different in the shapefile
districtNames <- gsub("LBR_RIVER_GEE", "LBR_RIVER_GHEE", districtNames)
districtNames <- gsub("CIV_GBEKE", "CIV_GBÊKE", districtNames)
districtNames <- gsub("CIV_GBOKLE", "CIV_GBÔKLE", districtNames)
districtNames <- gsub("CIV_GOH", "CIV_GÔH", districtNames)
districtNames <- gsub("CIV_LOH_DJIBOUA", "CIV_LÔH_DJIBOUA", districtNames)
names(allcasedata) <- districtNames

## prepare the region names in the west africa gravity file
districtNames <- west_africa_gravity$admin2_from
districtNames <- gsub("[.]", "_", districtNames)
districtNames <- gsub("\\s", "_", districtNames)
districtNames <- gsub("'", "_", districtNames)
# Correct names of certain regions which are different in the shapefile
districtNames <- gsub("LBR_RIVER_GEE", "LBR_RIVER_GHEE", districtNames)
west_africa_gravity$admin2_from <- districtNames

districtNames <- west_africa_gravity$admin2_to
districtNames <- gsub("[.]", "_", districtNames)
districtNames <- gsub("\\s", "_", districtNames)
districtNames <- gsub("'", "_", districtNames)
# Correct names of certain regions which are different in the shapefile
districtNames <- gsub("LBR_RIVER_GEE", "LBR_RIVER_GHEE", districtNames)
west_africa_gravity$admin2_to <- districtNames

# get the index of the most recent week of data
mostRecent <- nrow(allcasedata)

# the prediction models used
predictionModelNames <- c("France Gravity", "Portugal Gravity", "Spain Gravity", "France Original Radiation", "Portugal Original Radiation", "Spain Original Radiation", "France Radiation With Selection", "Portugal Radiation With Selection", "Spain Radiation With Selection", "France Uniform Selection", "Portugal Uniform Selection", "Spain Uniform Selection", "West Africa Gravity")

# titles to be displayed on the regional risk maps
regionalRiskTitles <- c(
"Regional relative risk of Ebola importation\n using gravity model from France", 
"Regional relative risk of Ebola importation\n using gravity model from Portugal", 
"Regional relative risk of Ebola importation\n using gravity model from Spain", 
"Regional relative risk of Ebola importation\n using original radiation model from France",
"Regional relative risk of Ebola importation\n using original radiation model from Portugal", 
"Regional relative risk of Ebola importation\n using original radiation model from Spain", 
"Regional relative risk of Ebola importation\n using radiation with selection model from France", 
"Regional relative risk of Ebola importation\n using radiation with selection model from Portugal", 
"Regional relative risk of Ebola importation\n using radiation with selection model from Spain", 
"Regional relative risk of Ebola importation\n using uniform selection model from France", 
"Regional relative risk of Ebola importation\n using uniform selection model from Portugal", 
"Regional relative risk of Ebola importation\n using uniform selection model from Spain", 
"Regional relative risk of Ebola importation\n using West Africa gravity model")

# pre-calculate all the movement matrices
cl <- makeCluster(8)
registerDoParallel(cl)

# for this to work the results of as.movementmatrix must be of the same dimensions. For this reason the west_africa_gravity dataset cannot be combined with the other movement models as it doesn't extend
# to many of the other surrounding countries
movementMatrices <- foreach(idx=3:14, .combine = function(...) abind(..., along=3)) %dopar% {
	as.movementmatrix(all_cdr_europe[,c(1,2,idx)])
}
stopCluster(cl)

print("Plotting regional results")
source("regional_results/plotMap.R")
print("Plotting weighted regional results")
source("regional_results/plotWeightedMap.R")
print("Plotting global risk map")
source("global_results/plotMap.R")
print("Plotting regional case history maps")
source("regional_case_history/createRegionalCaseHistoryPlots.R")
print("Plotting regional prediction history maps")
source("regional_prediction_history/createRegionalPredictionHistory.R")
print("Done")
riskData <- plotAllRegionalRisks(movementMatrices, predictionModelNames[1:12],  regionalRiskTitles[1:12], allcasedata)
plotAllRegionalRisks(abind(as.movementmatrix(west_africa_gravity[,c(8,14,19)]), along=3), predictionModelNames[13], regionalRiskTitles[13], allcasedata)

plotCompositeLeaflet(districts, riskData)

aucmat1 <- calculateAUCMatrix(movementMatrices, predictionModelNames[1:12], allcasedata)
aucmat2 <- calculateAUCMatrix(abind(as.movementmatrix(west_africa_gravity[,c(8,14,19)]), along=3), predictionModelNames[13], allcasedata)

aucmatrix <- cbind(aucmat1, aucmat2[,2])
colnames(aucmatrix) <- c("Week index", predictionModelNames)

