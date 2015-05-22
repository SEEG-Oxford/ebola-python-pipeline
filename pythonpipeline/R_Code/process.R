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
source('plotFunctions.R')
source('diseaseMapping.R')

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

###############################################################################
# Start actual plotting of results                                            #
###############################################################################

print("Plotting regional results")
riskData <- getRiskData(movementMatrices, predictionModelNames[1:12], allcasedata, mostRecent)
plotAllRegionalRisks(riskData, districts, countries, country_borders, regionalRiskTitles, paste(districts$COUNTRY_ID,districts$NAME, sep='_'), seqRamp('YlOrRd'))
africaRiskData <- getRiskData(abind(as.movementmatrix(west_africa_gravity[,c(8,14,19)]), along=3), predictionModelNames[13], allcasedata, mostRecent)
plotAllRegionalRisks(africaRiskData, districts, countries, country_borders, regionalRiskTitles[13], paste(districts$COUNTRY_ID,districts$NAME, sep='_'), seqRamp('YlOrRd'))
plotCompositeLeaflet(districts, riskData, seqRamp('YlOrRd'), paste(districts$COUNTRY_ID,districts$NAME, sep='_'))

print("Plotting weighted regional results")
aucmatrix <- calculateAUCMatrix(movementMatrices, predictionModelNames[1:12], allcasedata, mostRecent, coreRegex="GIN|LBR|SLE")
# at this point we don't want to include the west_africa_gravity model in the rest of the calculations
#aucmat2 <- calculateAUCMatrix(abind(as.movementmatrix(west_africa_gravity[,c(8,14,19)]), along=3), predictionModelNames[13], allcasedata, mostRecent)
#aucmatrix <- cbind(aucmat1, aucmat2[,2])
colnames(aucmatrix) <- c("Week index", predictionModelNames[1:12])
#write.csv(aucmatrix, "aucdata.csv")
weighted_riskdata <- calculateWeightedRisks(riskData, tail(aucmatrix,3))
plotRegionalRisks(districts, countries, country_borders, weighted_riskdata, riskData[[1]]$reportedCases, "Regional relative risk of Ebola importation\n using weighted prediction model data", "regional_prediction_weighted", paste(districts$COUNTRY_ID,districts$NAME, sep='_'), seqRamp('YlOrRd'), leaflet=TRUE)

print("Plotting global risk map")
# this calculation is currently ebola specific
source("calculateGlobalRisks.R")
globalRisks <- calculateGlobalRisks(read.csv('../data/all.csv'), allcasedata)
countrycodes <- as.vector(globalRisks$country.1)
risks <- data.frame(country=countrycodes, risk=globalRisks$importation_risk)
plotGlobalRisks(risk=risks[risks$country != "LBR" & risks$country != "GIN" & risks$country != "SLE",], informCountries, allCountries, "global_Overall_prediction", "Global relative risk of Ebola importation\n from overall model", informCountries$admin0_COU, seqRamp('YlOrRd'), "LBR|GIN|SLE")
risks <- data.frame(country=countrycodes, risk=globalRisks$adjacency_relative)
plotGlobalRisks(risk=risks[risks$country != "LBR" & risks$country != "GIN" & risks$country != "SLE",], informCountries, allCountries, "global_Adjacency_prediction", "Global relative risk of Ebola importation\n from Adjacency model", informCountries$admin0_COU, seqRamp('YlOrRd'), "LBR|GIN|SLE")
risks <- data.frame(country=countrycodes, risk=globalRisks$gravity_relative)
plotGlobalRisks(risk=risks[risks$country != "LBR" & risks$country != "GIN" & risks$country != "SLE",], informCountries, allCountries, "global_Gravity_prediction", "Global relative risk of Ebola importation\n from Gravity model", informCountries$admin0_COU, seqRamp('YlOrRd'), "LBR|GIN|SLE")
risks <- data.frame(country=countrycodes, risk=globalRisks$migration_relative)
plotGlobalRisks(risk=risks[risks$country != "LBR" & risks$country != "GIN" & risks$country != "SLE",], informCountries, allCountries, "global_Migration_prediction", "Global relative risk of Ebola importation\n from Migration model", informCountries$admin0_COU, seqRamp('YlOrRd'), "LBR|GIN|SLE")

print("Plotting regional case history maps")
createRegionalCaseHistoryMaps(allcasedata, districts, countries, country_borders, mostRecent, paste(districts$COUNTRY_ID,districts$NAME, sep='_'), colorRampPalette(c("#fcbba1","#fc9272","#fb6a4a","#ef3b2c","#cb181d","#a50f15","#67000d")))

print("Plotting regional prediction history maps")
createRegionalPredictionHistoryMaps(mostRecent, movementMatrices, predictionModelNames[1:12], allcasedata, districts, countries, country_borders, aucmatrix, paste(districts$COUNTRY_ID,districts$NAME, sep='_'), seqRamp('YlOrRd'))

print("Done")
