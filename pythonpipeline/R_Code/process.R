# load required libraries (will probably be unnecessary at some point)
require(raster)
require(SDMTools)
require(rgdal)
require(foreach)
require(doParallel)
require(aqfig)

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

# get the index of the most recent week of data
mostRecent <- nrow(allcasedata)

# the prediction models used
predictionModelNames <- c("France Gravity", "France Original Radiation", "France Radiation With Selection", "France Uniform Selection","Portugal Gravity", "Portugal Original Radiation", "Portugal Radiation With Selection", "Portugal Uniform Selection","Spain Gravity", "Spain Original Radiation", "Spain Radiation With Selection", "Spain Uniform Selection")

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
