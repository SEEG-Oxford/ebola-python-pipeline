# functions (and examples) to plot maps of human movement and EVD spread

# load packages
require(raster)
require(SDMTools)
require(rgdal)
require(aqfig)
require(foreach)
require(doParallel)
source('../plotFunctions.R')
source('../process_movement_data.R')

evdcasedata <- read.csv('../../data/EVD_conf_prob_.csv')
# this must be exactly the same format as allcasedata and will also need curating
# when cases move from the sitrep to the patientdb
additionalcasedata <- read.csv('../../data/EVD_conf_prob_additional.csv')

allcasedata <- evdcasedata + additionalcasedata

# load shapefiles
districts <- shapefile('../../data/shapefiles/ad2_FINAL.shp')
countries <- shapefile('../../data/shapefiles/countries_wa.shp')
country_borders <- shapefile('../../data/shapefiles/country_borders_wa.shp')

# fix an error in districts where CIV has 2 regions named BELIER. One should be MORONOU
dnames <- districts$NAME
dnames[105] <- "MORONOU"
districts$NAME <- dnames


# get total number of weeks of data
totalWeeks <- nrow(allcasedata)
# In order to calculate the AUC we need to not include the last week in the prediction
mostRecent <- totalWeeks

# For the current state we need to +1 the mostRecent value as we are wanting
# to create risk data for the coming week (i.e. predict the future) rather
# than comparing the predicted risk with the known data for subsequent weeks

all_cdr_europe <- read.csv('../../data/all_cdr_europe.csv')


aucmatrix <- foreach(idx=1:mostRecent,.combine=rbind) %do% {	
# plot
rawdate <- paste(gsub("-W", " ", as.character(evdcasedata[idx,1])), "1", sep=" ")
formattedDate <- format(as.POSIXct(rawdate, format="%Y %U %u"), format="%B %d %Y")
plotDate(formattedDate, filename=paste(formatC(idx, width=2, flag="0"), "date", sep="_"))
plotHistoricCases(districts, countries, country_borders, 0, getSimpleData(idx), plotTitle="", filename=paste(formatC(idx, width=2, flag="0"), "regional_cases_week", sep="_"))

}