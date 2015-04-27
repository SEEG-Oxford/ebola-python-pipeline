# functions (and examples) to plot maps of human movement and EVD spread

# load packages
require(raster)
require(SDMTools)
require(rgdal)
source('../plotFunctions.R')

# load shapefiles
countries <- shapefile('infoRM_countries.shp')
adm0 <- shapefile('admin2013_0.shp')

source("calculateRisks.R")

countrycodes <- as.vector(all$country.1)

risks <- data.frame(country=countrycodes, risk=all$importation_risk)
plotGlobalRisks(risks, countries, adm0, "global_Overall_prediction", "Global relative risk of Ebola importation\n from overall model")

risks <- data.frame(country=countrycodes, risk=all$adjacency_relative)
plotGlobalRisks(risks, countries, adm0, "global_Adjacency_prediction", "Global relative risk of Ebola importation\n from Adjacency model")

risks <- data.frame(country=countrycodes, risk=all$gravity_relative)
plotGlobalRisks(risks, countries, adm0, "global_Gravity_prediction", "Global relative risk of Ebola importation\n from Gravity model")

risks <- data.frame(country=countrycodes, risk=all$migration_relative)
plotGlobalRisks(risks, countries, adm0, "global_Migration_prediction", "Global relative risk of Ebola importation\n from Migration model")

