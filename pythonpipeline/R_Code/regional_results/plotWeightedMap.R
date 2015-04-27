# functions (and examples) to plot maps of human movement and EVD spread

# load packages
require(raster)
require(SDMTools)
require(rgdal)
source('process_movement_data.R')
source('plotFunctions.R')

# load shapefiles
districts <- shapefile('ad2_FINAL.shp')
countries <- shapefile('countries_wa.shp')
country_borders <- shapefile('country_borders_wa.shp')

# fix an error in districts where CIV has 2 regions named BELIER. One should be MORONOU
dnames <- districts$NAME
dnames[105] <- "MORONOU"
districts$NAME <- dnames



# the prediction models used
predictionModelNames <- c("France Gravity", "France Original Radiation", "France Radiation With Selection", "France Uniform Selection","Portugal Gravity", "Portugal Original Radiation", "Portugal Radiation With Selection", "Portugal Uniform Selection","Spain Gravity", "Spain Original Radiation", "Spain Radiation With Selection", "Spain Uniform Selection")

# get total number of weeks of data
totalWeeks <- nrow(allcasedata)
# In order to calculate the AUC we need to not include the last week in the prediction
mostRecent <- totalWeeks - 3

all_cdr_europe <- read.csv('../../data/all_cdr_europe.csv')

# 3 is france/gravity
francegravityriskdata <- getData(as.movementmatrix(all_cdr_europe[,c(1,2,3)]), mostRecent+1, "France Gravity", auc=FALSE)

# 6 is france/radiation
franceradiationriskdata <- getData(as.movementmatrix(all_cdr_europe[,c(1,2,6)]), mostRecent+1, "France Original Radiation", auc=FALSE)

# 9 is france/radiation-with-selection
franceradselriskdata <- getData(as.movementmatrix(all_cdr_europe[,c(1,2,9)]), mostRecent+1, "France Radiation with Selection", auc=FALSE)

# 12 is france/uniform
franceuniformriskdata <- getData(as.movementmatrix(all_cdr_europe[,c(1,2,12)]), mostRecent+1, "France Uniform", auc=FALSE)

# 4 is portugal/gravity
portugalgravityriskdata <- getData(as.movementmatrix(all_cdr_europe[,c(1,2,4)]), mostRecent+1, "Portugal Gravity", auc=FALSE)

# 7 is portugal/radiation
portugalradiationriskdata <- getData(as.movementmatrix(all_cdr_europe[,c(1,2,7)]), mostRecent+1, "Portugal Original Radiation", auc=FALSE)

# 10 is portugal/radiation-with-selection
portugalradselriskdata <- getData(as.movementmatrix(all_cdr_europe[,c(1,2,10)]), mostRecent+1, "Portugal Radiation with Selection", auc=FALSE)

# 13 is portugal/uniform
portugaluniformriskdata <- getData(as.movementmatrix(all_cdr_europe[,c(1,2,13)]), mostRecent+1, "Portugal Uniform", auc=FALSE)

# 5 is spain/gravity
spaingravityriskdata <- getData(as.movementmatrix(all_cdr_europe[,c(1,2,5)]), mostRecent+1, "Spain Gravity", auc=FALSE)

# 8 is spain/radiation
spainradiationriskdata <- getData(as.movementmatrix(all_cdr_europe[,c(1,2,8)]), mostRecent+1, "Spain Original Radiation", auc=FALSE)

# 11 is spain/radiation-with-selection
spainradselriskdata <- getData(as.movementmatrix(all_cdr_europe[,c(1,2,11)]), mostRecent+1, "Spain Radiation with Selection", auc=FALSE)

# 14 is spain/uniform
spainuniformriskdata <- getData(as.movementmatrix(all_cdr_europe[,c(1,2,14)]), mostRecent+1, "Spain Uniform", auc=FALSE)

# read all aucs
aucs <- read.csv('aucdata.csv')
# pull out the last 3
latestaucs <- tail(aucs,3)
# get the average ignore NaNs, nulls etc
avgauc <- colMeans(latestaucs[,c(-1:-2)], na.rm = TRUE)
names(avgauc) <- predictionModelNames

# multiply each models predicted risk by its avg auc value
franceweighted_gravity <- francegravityriskdata$predictedRegions * avgauc[1]
franceweighted_radiation <- franceradiationriskdata$predictedRegions * avgauc[2]
franceweighted_radsel <- franceradselriskdata$predictedRegions * avgauc[3]
franceweighted_uniform <- franceuniformriskdata$predictedRegions * avgauc[4]
portugalweighted_gravity <- portugalgravityriskdata$predictedRegions * avgauc[5]
portugalweighted_radiation <- portugalradiationriskdata$predictedRegions * avgauc[6]
portugalweighted_radsel <- portugalradselriskdata$predictedRegions * avgauc[7]
portugalweighted_uniform <- portugaluniformriskdata$predictedRegions * avgauc[8]
spainweighted_gravity <- spaingravityriskdata$predictedRegions * avgauc[9]
spainweighted_radiation <- spainradiationriskdata$predictedRegions * avgauc[10]
spainweighted_radsel <- spainradselriskdata$predictedRegions * avgauc[11]
spainweighted_uniform <- spainuniformriskdata$predictedRegions * avgauc[12]

# sum the weighted risks
weighted_riskdata <- franceweighted_gravity + franceweighted_radiation + franceweighted_radsel + franceweighted_uniform + portugalweighted_gravity + portugalweighted_radiation + portugalweighted_radsel + portugalweighted_uniform + spainweighted_gravity + spainweighted_radiation + spainweighted_radsel + spainweighted_uniform
# normalise 0..1
weighted_riskdata <- weighted_riskdata / max(weighted_riskdata)

# plot
plotRisks(districts, countries, country_borders, weighted_riskdata, francegravityriskdata$reportedCases, "Regional relative risk of Ebola importation\n using weighted prediction model data", "regional_prediction_weighted")
# print out the relative proportions of each prediction in the weighted map
print((avgauc[1:12] / max(avgauc[1:12])) /sum(avgauc[1:12]))
write.csv((avgauc[1:12] / max(avgauc[1:12])) /sum(avgauc[1:12]), "weightings.csv")
