# functions (and examples) to plot maps of human movement and EVD spread

# load packages
require(raster)
require(SDMTools)
require(rgdal)
require(aqfig)
require(foreach)
require(doParallel)
source('palettes.R')
source('process_movement_data.R')

plotMap <- function (vals,
                     districts,
                     countries,
                     country_borders,
                     zlim = range(vals),
                     ramp = seqRamp(),
                     n = 1000,
					 reportedCases,
					 plotTitle) {
  # given some values vals (one for each district) a corresponding 
  # district-level shapefile, a colorRampPalette and
  # optional range of the colours, plot a map of the values with a nice legend.
  
  # replaced by include.lowest arg to cut
#   # if the range of vals is require as zlim
#   if(all.equal(range(vals), zlim)) {
# 
#     # define epsilon
#     eps <- .Machine$double.eps
#     
#     # and add to range
#     zlim <- zlim + c(-1, 1) * eps
#     
#     # this way one won't get omitted when calculating bins
#   }
  
  # get the colours
  col_range <- seq(zlim[1], zlim[2], length.out = n)
  cols <- ramp(n)
  
  bins <- cut(vals, col_range, include.lowest = TRUE)
  regionColours <- cols[bins]
  
  regionColours[paste(districts$COUNTRY_ID,districts$NAME, sep='_') %in% names(reportedCases)] <- "#33D3FF"
  
  par(mar=c(1,1,2,2))
  plot(countries, col = grey(0.9), border = 'white', lwd = 3)
  plot(districts, col = regionColours, border = 'white', add = TRUE)
  plot(country_borders, col = grey(0.4), add = TRUE)
  vertical.image.legend(col=seqRamp('YlOrRd')(1000),zlim=c(0,1))
  title(main=plotTitle)  
}

plotRisks <- function(districts, countries, country_borders, predictedRegions, reportedCases, plotTitle, filename) {
	vals <- rep(NA, nrow(districts))
	for(idx in 1:length(predictedRegions)) {	
		vals[match(names(predictedRegions[idx]), paste(districts$COUNTRY_ID,districts$NAME, sep='_'))] <- predictedRegions[idx]
	}
	png(filename=paste(filename, ".png", sep=""), width=800, height=700, units='px', pointsize=20)
	plotMap(vals,
			districts,
			countries,
			country_borders,
			zlim = c(0, 1),
			ramp = seqRamp('YlOrRd'),
			reportedCases = reportedCases,
			plotTitle = plotTitle)
	dev.off()
	png(filename=paste(filename, "large.png", sep="_"), width=8000, height=7000, units='px', pointsize=100)
	plotMap(vals,
			districts,
			countries,
			country_borders,
			zlim = c(0, 1),
			ramp = seqRamp('YlOrRd'),
			reportedCases = reportedCases,
			plotTitle = plotTitle)
	dev.off()
}

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
mostRecent <- totalWeeks

# For the current state we need to +1 the mostRecent value as we are wanting
# to create risk data for the coming week (i.e. predict the future) rather
# than comparing the predicted risk with the known data for subsequent weeks

all_cdr_europe <- read.csv('../../data/all_cdr_europe.csv')
west_africa_gravity <- read.csv('../../data/gravity.csv')

# 3 is france/gravity
riskdata <- getData(as.movementmatrix(all_cdr_europe[,c(1,2,3)]), mostRecent, "France Gravity", auc=FALSE)
plotRisks(districts, countries, country_borders, riskdata$predictedRegions, riskdata$reportedCases, "Regional relative risk of Ebola importation\n using gravity model from France", "regional_prediction_gravity_france")

# 6 is france/radiation
riskdata <- getData(as.movementmatrix(all_cdr_europe[,c(1,2,6)]), mostRecent, "France Original Radiation", auc=FALSE)
plotRisks(districts, countries, country_borders, riskdata$predictedRegions, riskdata$reportedCases, "Regional relative risk of Ebola importation\n using original radiation model from France", "regional_prediction_radiation_france")

# 9 is france/radiation-with-selection
riskdata <- getData(as.movementmatrix(all_cdr_europe[,c(1,2,9)]), mostRecent, "France Radiation with Selection", auc=FALSE)
plotRisks(districts, countries, country_borders, riskdata$predictedRegions, riskdata$reportedCases, "Regional relative risk of Ebola importation\n using radiation with selection model from France", "regional_prediction_radsel_france")

# 12 is france/uniform
riskdata <- getData(as.movementmatrix(all_cdr_europe[,c(1,2,12)]), mostRecent, "France Uniform", auc=FALSE)
plotRisks(districts, countries, country_borders, riskdata$predictedRegions, riskdata$reportedCases, "Regional relative risk of Ebola importation\n using uniform selection model from France", "regional_prediction_uniform_france")

# 4 is portugal/gravity
riskdata <- getData(as.movementmatrix(all_cdr_europe[,c(1,2,4)]), mostRecent, "Portugal Gravity", auc=FALSE)
plotRisks(districts, countries, country_borders, riskdata$predictedRegions, riskdata$reportedCases, "Regional relative risk of Ebola importation\n using gravity model from Portugal", "regional_prediction_gravity_portugal")

# 7 is portugal/radiation
riskdata <- getData(as.movementmatrix(all_cdr_europe[,c(1,2,7)]), mostRecent, "Portugal Original Radiation", auc=FALSE)
plotRisks(districts, countries, country_borders, riskdata$predictedRegions, riskdata$reportedCases, "Regional relative risk of Ebola importation\n using original radiation model from Portugal", "regional_prediction_radiation_portugal")

# 10 is portugal/radiation-with-selection
riskdata <- getData(as.movementmatrix(all_cdr_europe[,c(1,2,10)]), mostRecent, "Portugal Radiation with Selection", auc=FALSE)
plotRisks(districts, countries, country_borders, riskdata$predictedRegions, riskdata$reportedCases, "Regional relative risk of Ebola importation\n using radiation with selection model from Portugal", "regional_prediction_radsel_portugal")

# 13 is portugal/uniform
riskdata <- getData(as.movementmatrix(all_cdr_europe[,c(1,2,13)]), mostRecent, "Portugal Uniform", auc=FALSE)
plotRisks(districts, countries, country_borders, riskdata$predictedRegions, riskdata$reportedCases, "Regional relative risk of Ebola importation\n using uniform selection model from Portugal", "regional_prediction_uniform_portugal")

# 5 is spain/gravity
riskdata <- getData(as.movementmatrix(all_cdr_europe[,c(1,2,5)]), mostRecent, "Spain Gravity", auc=FALSE)
plotRisks(districts, countries, country_borders, riskdata$predictedRegions, riskdata$reportedCases, "Regional relative risk of Ebola importation\n using gravity model from Spain", "regional_prediction_gravity_spain")

# 8 is spain/radiation
riskdata <- getData(as.movementmatrix(all_cdr_europe[,c(1,2,8)]), mostRecent, "Spain Original Radiation", auc=FALSE)
plotRisks(districts, countries, country_borders, riskdata$predictedRegions, riskdata$reportedCases, "Regional relative risk of Ebola importation\n using original radiation model from Spain", "regional_prediction_radiation_spain")

# 11 is spain/radiation-with-selection
riskdata <- getData(as.movementmatrix(all_cdr_europe[,c(1,2,11)]), mostRecent, "Spain Radiation with Selection", auc=FALSE)
plotRisks(districts, countries, country_borders, riskdata$predictedRegions, riskdata$reportedCases, "Regional relative risk of Ebola importation\n using radiation with selection model from Spain", "regional_prediction_radsel_spain")

# 14 is spain/uniform
riskdata <- getData(as.movementmatrix(all_cdr_europe[,c(1,2,14)]), mostRecent, "Spain Uniform", auc=FALSE)
plotRisks(districts, countries, country_borders, riskdata$predictedRegions, riskdata$reportedCases, "Regional relative risk of Ebola importation\n using uniform selection model from Spain", "regional_prediction_uniform_spain")

# overall africa gravity model
riskdata <- getData(as.movementmatrix(west_africa_gravity[,c(8,14,19)]), mostRecent, "West Africa Gravity", auc=FALSE)
plotRisks(districts, countries, country_borders, riskdata$predictedRegions, riskdata$reportedCases, "Regional relative risk of Ebola importation\n using West Africa gravity model", "regional_prediction_gravity_west_africa")


cl <- makeCluster(8)
registerDoParallel(cl)


aucmatrix <- foreach(idx=1:mostRecent,.combine=rbind) %dopar% {
	# we need to manually track the indices if using a parallel library
	aucresult <- vector()
	aucresult[1] <- idx
	# 3 is france/gravity
	riskdata <- getData(as.movementmatrix(all_cdr_europe[,c(1,2,3)]), idx, predictionModelNames[1])
	aucresult[2] <- riskdata$AUC
	# plotRisks(vals, districts, countries, country_borders, riskdata$predictedRegions, riskdata$reportedCases, "Regional relative risk of Ebola importation\n using gravity model from France", paste("historical/plots/regional_prediction_gravity_france_week", idx, sep="_"))

	# 6 is france/radiation
	riskdata <- getData(as.movementmatrix(all_cdr_europe[,c(1,2,6)]), idx, predictionModelNames[2])
	# plotRisks(vals, districts, countries, country_borders, riskdata$predictedRegions, riskdata$reportedCases, "Regional relative risk of Ebola importation\n using original radiation model", paste("historical/plots/regional_prediction_radiation_france_week", idx, sep="_"))
	aucresult[3] <- riskdata$AUC

	# 9 is france/radiation-with-selection
	riskdata <- getData(as.movementmatrix(all_cdr_europe[,c(1,2,9)]), idx, predictionModelNames[3])
	# plotRisks(vals, districts, countries, country_borders, riskdata$predictedRegions, riskdata$reportedCases, "Regional relative risk of Ebola importation\n using radiation with selection model", paste("historical/plots/regional_prediction_radsel_france_week", idx, sep="_"))
	aucresult[4] <- riskdata$AUC

	# 12 is france/uniform
	riskdata <- getData(as.movementmatrix(all_cdr_europe[,c(1,2,12)]), idx, predictionModelNames[4])
	# plotRisks(vals, districts, countries, country_borders, riskdata$predictedRegions, riskdata$reportedCases, "Regional relative risk of Ebola importation\n using uniform selection model", paste("historical/plots/regional_prediction_uniform_france_week", idx, sep="_"))
	aucresult[5] <- riskdata$AUC
	
	
	# 4 is portual/gravity
	riskdata <- getData(as.movementmatrix(all_cdr_europe[,c(1,2,4)]), idx, predictionModelNames[5])
	# plotRisks(vals, districts, countries, country_borders, riskdata$predictedRegions, riskdata$reportedCases, "Regional relative risk of Ebola importation\n using gravity model", paste("historical/plots/regional_prediction_gravity_portugal_week", idx, sep="_"))
	aucresult[6] <- riskdata$AUC

	# 7 is portual/radiation
	riskdata <- getData(as.movementmatrix(all_cdr_europe[,c(1,2,7)]), idx, predictionModelNames[6])
	# plotRisks(vals, districts, countries, country_borders, riskdata$predictedRegions, riskdata$reportedCases, "Regional relative risk of Ebola importation\n using original radiation model", paste("historical/plots/regional_prediction_radiation_portugal_week", idx, sep="_"))
	aucresult[7] <- riskdata$AUC

	# 10 is portual/radiation-with-selection
	riskdata <- getData(as.movementmatrix(all_cdr_europe[,c(1,2,10)]), idx, predictionModelNames[7])
	# plotRisks(vals, districts, countries, country_borders, riskdata$predictedRegions, riskdata$reportedCases, "Regional relative risk of Ebola importation\n using radiation with selection model", paste("historical/plots/regional_prediction_radsel_portugal_week", idx, sep="_"))
	aucresult[8] <- riskdata$AUC

	# 13 is portual/uniform
	riskdata <- getData(as.movementmatrix(all_cdr_europe[,c(1,2,13)]), idx, predictionModelNames[8])
	# plotRisks(vals, districts, countries, country_borders, riskdata$predictedRegions, riskdata$reportedCases, "Regional relative risk of Ebola importation\n using uniform selection model", paste("historical/plots/regional_prediction_uniform_portugal_week", idx, sep="_"))
	aucresult[9] <- riskdata$AUC
	
	
	# 5 is spain/gravity
	riskdata <- getData(as.movementmatrix(all_cdr_europe[,c(1,2,5)]), idx, predictionModelNames[9])
	# plotRisks(vals, districts, countries, country_borders, riskdata$predictedRegions, riskdata$reportedCases, "Regional relative risk of Ebola importation\n using gravity model", paste("historical/plots/regional_prediction_gravity_spain_week", idx, sep="_"))
	aucresult[10] <- riskdata$AUC

	# 8 is portual/radiation
	riskdata <- getData(as.movementmatrix(all_cdr_europe[,c(1,2,8)]), idx, predictionModelNames[10])
	# plotRisks(vals, districts, countries, country_borders, riskdata$predictedRegions, riskdata$reportedCases, "Regional relative risk of Ebola importation\n using original radiation model", paste("historical/plots/regional_prediction_radiation_spain_week", idx, sep="_"))
	aucresult[11] <- riskdata$AUC

	# 11 is portual/radiation-with-selection
	riskdata <- getData(as.movementmatrix(all_cdr_europe[,c(1,2,11)]), idx, predictionModelNames[11])
	# plotRisks(vals, districts, countries, country_borders, riskdata$predictedRegions, riskdata$reportedCases, "Regional relative risk of Ebola importation\n using radiation with selection model", paste("historical/plots/regional_prediction_radsel_spain_week", idx, sep="_"))
	aucresult[12] <- riskdata$AUC

	# 14 is portual/uniform
	riskdata <- getData(as.movementmatrix(all_cdr_europe[,c(1,2,14)]), idx, predictionModelNames[12])
	# plotRisks(vals, districts, countries, country_borders, riskdata$predictedRegions, riskdata$reportedCases, "Regional relative risk of Ebola importation\n using uniform selection model", paste("historical/plots/regional_prediction_uniform_spain_week", idx, sep="_"))
	aucresult[13] <- riskdata$AUC
	
	# overall africa gravity model
	riskdata <- getData(as.movementmatrix(west_africa_gravity[,c(8,14,19)]), idx, "West Africa Gravity")
	# plotRisks(vals, districts, countries, country_borders, riskdata$predictedRegions, riskdata$reportedCases, "Regional relative risk of Ebola importation\n using West Africa gravity model", paste("historical/plots/regional_prediction_gravity_west_africa_week", idx, sep="_"))
	aucresult[14] <- riskdata$AUC
	
	aucresult
}
stopCluster(cl)


colnames(aucmatrix) <- c("Week index", predictionModelNames, "West Africa Gravity")

write.csv(aucmatrix, "aucdata.csv")