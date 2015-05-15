# functions (and examples) to plot maps of human movement and EVD spread

# load packages
require(raster)
require(SDMTools)
require(rgdal)
require(aqfig)
require(foreach)
require(doParallel)
source('../palettes.R')
source('../process_movement_data.R')

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
  
  # get the colours
  col_range <- seq(zlim[1], zlim[2], length.out = n)
  cols <- ramp(n)
  
  bins <- cut(vals, col_range, include.lowest = TRUE)
  regionColours <- cols[bins]
  
  regionColours[paste(districts$COUNTRY_ID,districts$NAME, sep='_') %in% names(reportedCases)] <- grey(0.7)
  
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
}

# load shapefiles
districts <- shapefile('../../data/shapefiles/ad2_FINAL.shp')
countries <- shapefile('../../data/shapefiles/countries_wa.shp')
country_borders <- shapefile('../../data/shapefiles/country_borders_wa.shp')

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

all_cdr_europe <- read.csv('../../data/all_cdr_europe.csv')

cl <- makeCluster(32, outfile="out.log")
registerDoParallel(cl)


aucmatrix <- foreach(idx=4:mostRecent,.combine=rbind) %dopar% {
	source('../palettes.R')
	source('../process_movement_data.R')
	require(aqfig)
	require(raster)
	# 3 is france/gravity
	francegravityriskdata <- getData(as.movementmatrix(all_cdr_europe[,c(1,2,3)]), idx, "France Gravity", auc=FALSE)

	# 6 is france/radiation
	franceradiationriskdata <- getData(as.movementmatrix(all_cdr_europe[,c(1,2,6)]), idx, "France Original Radiation", auc=FALSE)

	# 9 is france/radiation-with-selection
	franceradselriskdata <- getData(as.movementmatrix(all_cdr_europe[,c(1,2,9)]), idx, "France Radiation with Selection", auc=FALSE)

	# 12 is france/uniform
	franceuniformriskdata <- getData(as.movementmatrix(all_cdr_europe[,c(1,2,12)]), idx, "France Uniform", auc=FALSE)

	# 4 is portugal/gravity
	portugalgravityriskdata <- getData(as.movementmatrix(all_cdr_europe[,c(1,2,4)]), idx, "Portugal Gravity", auc=FALSE)

	# 7 is portugal/radiation
	portugalradiationriskdata <- getData(as.movementmatrix(all_cdr_europe[,c(1,2,7)]), idx, "Portugal Original Radiation", auc=FALSE)

	# 10 is portugal/radiation-with-selection
	portugalradselriskdata <- getData(as.movementmatrix(all_cdr_europe[,c(1,2,10)]), idx, "Portugal Radiation with Selection", auc=FALSE)

	# 13 is portugal/uniform
	portugaluniformriskdata <- getData(as.movementmatrix(all_cdr_europe[,c(1,2,13)]), idx, "Portugal Uniform", auc=FALSE)

	# 5 is spain/gravity
	spaingravityriskdata <- getData(as.movementmatrix(all_cdr_europe[,c(1,2,5)]), idx, "Spain Gravity", auc=FALSE)

	# 8 is spain/radiation
	spainradiationriskdata <- getData(as.movementmatrix(all_cdr_europe[,c(1,2,8)]), idx, "Spain Original Radiation", auc=FALSE)

	# 11 is spain/radiation-with-selection
	spainradselriskdata <- getData(as.movementmatrix(all_cdr_europe[,c(1,2,11)]), idx, "Spain Radiation with Selection", auc=FALSE)

	# 14 is spain/uniform
	spainuniformriskdata <- getData(as.movementmatrix(all_cdr_europe[,c(1,2,14)]), idx, "Spain Uniform", auc=FALSE)

	# read all aucs
	aucs <- read.csv('../regional_results/aucdata.csv')
	# pull out the last 3
	latestaucs <- aucs[(idx-3):(idx-1),]

	# get the average ignore NaNs, nulls etc
	avgauc <- colMeans(latestaucs [,c(-1:-2)], na.rm = TRUE)
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
	plotRisks(districts, countries, country_borders, weighted_riskdata, francegravityriskdata$reportedCases, plotTitle="", filename=paste(formatC(idx, width=2, flag="0"), "regional_prediction_weighted", sep="_"))
}
stopCluster(cl)
