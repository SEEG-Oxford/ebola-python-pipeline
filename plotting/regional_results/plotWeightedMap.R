# functions (and examples) to plot maps of human movement and EVD spread

# load packages
require(raster)
require(SDMTools)
require(rgdal)
require(aqfig)
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

plotRisks <- function(vals, districts, countries, country_borders, predictedRegions, reportedCases, plotTitle, filename) {
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
mostRecent <- totalWeeks - 3

all_cdr_europe <- read.csv('../data/all_cdr_europe.csv')

# 3 is france/gravity
gravityriskdata <- getData(as.movementmatrix(all_cdr_europe[,c(1,2,3)]), mostRecent+1, "France Gravity", auc=FALSE)

# 6 is france/radiation
radiationriskdata <- getData(as.movementmatrix(all_cdr_europe[,c(1,2,6)]), mostRecent+1, "France Original Radiation", auc=FALSE)

# 9 is france/radiation-with-selection
radselriskdata <- getData(as.movementmatrix(all_cdr_europe[,c(1,2,9)]), mostRecent+1, "France Radiation with Selection", auc=FALSE)

# 12 is france/uniform
uniformriskdata <- getData(as.movementmatrix(all_cdr_europe[,c(1,2,12)]), mostRecent+1, "France Uniform", auc=FALSE)

# read all aucs
aucs <- read.csv('aucdata.csv')
# pull out the last 3
latestaucs <- tail(aucs,3)
# get the average ignore NaNs, nulls etc
avgauc <- colMeans(aucs[,c(-1:-2)], na.rm = TRUE)

# multiply each models predicted risk by its avg auc value
weighted_gravity <- gravityriskdata$predictedRegions * avgauc[1]
weighted_radiation <- radiationriskdata$predictedRegions * avgauc[2]
weighted_radsel <- radselriskdata$predictedRegions * avgauc[3]
weighted_uniform <- uniformriskdata$predictedRegions * avgauc[4]

# sum the weighted risks
weighted_riskdata <- weighted_gravity + weighted_radiation + weighted_radsel + weighted_uniform
# normalise 0..1
weighted_riskdata <- weighted_riskdata / max(weighted_riskdata)

# plot
plotRisks(vals, districts, countries, country_borders, weighted_riskdata, gravityriskdata$reportedCases, "Regional relative risk of Ebola importation\n using weighted prediction model data", "regional_prediction_weighted")
