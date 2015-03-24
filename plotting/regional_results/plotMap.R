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
  
  #
  
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

# get total number of weeks of data
totalWeeks <- nrow(allcasedata)
mostRecent <- totalWeeks - 2

# 9 is france/radiation-with-selection
riskdata <- getData(9, mostRecent)
plotRisks(vals, districts, countries, country_borders, riskdata$predictedRegions, riskdata$reportedCases, "Regional relative risk of Ebola importation\n using radiation with selection model", "regional_prediction_radsel_france")

# 6 is france/radiation
riskdata <- getData(6, mostRecent)
plotRisks(vals, districts, countries, country_borders, riskdata$predictedRegions, riskdata$reportedCases, "Regional relative risk of Ebola importation\n using original radiation model", "regional_prediction_radiation_france")

# 3 is france/gravity
riskdata <- getData(3, mostRecent)
plotRisks(vals, districts, countries, country_borders, riskdata$predictedRegions, riskdata$reportedCases, "Regional relative risk of Ebola importation\n using gravity model", "regional_prediction_gravity_france")

# 12 is france/uniform
riskdata <- getData(12, mostRecent)
plotRisks(vals, districts, countries, country_borders, riskdata$predictedRegions, riskdata$reportedCases, "Regional relative risk of Ebola importation\n using uniform selection model", "regional_prediction_uniform_france")

for(idx in 1:mostRecent) {
	# 9 is france/radiation-with-selection
	riskdata <- getData(9, idx)
	plotRisks(vals, districts, countries, country_borders, riskdata$predictedRegions, riskdata$reportedCases, "Regional relative risk of Ebola importation\n using radiation with selection model", paste("historical/regional_prediction_radsel_france_week", idx, sep="_"))

	# 6 is france/radiation
	riskdata <- getData(6, idx)
	plotRisks(vals, districts, countries, country_borders, riskdata$predictedRegions, riskdata$reportedCases, "Regional relative risk of Ebola importation\n using original radiation model", paste("historical/regional_prediction_radiation_france_week", idx, sep="_"))

	# 3 is france/gravity
	riskdata <- getData(3, idx)
	plotRisks(vals, districts, countries, country_borders, riskdata$predictedRegions, riskdata$reportedCases, "Regional relative risk of Ebola importation\n using gravity model", paste("historical/regional_prediction_gravity_france_week", idx, sep="_"))

	# 12 is france/uniform
	riskdata <- getData(12, idx)
	plotRisks(vals, districts, countries, country_borders, riskdata$predictedRegions, riskdata$reportedCases, "Regional relative risk of Ebola importation\n using uniform selection model", paste("historical/regional_prediction_uniform_france_week", idx, sep="_"))
}