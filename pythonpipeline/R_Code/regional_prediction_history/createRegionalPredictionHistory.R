# functions (and examples) to plot maps of human movement and EVD spread

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

createRegionalPredictionHistoryMaps <- function(mostRecent, movementMatrices, predictionModelNames, allcasedata, districts, countries, country_borders, aucmatrix) {
	cl <- makeCluster(8, outfile="out.log")
	registerDoParallel(cl)

	foreach(idx=4:mostRecent, .packages=c("aqfig", "raster", "doParallel", "foreach"), .export=c("getData", "plotRisks", "plotMap")) %dopar% {
		source('palettes.R')
		source('diseaseMapping.R')
		riskData <- getRiskData(movementMatrices, predictionModelNames, allcasedata, idx)
		weighted_riskdata <- calculateWeightedRisks(riskData, aucmatrix[(idx-3):(idx-1),], predictionModelNames)

		# plot
		plotRisks(districts, countries, country_borders, weighted_riskdata, riskData[1][[1]]$reportedCases, plotTitle="", filename=paste(formatC(idx, width=2, flag="0"), "regional_prediction_weighted", sep="_"))
	}
stopCluster(cl)
}
