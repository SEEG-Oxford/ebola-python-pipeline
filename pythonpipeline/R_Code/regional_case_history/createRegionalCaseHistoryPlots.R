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

evdcasedata <- read.csv('../../data/EVD_conf_prob_.csv')
# this must be exactly the same format as allcasedata and will also need curating
# when cases move from the sitrep to the patientdb
additionalcasedata <- read.csv('../../data/EVD_conf_prob_additional.csv')

allcasedata <- evdcasedata + additionalcasedata

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

  newbins <- cut(log(reportedCases), col_range, include.lowest = TRUE)
  newcols <- cols[newbins]
  
  regionColours[paste(districts$COUNTRY_ID,districts$NAME, sep='_') %in% names(reportedCases)] <- newcols #"#FF0000"
  
  par(mar=c(1,1,2,2))
  plot(countries, col = gray(0.9), border = 'white', lwd = 3)
  plot(districts, col = regionColours, border = 'white', add = TRUE)
  plot(country_borders, col = grey(0.4), add = TRUE)
  # vertical.image.legend(col=seqRamp('YlOrRd')(1000),zlim=c(0,1))
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
			zlim = c(0, max(log(allcasedata[,-1]))),
			ramp = colorRampPalette(c("#fcbba1","#fc9272","#fb6a4a","#ef3b2c","#cb181d","#a50f15","#67000d")),#seqRamp('Reds'), #c("#fff5f0","#fee0d2","#fcbba1","#fc9272","#fb6a4a","#ef3b2c","#cb181d","#a50f15","#67000d")
			reportedCases = reportedCases,
			plotTitle = plotTitle)
	dev.off()
}

plotDate <- function(datestring, filename) {
	par(mar = c(0,0,0,0))
	png(filename=paste(filename, ".png", sep=""), width=800, height=200, units='px', pointsize=18)
		plot.new()
		title(main=datestring, col.main=grey(0.4))
		dev.off()
	par(mar = c(5, 4, 4, 2) + 0.1)
}

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
plotRisks(districts, countries, country_borders, 0, getSimpleData(idx), plotTitle="", filename=paste(formatC(idx, width=2, flag="0"), "regional_cases_week", sep="_"))

}
