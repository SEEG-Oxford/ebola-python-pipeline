# functions (and examples) to plot maps of human movement and EVD spread

# load packages
require(raster)
require(SDMTools)
require(rgdal)
require(aqfig)
source('palettes.R')

plotMap <- function (vals,
                     countries,
                     all_countries,
                     zlim = range(vals),
                     ramp = seqRamp(),
                     n = 1000,
					 maptitle) {
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
  
  countryColors <- cols[bins]
  # special cases for LBR, SLE and GIN
  countryColors[grep("LBR|GIN|SLE", countries$admin0_COU)] <- "#33D3FF"
  
  par(mar=c(1,1,2,2))
  plot(all_countries[-9,], col = grey(0.9))
  plot(countries, col = countryColors, border = 'black', lwd = 1,add=TRUE)
  vertical.image.legend(col=seqRamp('YlOrRd')(1000),zlim=c(0,1))
  title(main=maptitle)
  
  #
  
}

plotRisks <- function(risks, countries, all_countries, filename, maptitle) {
	nonCoreRisks <- risks[risks$country != "LBR" & risks$country != "GIN" & risks$country != "SLE",]

	# scale the risks between 0 and 1
	nonCoreRisks$risk = as.numeric(as.vector(nonCoreRisks$risk)) / max(as.numeric(as.vector(nonCoreRisks$risk)))

	vals <- rep(NA, length(countries$admin0_COU))

	for(idx in 1:nrow(nonCoreRisks)) {
		vals[match(as.character(nonCoreRisks[idx,1]), countries$admin0_COU)] <- as.numeric(as.vector(nonCoreRisks[idx,2]))
	}

	png(filename=paste(filename, "large.png", sep="_"), width=8000, height=4000, units='px', pointsize=100)
	plotMap(vals,
			countries,
			all_countries,
			zlim = c(0, 1),
			ramp = seqRamp('YlOrRd'),
			maptitle=maptitle)
	dev.off()
	png(filename=paste(filename, ".png", sep=""), width=800, height=400, units='px', pointsize=20)
	plotMap(vals,
			countries,
			all_countries,
			zlim = c(0, 1),
			ramp = seqRamp('YlOrRd'),
			maptitle=maptitle)
	dev.off()
}


# load shapefiles
countries <- shapefile('infoRM_countries.shp')
adm0 <- shapefile('admin2013_0.shp')

source("calculateRisks.R")

countrycodes <- as.vector(all$country.1)

risks <- data.frame(country=countrycodes, risk=all$importation_risk)
plotRisks(risks, countries, adm0, "global_Overall_prediction", "Global relative risk of Ebola importation from overall model")

risks <- data.frame(country=countrycodes, risk=all$adjacency_relative)
plotRisks(risks, countries, adm0, "global_Adjacency_prediction", "Global relative risk of Ebola importation from Adjacency model")

risks <- data.frame(country=countrycodes, risk=all$gravity_relative)
plotRisks(risks, countries, adm0, "global_Gravity_prediction", "Global relative risk of Ebola importation from Gravity model")

risks <- data.frame(country=countrycodes, risk=all$migration_relative)
plotRisks(risks, countries, adm0, "global_Migration_prediction", "Global relative risk of Ebola importation from Migration model")

