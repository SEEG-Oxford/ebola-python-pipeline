require(aqfig)
require(leafletR)
require(rgeos)
require(RColorBrewer)

getColors <- function(a, b, samples, vals, ramp) {
	col_range <- seq(a, b, length.out = samples)
	cols <- ramp(samples)  
	bins <- cut(vals, col_range, include.lowest = TRUE)
	return(cols[bins])
}

plotLeafletMap <- function(vals, districts, dirName, regionColours) {
	districts$risk <- vals
	q.dat <- toGeoJSON(data=districts, dest=dirName, name="districts")
	q.style <- styleCat(prop="ID", val=seq(0,248), style.val=regionColours, lwd=1, leg="a")
	q.map <- leaflet(data=q.dat, dest=dirName, title="Regional Risk", base.map=list("positron", "darkmatter", "mqsat", "tls", "osm"), style=q.style, popup="*", controls=list("zoom", "scale", "layer"))
	q.map
}

plotRegionalRisks <- function(districts, countries, country_borders, predictedRegions, reportedCases, plotTitle, filename, leaflet=FALSE) {
	vals <- rep(NA, nrow(districts))
	for(idx in 1:length(predictedRegions)) {	
		vals[match(names(predictedRegions[idx]), paste(districts$COUNTRY_ID,districts$NAME, sep='_'))] <- predictedRegions[idx]
	}
	regionColours <- getColors(0, 1, 1000, vals, seqRamp('YlOrRd'))  
	regionColours[paste(districts$COUNTRY_ID,districts$NAME, sep='_') %in% names(reportedCases)] <- "#33D3FF"
	legendColors <- seqRamp('YlOrRd')(1000)
	legendRange <- c(0,1)	
	
	newfilename <- paste(filename, ".png", sep="")
	png(filename=newfilename, width=800, height=700, units='px', pointsize=20)
	if(leaflet) {
		plotMap(districts,
				countries,
				country_borders,
				plotTitle,
				regionColours,
				legendColors,
				legendRange)
	}
	dev.off()
	if(leaflet) {
		plotLeafletMap(vals, districts, dirname(newfilename), regionColours)
	}
	png(filename=paste(filename, "large.png", sep="_"), width=8000, height=7000, units='px', pointsize=100)
	plotMap(districts,
			countries,
			country_borders,
			plotTitle,
			regionColours,
			legendColors,
			legendRange)
	dev.off()
}

plotCompositeLeaflet <- function(districts, riskList) {
	for(i in 1:12) {
		vals <- getVals(districts, riskList[[i]])
		
		districts$risk <- vals
	}
	districts$risk1 <- getVals(districts, riskList[[1]])
	districts$risk2 <- getVals(districts, riskList[[2]])
	districts$risk3 <- getVals(districts, riskList[[3]])
	districts$risk4 <- getVals(districts, riskList[[4]])
	districts$risk5 <- getVals(districts, riskList[[5]])
	districts$risk6 <- getVals(districts, riskList[[6]])
	districts$risk7 <- getVals(districts, riskList[[7]])
	districts$risk8 <- getVals(districts, riskList[[8]])
	districts$risk9 <- getVals(districts, riskList[[9]])
	districts$risk10 <- getVals(districts, riskList[[10]])
	districts$risk11 <- getVals(districts, riskList[[11]])
	districts$risk12 <- getVals(districts, riskList[[12]])
	q.dat <- toGeoJSON(data=districts, name="weighted-districts")	
}

getVals <- function(districts, risk) {
	col_range <- seq(0, 1, length.out = 1000)
	cols <- seqRamp('YlOrRd')(1000)
	vals <- rep(NA, nrow(districts))
	predictedRegions <- risk$predictedRegions
	reportedCases <- risk$reportedCases
	for(idx in 1:length(predictedRegions)) {	
		vals[match(names(predictedRegions[idx]), paste(districts$COUNTRY_ID,districts$NAME, sep='_'))] <- predictedRegions[idx]
	}
	return(vals)
}

plotGlobalMap <- function (vals,
                     countries,
                     all_countries,
                     zlim = range(vals),
                     ramp = seqRamp(),
                     n = 1000,
					 maptitle,
					 dir_name=NA,
					 filename) {
  # get the colours
  countryColors <- getColors(zlim[1], zlim[2], n, vals, ramp)
  # special cases for LBR, SLE and GIN
  countryColors[grep("LBR|GIN|SLE", countries$admin0_COU)] <- "#33D3FF"
  countryColors[is.na(countryColors)] <- grey(0.9)
  
  par(mar=c(1,1,2,2))
  plot(all_countries[-9,], col = grey(0.9))
  plot(countries, col = countryColors, border = 'black', lwd = 1,add=TRUE)
  vertical.image.legend(col=seqRamp('YlOrRd')(1000),zlim=c(0,1))
  title(main=maptitle)
  
  if(!is.na(dir_name)) {
	  countryData <- countries@data
	  countries <- gSimplify(countries, tol=0.05, topologyPreserve=TRUE)
	  countries <- SpatialPolygonsDataFrame(countries, countryData, match.ID=FALSE)
	  #countries$risk <- vals
	  q.dat <- toGeoJSON(data=countries, dest=dir_name, name="countries")
	  q.style <- styleCat(prop="ID", val=seq(0,190), style.val=countryColors, lwd=1, leg="a")
	  q.map <- leaflet(data=q.dat, dest=dir_name, title=filename, base.map=list("positron", "darkmatter", "mqsat", "tls", "osm"), style=q.style, popup="*", controls=list("zoom", "scale", "layer"))
	  q.map
  }
  
}

plotGlobalRisks <- function(risks, countries, all_countries, filename, maptitle) {
	nonCoreRisks <- risks[risks$country != "LBR" & risks$country != "GIN" & risks$country != "SLE",]

	# scale the risks between 0 and 1
	nonCoreRisks$risk = as.numeric(as.vector(nonCoreRisks$risk)) / max(as.numeric(as.vector(nonCoreRisks$risk)))

	vals <- rep(NA, length(countries$admin0_COU))

	for(idx in 1:nrow(nonCoreRisks)) {
		vals[match(as.character(nonCoreRisks[idx,1]), countries$admin0_COU)] <- as.numeric(as.vector(nonCoreRisks[idx,2]))
	}

	newfilename <- paste(filename, "large.png", sep="_")
	png(filename=newfilename, width=8000, height=4000, units='px', pointsize=100)
	plotGlobalMap(vals,
			countries,
			all_countries,
			zlim = c(0, 1),
			ramp = seqRamp('YlOrRd'),
			maptitle=maptitle,
			dir_name = dirname(newfilename),
			filename = filename
			)
	dev.off()
	png(filename=paste(filename, ".png", sep=""), width=800, height=400, units='px', pointsize=20)
	plotGlobalMap(vals,
			countries,
			all_countries,
			zlim = c(0, 1),
			ramp = seqRamp('YlOrRd'),
			maptitle=maptitle,
			filename = filename)
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

plotHistoricCases <- function(districts, countries, country_borders, predictedRegions, reportedCases, plotTitle, filename, upperLimit) {
	vals <- rep(NA, nrow(districts))
	for(idx in 1:length(predictedRegions)) {	
		vals[match(names(predictedRegions[idx]), paste(districts$COUNTRY_ID,districts$NAME, sep='_'))] <- predictedRegions[idx]
	}
	
	redPalette <- colorRampPalette(c("#fcbba1","#fc9272","#fb6a4a","#ef3b2c","#cb181d","#a50f15","#67000d"))
	
	regionColours <- getColors(0, upperLimit, 1000, vals, redPalette)  
	regionColours[paste(districts$COUNTRY_ID,districts$NAME, sep='_') %in% names(reportedCases)] <- grey(0.7)
	legendColors <- redPalette(1000)
	legendRange <- c(0,upperLimit)
	
	png(filename=paste(filename, ".png", sep=""), width=800, height=700, units='px', pointsize=20)
	plotMap(districts,
			countries,
			country_borders,
			plotTitle,
			regionColours,
			legendColors,
			legendRange)
	dev.off()
}

plotMap <- function (districts,
                     countries,
                     country_borders,
					 plotTitle,
					 objectColors,
					 legendColors,
					 legendRange) {
  par(mar=c(1,1,2,2))
  plot(countries, col = grey(0.9), border = 'white', lwd = 3)
  plot(districts, col = objectColors, border = 'white', add = TRUE)
  plot(country_borders, col = grey(0.4), add = TRUE)
  vertical.image.legend(col=legendColors,zlim=legendRange)
  title(main=plotTitle)  
}

plotRisks <- function(districts, countries, country_borders, predictedRegions, reportedCases, plotTitle, filename) {
	vals <- rep(NA, nrow(districts))
	for(idx in 1:length(predictedRegions)) {	
		vals[match(names(predictedRegions[idx]), paste(districts$COUNTRY_ID,districts$NAME, sep='_'))] <- predictedRegions[idx]
	}
	
	regionColours <- getColors(0, 1, 1000, vals, seqRamp('YlOrRd'))  
	regionColours[paste(districts$COUNTRY_ID,districts$NAME, sep='_') %in% names(reportedCases)] <- grey(0.7)
	legendColors <- seqRamp('YlOrRd')(1000)
	legendRange <- c(0,1)
	
	png(filename=paste(filename, ".png", sep=""), width=800, height=700, units='px', pointsize=20)
	plotMap(districts,
			countries,
			country_borders,
			plotTitle,
			regionColours,
			legendColors,
			legendRange)
	dev.off()
}

# convert an RColorBrewer div palette into a colour ramp
divRamp <- function(name = c("Spectral", "RdYlGn", "RdYlBu", "RdGy", "RdBu",
                             "PuOr", "PRGn", "PiYG", "BrBG")) {
  
  # match the name
  name <- match.arg(name)
  
  # fetch the palette
  pal <- brewer.pal(n = 11, name = name)
  
  # convert to a ramp
  ramp <- colorRampPalette(pal)
  
  # return this
  return (ramp)
  
}

# convert an RColorBrewer seq palette into a colour ramp
seqRamp <- function(name = c("YlOrRd", "YlOrBr", "YlGnBu", "YlGn", "Reds",
                             "RdPu", "Purples", "PuRd", "PuBuGn", "PuBu",
                             "OrRd", "Oranges", "Greys", "Greens", "GnBu",
                             "BuPu", "BuGn", "Blues")) {
  
  # match the name
  name <- match.arg(name)
  
  # fetch the palette
  pal <- brewer.pal(n = 9, name = name)
  
  # convert to a ramp
  ramp <- colorRampPalette(pal)
  
  # return this
  return (ramp)
  
}