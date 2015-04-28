require(aqfig)
require(leafletR)
source('../palettes.R')

plotRegionalMap <- function (vals,
                     districts,
                     countries,
                     country_borders,
                     zlim = range(vals),
                     ramp = seqRamp(),
                     n = 1000,
					 reportedCases,
					 plotTitle,
					 dir_name=NA) {
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
  if(!is.na(dir_name)) {
	  print("test")
	  districts$risk <- vals
	  q.dat <- toGeoJSON(data=districts, dest=dir_name, name="districts")
	  q.style <- styleCat(prop="ID", val=seq(0,248), style.val=regionColours, lwd=1, leg="a")
	  q.map <- leaflet(data=q.dat, dest=dir_name, title="Regional Risk", base.map=list("positron", "darkmatter", "mqsat", "tls", "osm"), style=q.style, popup="*", controls=list("zoom", "scale", "layer"))
	  q.map
  }
}

plotRegionalRisks <- function(districts, countries, country_borders, predictedRegions, reportedCases, plotTitle, filename) {
	vals <- rep(NA, nrow(districts))
	for(idx in 1:length(predictedRegions)) {	
		vals[match(names(predictedRegions[idx]), paste(districts$COUNTRY_ID,districts$NAME, sep='_'))] <- predictedRegions[idx]
	}
	newfilename <- paste(filename, ".png", sep="")
	png(filename=newfilename, width=800, height=700, units='px', pointsize=20)
	plotRegionalMap(vals,
			districts,
			countries,
			country_borders,
			zlim = c(0, 1),
			ramp = seqRamp('YlOrRd'),
			reportedCases = reportedCases,
			plotTitle = plotTitle,
			dir_name = dirname(newfilename))
	dev.off()
	png(filename=paste(filename, "large.png", sep="_"), width=8000, height=7000, units='px', pointsize=100)
	plotRegionalMap(vals,
			districts,
			countries,
			country_borders,
			zlim = c(0, 1),
			ramp = seqRamp('YlOrRd'),
			reportedCases = reportedCases,
			plotTitle = plotTitle)
	dev.off()
}

plotGlobalMap <- function (vals,
                     countries,
                     all_countries,
                     zlim = range(vals),
                     ramp = seqRamp(),
                     n = 1000,
					 maptitle) {
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

plotGlobalRisks <- function(risks, countries, all_countries, filename, maptitle) {
	nonCoreRisks <- risks[risks$country != "LBR" & risks$country != "GIN" & risks$country != "SLE",]

	# scale the risks between 0 and 1
	nonCoreRisks$risk = as.numeric(as.vector(nonCoreRisks$risk)) / max(as.numeric(as.vector(nonCoreRisks$risk)))

	vals <- rep(NA, length(countries$admin0_COU))

	for(idx in 1:nrow(nonCoreRisks)) {
		vals[match(as.character(nonCoreRisks[idx,1]), countries$admin0_COU)] <- as.numeric(as.vector(nonCoreRisks[idx,2]))
	}

	png(filename=paste(filename, "large.png", sep="_"), width=8000, height=4000, units='px', pointsize=100)
	plotGlobalMap(vals,
			countries,
			all_countries,
			zlim = c(0, 1),
			ramp = seqRamp('YlOrRd'),
			maptitle=maptitle)
	dev.off()
	png(filename=paste(filename, ".png", sep=""), width=800, height=400, units='px', pointsize=20)
	plotGlobalMap(vals,
			countries,
			all_countries,
			zlim = c(0, 1),
			ramp = seqRamp('YlOrRd'),
			maptitle=maptitle)
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

plotHistoricCases <- function(districts, countries, country_borders, predictedRegions, reportedCases, plotTitle, filename) {
	vals <- rep(NA, nrow(districts))
	for(idx in 1:length(predictedRegions)) {	
		vals[match(names(predictedRegions[idx]), paste(districts$COUNTRY_ID,districts$NAME, sep='_'))] <- predictedRegions[idx]
	}
	png(filename=paste(filename, ".png", sep=""), width=800, height=700, units='px', pointsize=20)
	plotHistoricCaseMap(vals,
			districts,
			countries,
			country_borders,
			zlim = c(0, max(log(allcasedata[,-1]))),
			ramp = colorRampPalette(c("#fcbba1","#fc9272","#fb6a4a","#ef3b2c","#cb181d","#a50f15","#67000d")),#seqRamp('Reds'), #c("#fff5f0","#fee0d2","#fcbba1","#fc9272","#fb6a4a","#ef3b2c","#cb181d","#a50f15","#67000d")
			reportedCases = reportedCases,
			plotTitle = plotTitle)
	dev.off()
}

plotHistoricCaseMap <- function (vals,
                     districts,
                     countries,
                     country_borders,
                     zlim = range(vals),
                     ramp = seqRamp(),
                     n = 1000,
					 reportedCases,
					 plotTitle) {
# get the colours
  col_range <- seq(zlim[1], zlim[2], length.out = n)
  cols <- ramp(n)
  
  bins <- cut(vals, col_range, include.lowest = TRUE)
  regionColours <- cols[bins]

  newbins <- cut(log(reportedCases), col_range, include.lowest = TRUE)
  newcols <- cols[newbins]
  
  regionColours[paste(districts$COUNTRY_ID,districts$NAME, sep='_') %in% names(reportedCases)] <- newcols
  
  par(mar=c(1,1,2,2))
  plot(countries, col = gray(0.9), border = 'white', lwd = 3)
  plot(districts, col = regionColours, border = 'white', add = TRUE)
  plot(country_borders, col = grey(0.4), add = TRUE)
  title(main=plotTitle)  
}