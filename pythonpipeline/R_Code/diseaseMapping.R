require(aqfig)
require(leafletR)
require(rgeos)
require(RColorBrewer)
require(foreach)
require(doParallel)

getRiskData <- function(movementMatrices, predictionModelNames, caseData, sampleOfInterest) {
	# work out how many risk models we have to deal with
	riskModelCount <- dim(movementMatrices)[3]

	riskData <- rep(NULL, riskModelCount)

	for (i in 1:riskModelCount) {
		riskData[[i]] <- getData(movementMatrices[,,i], sampleOfInterest, predictionModelNames[i], caseData, auc=FALSE)
	}
	
	return(riskData)
}

plotAllRegionalRisks <- function(riskData, districts, countries, country_borders, regionalRiskTitles, regionNames, ramp) {
	riskModelCount <- length(riskData)
	for (i in 1:riskModelCount) {
		plotRegionalRisks(districts, countries, country_borders, riskData[[i]]$predictedRegions, riskData[[i]]$reportedCases, regionalRiskTitles[i], paste(sep='_', "regional_prediction", riskData[[i]]$name), regionNames, ramp, FALSE)
	}
}

calculateAUCMatrix <- function(movementMatrices, predictionModelNames, caseData, sampleOfInterest, coreRegex) {
	# work out how many risk models we have to deal with
	riskModelCount <- dim(movementMatrices)[3]
	
	cl <- makeCluster(8)
	registerDoParallel(cl)

	aucmatrix <- foreach(idx=1:sampleOfInterest,.combine="rbind", .export=c("getData", "calcAUC", "desc_integrate")) %dopar% {
		# we sometimes(?) need to manually track the indices if using a parallel library
		aucresult <- numeric()
		aucresult[1] <- idx
		for (i in 1:riskModelCount) {
			riskdata1 <- getData(movementMatrices[,,i], idx, predictionModelNames[i], caseData, coreRegex=coreRegex)
			aucresult[i+1] <- riskdata1$AUC
		}
		
		aucresult
	}
	stopCluster(cl)

	# convert the results back into a numeric matrix rather than a matrix of lists
	aucmatrix <- matrix(as.numeric(unlist(aucmatrix)),nrow=nrow(aucmatrix))
	return(aucmatrix)
}

calculateWeightedRisks <- function(riskData, aucs, outfile=TRUE) {
	# get the average ignore NaNs, nulls etc
	avgauc <- colMeans(aucs[,c(-1)], na.rm = TRUE)
	
	aucCount <- length(avgauc)
	cl <- makeCluster(8)
	registerDoParallel(cl)
	weighted_riskdata <- foreach (i=1:aucCount, .combine='+') %dopar% {
		riskData[[i]]$predictedRegions * avgauc[i]
	}
	
	stopCluster(cl)
	
	# normalise 0..1
	# if the weightings are all zero then don't do anything (means AUC calculation was really bad)
	if(max(weighted_riskdata) > 0) {
		weighted_riskdata <- weighted_riskdata / max(weighted_riskdata)
	}
	if(outfile) {
		write.csv((avgauc / max(avgauc)) /sum(avgauc), "weightings.csv")
	}
	return(weighted_riskdata)
}

createRegionalCaseHistoryMaps <- function(caseData, districts, countries, country_borders, finalSample, regionNames, ramp) {
	foreach(idx=1:finalSample,.packages=c("aqfig"),.export=c("getSimpleData", "plotRisks", "plotMap", "seqRamp", "getColors", "plotDate", "plotHistoricCases")) %do% {
		plotHistoricCases(districts, countries, country_borders, 0, getSimpleData(idx, caseData), plotTitle="", filename=paste(formatC(idx, width=2, flag="0"), "regional_cases_week", sep="_"), max(log(caseData[,-1])), regionNames, ramp)
	}
}

createRegionalPredictionHistoryMaps <- function(finalSample, movementMatrices, predictionModelNames, caseData, districts, countries, country_borders, aucmatrix, regionNames, ramp) {
	cl <- makeCluster(8, outfile="out.log")
	registerDoParallel(cl)

	foreach(idx=4:finalSample, .packages=c("aqfig", "raster", "doParallel", "foreach", "RColorBrewer"), .export=c("getData", "plotRisks", "plotMap", "seqRamp", "getColors")) %dopar% {
		source('diseaseMapping.R')
		riskData <- getRiskData(movementMatrices, predictionModelNames, caseData, idx)
		weighted_riskdata <- calculateWeightedRisks(riskData, aucmatrix[(idx-3):(idx-1),], FALSE)

		# plot
		plotRisks(districts, countries, country_borders, weighted_riskdata, riskData[1][[1]]$reportedCases, plotTitle="", filename=paste(formatC(idx, width=2, flag="0"), "regional_prediction_weighted", sep="_"), regionNames, ramp)
	}
	stopCluster(cl)
}

# helper function
as.movementmatrix <- function(dataframe) {
	nrows <- length(unique(dataframe[1])[,])
	ncols <- length(unique(dataframe[2])[,])
	if(nrows != ncols) {
		stop ("Error: Expected a square matrix!")
	}
	
	mat <- matrix(ncol = ncols, nrow = nrows, dimnames = list((unique(dataframe[1])[,]),(unique(dataframe[2])[,])))
	for(idx in 1:nrow(dataframe)) {
		mat[as.character(dataframe[idx,2]),as.character(dataframe[idx,1])] <- dataframe[idx,3]
	}
	
	# correct potential data issues
	mat[is.na(mat)] <- 0
	diag(mat) <- 0
	
	mat <- mat[order(rownames(mat)),]
	mat <- mat[,order(colnames(mat))]

	return (mat)
}

# pull out origin, destination and radiation with selection_france
getData <- function(raw_movement_matrix, sampleOfInterest, name, allCaseData, auc=TRUE, coreRegex=NULL) {
	AUC=NULL
	startWeek <- sampleOfInterest-3
	if (startWeek < 1) startWeek <- 0
	casedata <- colSums(allCaseData[c(startWeek:(sampleOfInterest-1)),][,-1])
	
	for(idx in 1:nrow(raw_movement_matrix)) {
		nametofind <- gsub("\\s", ".", row.names(raw_movement_matrix)[idx])
		casecount <- casedata[match(nametofind, gsub("[.]","_",names(casedata)))]
		raw_movement_matrix[idx,] <- (raw_movement_matrix[idx,] * casecount)
	}

	raw_movement_matrix[is.na(raw_movement_matrix)] <- 0
	summedRegions <- colSums(raw_movement_matrix)
	summedRegions[is.na(summedRegions)] <- 0
	summedRegions <- summedRegions / max(summedRegions)

	# these are the "core" districts
	reportedCases <- casedata[casedata > 0]

	predictedRegions <- summedRegions[!(names(summedRegions) %in% names(reportedCases))]
	predictedRegions = predictedRegions / max(predictedRegions)
	
	if(auc) {
		# calculate AUC
		# select the week being predicted
		weekbeingpredicted <- allCaseData[sampleOfInterest,][,-1]
		# make it logical (either there are cases or not)
		weekbeingpredicted[weekbeingpredicted > 0] <- 1
		
		# remove regions where the are already cases to prevent skewing the data
		weekbeingpredicted <- weekbeingpredicted[!(names(weekbeingpredicted) %in% names(reportedCases))]
		
		predictions <- summedRegions[grep(coreRegex, names(summedRegions))]
		predictions <- predictions[!(names(predictions) %in% names(reportedCases))]
		
		AUC <- calcAUC(data.frame(weekbeingpredicted), data.frame(predictions),plot=FALSE,error_bars=FALSE,ci=0.95,res=100,main=name)
	}
	
	return (list(reportedCases=reportedCases, predictedRegions=predictedRegions, AUC=AUC, name=name))
}

getSimpleData <- function(sampleOfInterest, allCaseData) {
	startWeek <- sampleOfInterest-2
	if(startWeek < 0) startWeek <- 0
	casedata <- colSums(allCaseData[c(startWeek:sampleOfInterest),][,-1])
	
	# these are the "core" districts
	reportedCases <- casedata[casedata > 0]
	
	return (reportedCases)
}

###################################################
##
## Functions for calculating AUC and plotting ROC
## Corey Chivers, 2013
## corey.chivers@mail.mcgill.ca
##
###################################################


## Discrete integration for AUC calc
## ?x.y1 + 1/2?x.?y  <- summation of trapezoids
desc_integrate<-function(x,y)
{
   f<-cbind(x,y)
   ## Sort by x, then by y (ascending)
   f<-f[order(f[,1],f[,2]),] 
   dint<-0
   x<-f[,1]
   y<-f[,2]
   dint<-sapply(2:length(x),function(i){
      (x[i]-x[i-1])*y[i-1] + 0.5*(x[i]-x[i-1]) * (y[i]-y[i-1])})
   dint<-sum(dint)
   return(dint)
}

## This is a handy generic.
add_error_bars<-function(data,error,dimensions=1,...)
{
   for(i in 1:length(data[,1]))
   {
      # y axis is 1st dimension
      arrows(data[i,1],data[i,2],data[i,1],error[i,1],angle=90,...)
      arrows(data[i,1],data[i,2],data[i,1],error[i,2],angle=90,...)

      if(dimensions==2)
      {
         arrows(data[i,1],data[i,2],error[i,3],data[i,2],angle=90,...)
         arrows(data[i,1],data[i,2],error[i,4],data[i,2],angle=90,...)
      }
   }
}


####################################################################
## Calculate the AUC and optionally plot the ROC
##  **Usage**
## d: a vector of logicals (0,1)
## pred: a vector of predicted values on range [0,1]
## plot: logical - plot or not
## error_bars: logical - add error bars or not
## ci: atomic vector - confidence interval width for error bars
## res: atomic vector - resolution of the thresholds to test
####################################################################
calcAUC<-function(d,pred,plot=FALSE,error_bars=FALSE,ci=0.95,res=100,add=FALSE,...)
{
   n<-length(d)
   dt<-seq(0,1,length.out=res)
   tp<-numeric(res)
   fp<-numeric(res)
   fn<-numeric(res)
 
   error<-array(dim=c(res,4)) # <tp upper, tp lower, fp upper, fp lower>
   sapply(1:res,function(i)
   {
      tp[i]<<- sum( d[pred > dt[i] ] )/ sum(d)
      fp[i]<<- sum( d[pred > dt[i] ] == 0 )/ sum(!d)
      fn[i]<<- sum( d[pred < dt[i] ] )/ sum(d) 
   

      #Calculate CI based on the beta distribution
      alpha_tp<-sum( d[pred > dt[i] ] ) + 1
      beta_tp<- sum(d) - sum( d[pred > dt[i] ] ) + 1
      error[i,1]<<-qbeta((1-ci)/2,alpha_tp,beta_tp)   #ci% bounds based on beta dist
      error[i,2]<<-qbeta(1-(1-ci)/2,alpha_tp,beta_tp)

      alpha_fp<- sum( d[pred > dt[i] ] == 0 ) + 1
      beta_fp<- sum(!d) - sum( d[pred > dt[i] ] == 0 ) + 1
      error[i,3]<<-qbeta((1-ci)/2,alpha_fp,beta_fp)   #ci% bounds based on beta dist
      error[i,4]<<-qbeta(1-(1-ci)/2,alpha_fp,beta_fp)
   })

   # Which threshold value minimises
   # the sum of the error rates.
   opt_thresh<-dt[which.min(fp+fn)]

   # Ensure collisions at 0,0 and 1,1
   fp<-c(1,fp,0)
   tp<-c(1,tp,0)

   # Integrate the ROC
   auc<-desc_integrate(fp,tp)

   if(plot)
   {
      if(add)
      {
         lines(fp,tp,type='b',pch=20)
      }else{
         plot(fp,tp,type='b',pch=20,xlim=c(0,1),ylim=c(0,1),...)
         text( 0.8,0.2,paste('AUC =',round(auc,3)) )
         abline(0,1,lty=2)
      }

      if(error_bars)
         add_error_bars(cbind(fp[2:(res+1)],tp[2:(res+1)]),error,dimensions=2,length=0.01)
   }
   if(is.na(auc)) {
	auc <- 0
   }
   return(list(auc=auc,opt_thresh=opt_thresh))
}


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

plotRegionalRisks <- function(districts, countries, country_borders, predictedRegions, reportedCases, plotTitle, filename, regionNames, ramp, leaflet=FALSE) {
	vals <- rep(NA, nrow(districts))
	for(idx in 1:length(predictedRegions)) {	
		vals[match(names(predictedRegions[idx]), regionNames)] <- predictedRegions[idx]
	}
	regionColours <- getColors(0, 1, 1000, vals, ramp)  
	regionColours[regionNames %in% names(reportedCases)] <- "#33D3FF"
	legendColors <- ramp(1000)
	legendRange <- c(0,1)	
	
	newfilename <- paste(filename, ".png", sep="")
	png(filename=newfilename, width=800, height=700, units='px', pointsize=20)
	plotMap(districts,
			countries,
			country_borders,
			plotTitle,
			regionColours,
			legendColors,
			legendRange)
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

plotCompositeLeaflet <- function(districts, riskList, ramp, regionNames) {
	riskCount <- length(riskList)
	initialNames <- names(districts@data)
	addedNames <- NULL
	for(i in 1:riskCount) {
		vals <- getVals(districts, riskList[[i]], ramp, regionNames)
		
		districts@data[length(initialNames)+i] <- vals
		addedNames[i] <- paste(sep="", "risk", i)		
	}
	names(districts@data) <- c(initialNames, addedNames)
	q.dat <- toGeoJSON(data=districts, name="weighted-districts")	
}

getVals <- function(districts, risk, ramp, regionNames) {
	col_range <- seq(0, 1, length.out = 1000)
	cols <- ramp(1000)
	vals <- rep(NA, nrow(districts))
	predictedRegions <- risk$predictedRegions
	reportedCases <- risk$reportedCases
	for(idx in 1:length(predictedRegions)) {	
		vals[match(names(predictedRegions[idx]), regionNames)] <- predictedRegions[idx]
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
					 filename,
					 coreRegex) {
  # get the colours
  countryColors <- getColors(zlim[1], zlim[2], n, vals, ramp)
  # special cases for LBR, SLE and GIN
  countryColors[grep(coreRegex, countries$admin0_COU)] <- "#33D3FF"
  countryColors[is.na(countryColors)] <- grey(0.9)
  
  par(mar=c(1,1,2,2))
  plot(all_countries[-9,], col = grey(0.9))
  plot(countries, col = countryColors, border = 'black', lwd = 1,add=TRUE)
  vertical.image.legend(col=ramp(1000),zlim=c(0,1))
  title(main=maptitle)
  
  if(!is.na(dir_name)) {
	  countryData <- countries@data
	  countries <- gSimplify(countries, tol=0.05, topologyPreserve=TRUE)
	  countries <- SpatialPolygonsDataFrame(countries, countryData, match.ID=FALSE)
	  q.dat <- toGeoJSON(data=countries, dest=dir_name, name="countries")
	  q.style <- styleCat(prop="ID", val=seq(0,190), style.val=countryColors, lwd=1, leg="a")
	  q.map <- leaflet(data=q.dat, dest=dir_name, title=filename, base.map=list("positron", "darkmatter", "mqsat", "tls", "osm"), style=q.style, popup="*", controls=list("zoom", "scale", "layer"))
	  q.map
  }
  
}

plotGlobalRisks <- function(risks, countries, all_countries, filename, maptitle, allCountryNames, ramp, coreRegex) {
	# scale the risks between 0 and 1
	risks$risk = as.numeric(as.vector(risks$risk)) / max(as.numeric(as.vector(risks$risk)))

	vals <- rep(NA, length(allCountryNames))

	for(idx in 1:nrow(risks)) {
		vals[match(as.character(risks[idx,1]), allCountryNames)] <- as.numeric(as.vector(risks[idx,2]))
	}

	newfilename <- paste(filename, "large.png", sep="_")
	png(filename=newfilename, width=8000, height=4000, units='px', pointsize=100)
	plotGlobalMap(vals,
			countries,
			all_countries,
			zlim = c(0, 1),
			ramp = ramp,
			maptitle=maptitle,
			dir_name = dirname(newfilename),
			filename = filename,
			coreRegex = coreRegex
			)
	dev.off()
	png(filename=paste(filename, ".png", sep=""), width=800, height=400, units='px', pointsize=20)
	plotGlobalMap(vals,
			countries,
			all_countries,
			zlim = c(0, 1),
			ramp = ramp,
			maptitle=maptitle,
			filename = filename,
			coreRegex = coreRegex)
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

plotHistoricCases <- function(districts, countries, country_borders, predictedRegions, reportedCases, plotTitle, filename, upperLimit, regionNames, ramp) {
	vals <- rep(NA, nrow(districts))
	for(idx in 1:length(predictedRegions)) {	
		vals[match(names(predictedRegions[idx]), regionNames)] <- predictedRegions[idx]
	}
	regionColours <- getColors(0, upperLimit, 1000, vals, ramp)  
	regionColours[regionNames %in% names(reportedCases)] <- grey(0.7)
	legendColors <- ramp(1000)
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

plotRisks <- function(districts, countries, country_borders, predictedRegions, reportedCases, plotTitle, filename, regionNames, ramp) {
	vals <- rep(NA, nrow(districts))
	for(idx in 1:length(predictedRegions)) {	
		vals[match(names(predictedRegions[idx]), regionNames)] <- predictedRegions[idx]
	}
	regionColours <- getColors(0, 1, 1000, vals, ramp)  
	regionColours[regionNames %in% names(reportedCases)] <- grey(0.7)
	legendColors <- ramp(1000)
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
