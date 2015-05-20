plotAllRegionalRisks <- function(movementMatrices, predictionModelNames, regionalRiskTitles, allCaseData) {
	# work out how many risk models we have to deal with
	riskModelCount <- dim(movementMatrices)[3]

	riskData <- rep(NULL, riskModelCount)

	for (i in 1:riskModelCount) {
		riskData[[i]] <- getData(movementMatrices[,,i], mostRecent, predictionModelNames[i], allCaseData, auc=FALSE)
		plotRegionalRisks(districts, countries, country_borders, riskData[[i]]$predictedRegions, riskData[[i]]$reportedCases, regionalRiskTitles[i], paste(sep='_', "regional_prediction", riskData[[i]]$name), FALSE)
	}
	
	return(riskData)
}

calculateAUCMatrix <- function(movementMatrices, predictionModelNames, allCaseData) {
	# work out how many risk models we have to deal with
	riskModelCount <- dim(movementMatrices)[3]
	
	cl <- makeCluster(8)
	registerDoParallel(cl)

	aucmatrix <- foreach(idx=1:mostRecent,.combine="rbind", .export=c("getData")) %dopar% {
		source("AUC.R")
		# we sometimes(?) need to manually track the indices if using a parallel library
		aucresult <- numeric()
		aucresult[1] <- idx
		for (i in 1:riskModelCount) {
			riskdata1 <- getData(movementMatrices[,,i], idx, predictionModelNames[i], allCaseData)
			aucresult[i+1] <- riskdata1$AUC
		}
		
		aucresult
	}
	stopCluster(cl)

	aucmatrix <- matrix(as.numeric(unlist(aucmatrix)),nrow=nrow(aucmatrix))
	return(aucmatrix)
}

calculateWeightedRisks <- function(riskData, aucs, predictionModelNames, outfile=TRUE) {
	# pull out the last 3
	latestaucs <- tail(aucs,3)
	# get the average ignore NaNs, nulls etc
	avgauc <- colMeans(latestaucs[,c(-1:-2)], na.rm = TRUE)
	names(avgauc) <- predictionModelNames[1:12]
	
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
		write.csv((avgauc[1:12] / max(avgauc[1:12])) /sum(avgauc[1:12]), "weightings.csv")
	}
	return(weighted_riskdata)
}

createRegionalCaseHistoryMaps <- function(allCaseData, districts, countries, country_borders) {
	cl <- makeCluster(8, outfile="out.log")
	registerDoParallel(cl)

	foreach(idx=1:mostRecent,.packages=c("aqfig"),.export=c("getSimpleData")) %dopar% {
		# plot
		rawdate <- paste(gsub("-W", " ", as.character(allCaseData[idx,1])), "1", sep=" ")
		formattedDate <- format(as.POSIXct(rawdate, format="%Y %U %u"), format="%B %d %Y")
		source('plotFunctions.R')
		plotDate(formattedDate, filename=paste(formatC(idx, width=2, flag="0"), "date", sep="_"))
		plotHistoricCases(districts, countries, country_borders, 0, getSimpleData(idx, allCaseData), plotTitle="", filename=paste(formatC(idx, width=2, flag="0"), "regional_cases_week", sep="_"), max(log(allCaseData[,-1])))
	}
	stopCluster(cl)
}