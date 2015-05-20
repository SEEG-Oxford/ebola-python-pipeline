# functions (and examples) to plot maps of human movement and EVD spread

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

	aucmatrix <- foreach(idx=1:mostRecent,.combine=rbind, .export=c("getData")) %dopar% {
		source("AUC.R")
		# we sometimes(?) need to manually track the indices if using a parallel library
		aucresult <- vector()
		aucresult[1] <- idx
		for (i in 1:riskModelCount) {
			riskdata1 <- getData(movementMatrices[,,i], idx, predictionModelNames[i], allCaseData)
			aucresult[i+1] <- riskdata1$AUC
		}
		
		aucresult
	}
	stopCluster(cl)

	return(aucmatrix)
}
