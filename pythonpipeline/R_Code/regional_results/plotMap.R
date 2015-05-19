# functions (and examples) to plot maps of human movement and EVD spread

# work out how many risk models we have to deal with
riskModelCount <- dim(movementMatrices)[3]

riskyData <- rep(NULL, riskModelCount)

for (i in 1:riskModelCount) {
	riskyData[[i]] <- getData(movementMatrices[,,i], mostRecent, predictionModelNames[i], auc=FALSE)
	plotRegionalRisks(districts, countries, country_borders, riskyData[[i]]$predictedRegions, riskyData[[i]]$reportedCases, regionalRiskTitles[i], paste(sep='_', "regional_prediction", riskyData[[i]]$name), FALSE)
}

# overall africa gravity model
riskdata <- getData(as.movementmatrix(west_africa_gravity[,c(8,14,19)]), mostRecent, "West Africa Gravity", auc=FALSE)
plotRegionalRisks(districts, countries, country_borders, riskdata$predictedRegions, riskdata$reportedCases, "Regional relative risk of Ebola importation\n using West Africa gravity model", "regional_prediction_gravity_west_africa", FALSE)

plotCompositeLeaflet(districts, riskyData)

cl <- makeCluster(8)
registerDoParallel(cl)

aucmatrix <- foreach(idx=1:mostRecent,.combine=rbind) %dopar% {
	# we sometimes(?) need to manually track the indices if using a parallel library
	aucresult <- vector()
	aucresult[1] <- idx
	for (i in 1:riskModelCount) {
		riskdata1 <- getData(movementMatrices[,,i], idx, predictionModelNames[i])
		aucresult[i+1] <- riskdata1$AUC
	}
	
	# overall africa gravity model
	riskdata <- getData(as.movementmatrix(west_africa_gravity[,c(8,14,19)]), idx, "West Africa Gravity")
	aucresult[14] <- riskdata$AUC
	
	aucresult
}
stopCluster(cl)

colnames(aucmatrix) <- c("Week index", predictionModelNames)

write.csv(aucmatrix, "regional_results/aucdata.csv")