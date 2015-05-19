# functions (and examples) to plot maps of human movement and EVD spread

# 3 is france/gravity
riskdata1 <- getData(movementMatrices[,,1], mostRecent, "France Gravity", auc=FALSE)
plotRegionalRisks(districts, countries, country_borders, riskdata1$predictedRegions, riskdata1$reportedCases, "Regional relative risk of Ebola importation\n using gravity model from France", "regional_prediction_gravity_france", FALSE)

# 6 is france/radiation
riskdata2 <- getData(movementMatrices[,,4], mostRecent, "France Original Radiation", auc=FALSE)
plotRegionalRisks(districts, countries, country_borders, riskdata2$predictedRegions, riskdata2$reportedCases, "Regional relative risk of Ebola importation\n using original radiation model from France", "regional_prediction_radiation_france", FALSE)

# 9 is france/radiation-with-selection
riskdata3 <- getData(movementMatrices[,,7], mostRecent, "France Radiation with Selection", auc=FALSE)
plotRegionalRisks(districts, countries, country_borders, riskdata3$predictedRegions, riskdata3$reportedCases, "Regional relative risk of Ebola importation\n using radiation with selection model from France", "regional_prediction_radsel_france", FALSE)

# 12 is france/uniform
riskdata4 <- getData(movementMatrices[,,10], mostRecent, "France Uniform", auc=FALSE)
plotRegionalRisks(districts, countries, country_borders, riskdata4$predictedRegions, riskdata4$reportedCases, "Regional relative risk of Ebola importation\n using uniform selection model from France", "regional_prediction_uniform_france", FALSE)

# 4 is portugal/gravity
riskdata5 <- getData(movementMatrices[,,2], mostRecent, "Portugal Gravity", auc=FALSE)
plotRegionalRisks(districts, countries, country_borders, riskdata5$predictedRegions, riskdata5$reportedCases, "Regional relative risk of Ebola importation\n using gravity model from Portugal", "regional_prediction_gravity_portugal", FALSE)

# 7 is portugal/radiation
riskdata6 <- getData(movementMatrices[,,5], mostRecent, "Portugal Original Radiation", auc=FALSE)
plotRegionalRisks(districts, countries, country_borders, riskdata6$predictedRegions, riskdata6$reportedCases, "Regional relative risk of Ebola importation\n using original radiation model from Portugal", "regional_prediction_radiation_portugal", FALSE)

# 10 is portugal/radiation-with-selection
riskdata7 <- getData(movementMatrices[,,8], mostRecent, "Portugal Radiation with Selection", auc=FALSE)
plotRegionalRisks(districts, countries, country_borders, riskdata7$predictedRegions, riskdata7$reportedCases, "Regional relative risk of Ebola importation\n using radiation with selection model from Portugal", "regional_prediction_radsel_portugal", FALSE)

# 13 is portugal/uniform
riskdata8 <- getData(movementMatrices[,,11], mostRecent, "Portugal Uniform", auc=FALSE)
plotRegionalRisks(districts, countries, country_borders, riskdata8$predictedRegions, riskdata8$reportedCases, "Regional relative risk of Ebola importation\n using uniform selection model from Portugal", "regional_prediction_uniform_portugal", FALSE)

# 5 is spain/gravity
riskdata9 <- getData(movementMatrices[,,3], mostRecent, "Spain Gravity", auc=FALSE)
plotRegionalRisks(districts, countries, country_borders, riskdata9$predictedRegions, riskdata9$reportedCases, "Regional relative risk of Ebola importation\n using gravity model from Spain", "regional_prediction_gravity_spain", FALSE)

# 8 is spain/radiation
riskdata10 <- getData(movementMatrices[,,6], mostRecent, "Spain Original Radiation", auc=FALSE)
plotRegionalRisks(districts, countries, country_borders, riskdata10$predictedRegions, riskdata10$reportedCases, "Regional relative risk of Ebola importation\n using original radiation model from Spain", "regional_prediction_radiation_spain", FALSE)

# 11 is spain/radiation-with-selection
riskdata11 <- getData(movementMatrices[,,9], mostRecent, "Spain Radiation with Selection", auc=FALSE)
plotRegionalRisks(districts, countries, country_borders, riskdata11$predictedRegions, riskdata11$reportedCases, "Regional relative risk of Ebola importation\n using radiation with selection model from Spain", "regional_prediction_radsel_spain", FALSE)

# 14 is spain/uniform
riskdata12 <- getData(movementMatrices[,,12], mostRecent, "Spain Uniform", auc=FALSE)
plotRegionalRisks(districts, countries, country_borders, riskdata12$predictedRegions, riskdata12$reportedCases, "Regional relative risk of Ebola importation\n using uniform selection model from Spain", "regional_prediction_uniform_spain", FALSE)

# overall africa gravity model
riskdata <- getData(as.movementmatrix(west_africa_gravity[,c(8,14,19)]), mostRecent, "West Africa Gravity", auc=FALSE)
plotRegionalRisks(districts, countries, country_borders, riskdata$predictedRegions, riskdata$reportedCases, "Regional relative risk of Ebola importation\n using West Africa gravity model", "regional_prediction_gravity_west_africa", FALSE)

plotCompositeLeaflet(districts, list(riskdata1,riskdata2,riskdata3,riskdata4,riskdata5,riskdata6,riskdata7,riskdata8,riskdata9,riskdata10,riskdata11,riskdata12))

cl <- makeCluster(8)
registerDoParallel(cl)

aucmatrix <- foreach(idx=1:mostRecent,.combine=rbind) %dopar% {
	# we sometimes(?) need to manually track the indices if using a parallel library
	aucresult <- vector()
	aucresult[1] <- idx
	# 3 is france/gravity
	riskdata <- getData(movementMatrices[,,1], idx, predictionModelNames[1])
	aucresult[2] <- riskdata$AUC

	# 6 is france/radiation
	riskdata <- getData(movementMatrices[,,4], idx, predictionModelNames[2])
	aucresult[3] <- riskdata$AUC

	# 9 is france/radiation-with-selection
	riskdata <- getData(movementMatrices[,,7], idx, predictionModelNames[3])
	aucresult[4] <- riskdata$AUC

	# 12 is france/uniform
	riskdata <- getData(movementMatrices[,,10], idx, predictionModelNames[4])
	aucresult[5] <- riskdata$AUC	
	
	# 4 is portual/gravity
	riskdata <- getData(movementMatrices[,,2], idx, predictionModelNames[5])
	aucresult[6] <- riskdata$AUC

	# 7 is portual/radiation
	riskdata <- getData(movementMatrices[,,5], idx, predictionModelNames[6])
	aucresult[7] <- riskdata$AUC

	# 10 is portual/radiation-with-selection
	riskdata <- getData(movementMatrices[,,8], idx, predictionModelNames[7])
	aucresult[8] <- riskdata$AUC

	# 13 is portual/uniform
	riskdata <- getData(movementMatrices[,,11], idx, predictionModelNames[8])
	aucresult[9] <- riskdata$AUC	
	
	# 5 is spain/gravity
	riskdata <- getData(movementMatrices[,,3], idx, predictionModelNames[9])
	aucresult[10] <- riskdata$AUC

	# 8 is portual/radiation
	riskdata <- getData(movementMatrices[,,6], idx, predictionModelNames[10])
	aucresult[11] <- riskdata$AUC

	# 11 is portual/radiation-with-selection
	riskdata <- getData(movementMatrices[,,9], idx, predictionModelNames[11])
	aucresult[12] <- riskdata$AUC

	# 14 is portual/uniform
	riskdata <- getData(movementMatrices[,,12], idx, predictionModelNames[12])
	aucresult[13] <- riskdata$AUC
	
	# overall africa gravity model
	riskdata <- getData(as.movementmatrix(west_africa_gravity[,c(8,14,19)]), idx, "West Africa Gravity")
	aucresult[14] <- riskdata$AUC
	
	aucresult
}
stopCluster(cl)

colnames(aucmatrix) <- c("Week index", predictionModelNames, "West Africa Gravity")

write.csv(aucmatrix, "regional_results/aucdata.csv")