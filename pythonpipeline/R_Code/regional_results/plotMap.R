# functions (and examples) to plot maps of human movement and EVD spread

# 3 is france/gravity
riskdata1 <- getData(as.movementmatrix(all_cdr_europe[,c(1,2,3)]), mostRecent, "France Gravity", auc=FALSE)
plotRegionalRisks(districts, countries, country_borders, riskdata1$predictedRegions, riskdata1$reportedCases, "Regional relative risk of Ebola importation\n using gravity model from France", "regional_prediction_gravity_france", FALSE)

# 6 is france/radiation
riskdata2 <- getData(as.movementmatrix(all_cdr_europe[,c(1,2,6)]), mostRecent, "France Original Radiation", auc=FALSE)
plotRegionalRisks(districts, countries, country_borders, riskdata2$predictedRegions, riskdata2$reportedCases, "Regional relative risk of Ebola importation\n using original radiation model from France", "regional_prediction_radiation_france", FALSE)

# 9 is france/radiation-with-selection
riskdata3 <- getData(as.movementmatrix(all_cdr_europe[,c(1,2,9)]), mostRecent, "France Radiation with Selection", auc=FALSE)
plotRegionalRisks(districts, countries, country_borders, riskdata3$predictedRegions, riskdata3$reportedCases, "Regional relative risk of Ebola importation\n using radiation with selection model from France", "regional_prediction_radsel_france", FALSE)

# 12 is france/uniform
riskdata4 <- getData(as.movementmatrix(all_cdr_europe[,c(1,2,12)]), mostRecent, "France Uniform", auc=FALSE)
plotRegionalRisks(districts, countries, country_borders, riskdata4$predictedRegions, riskdata4$reportedCases, "Regional relative risk of Ebola importation\n using uniform selection model from France", "regional_prediction_uniform_france", FALSE)

# 4 is portugal/gravity
riskdata5 <- getData(as.movementmatrix(all_cdr_europe[,c(1,2,4)]), mostRecent, "Portugal Gravity", auc=FALSE)
plotRegionalRisks(districts, countries, country_borders, riskdata5$predictedRegions, riskdata5$reportedCases, "Regional relative risk of Ebola importation\n using gravity model from Portugal", "regional_prediction_gravity_portugal", FALSE)

# 7 is portugal/radiation
riskdata6 <- getData(as.movementmatrix(all_cdr_europe[,c(1,2,7)]), mostRecent, "Portugal Original Radiation", auc=FALSE)
plotRegionalRisks(districts, countries, country_borders, riskdata6$predictedRegions, riskdata6$reportedCases, "Regional relative risk of Ebola importation\n using original radiation model from Portugal", "regional_prediction_radiation_portugal", FALSE)

# 10 is portugal/radiation-with-selection
riskdata7 <- getData(as.movementmatrix(all_cdr_europe[,c(1,2,10)]), mostRecent, "Portugal Radiation with Selection", auc=FALSE)
plotRegionalRisks(districts, countries, country_borders, riskdata7$predictedRegions, riskdata7$reportedCases, "Regional relative risk of Ebola importation\n using radiation with selection model from Portugal", "regional_prediction_radsel_portugal", FALSE)

# 13 is portugal/uniform
riskdata8 <- getData(as.movementmatrix(all_cdr_europe[,c(1,2,13)]), mostRecent, "Portugal Uniform", auc=FALSE)
plotRegionalRisks(districts, countries, country_borders, riskdata8$predictedRegions, riskdata8$reportedCases, "Regional relative risk of Ebola importation\n using uniform selection model from Portugal", "regional_prediction_uniform_portugal", FALSE)

# 5 is spain/gravity
riskdata9 <- getData(as.movementmatrix(all_cdr_europe[,c(1,2,5)]), mostRecent, "Spain Gravity", auc=FALSE)
plotRegionalRisks(districts, countries, country_borders, riskdata9$predictedRegions, riskdata9$reportedCases, "Regional relative risk of Ebola importation\n using gravity model from Spain", "regional_prediction_gravity_spain", FALSE)

# 8 is spain/radiation
riskdata10 <- getData(as.movementmatrix(all_cdr_europe[,c(1,2,8)]), mostRecent, "Spain Original Radiation", auc=FALSE)
plotRegionalRisks(districts, countries, country_borders, riskdata10$predictedRegions, riskdata10$reportedCases, "Regional relative risk of Ebola importation\n using original radiation model from Spain", "regional_prediction_radiation_spain", FALSE)

# 11 is spain/radiation-with-selection
riskdata11 <- getData(as.movementmatrix(all_cdr_europe[,c(1,2,11)]), mostRecent, "Spain Radiation with Selection", auc=FALSE)
plotRegionalRisks(districts, countries, country_borders, riskdata11$predictedRegions, riskdata11$reportedCases, "Regional relative risk of Ebola importation\n using radiation with selection model from Spain", "regional_prediction_radsel_spain", FALSE)

# 14 is spain/uniform
riskdata12 <- getData(as.movementmatrix(all_cdr_europe[,c(1,2,14)]), mostRecent, "Spain Uniform", auc=FALSE)
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
	riskdata <- getData(as.movementmatrix(all_cdr_europe[,c(1,2,3)]), idx, predictionModelNames[1])
	aucresult[2] <- riskdata$AUC
	# plotRegionalRisks(vals, districts, countries, country_borders, riskdata$predictedRegions, riskdata$reportedCases, "Regional relative risk of Ebola importation\n using gravity model from France", paste("historical/plots/regional_prediction_gravity_france_week", idx, sep="_"))

	# 6 is france/radiation
	riskdata <- getData(as.movementmatrix(all_cdr_europe[,c(1,2,6)]), idx, predictionModelNames[2])
	# plotRegionalRisks(vals, districts, countries, country_borders, riskdata$predictedRegions, riskdata$reportedCases, "Regional relative risk of Ebola importation\n using original radiation model", paste("historical/plots/regional_prediction_radiation_france_week", idx, sep="_"))
	aucresult[3] <- riskdata$AUC

	# 9 is france/radiation-with-selection
	riskdata <- getData(as.movementmatrix(all_cdr_europe[,c(1,2,9)]), idx, predictionModelNames[3])
	# plotRegionalRisks(vals, districts, countries, country_borders, riskdata$predictedRegions, riskdata$reportedCases, "Regional relative risk of Ebola importation\n using radiation with selection model", paste("historical/plots/regional_prediction_radsel_france_week", idx, sep="_"))
	aucresult[4] <- riskdata$AUC

	# 12 is france/uniform
	riskdata <- getData(as.movementmatrix(all_cdr_europe[,c(1,2,12)]), idx, predictionModelNames[4])
	# plotRegionalRisks(vals, districts, countries, country_borders, riskdata$predictedRegions, riskdata$reportedCases, "Regional relative risk of Ebola importation\n using uniform selection model", paste("historical/plots/regional_prediction_uniform_france_week", idx, sep="_"))
	aucresult[5] <- riskdata$AUC
	
	
	# 4 is portual/gravity
	riskdata <- getData(as.movementmatrix(all_cdr_europe[,c(1,2,4)]), idx, predictionModelNames[5])
	# plotRegionalRisks(vals, districts, countries, country_borders, riskdata$predictedRegions, riskdata$reportedCases, "Regional relative risk of Ebola importation\n using gravity model", paste("historical/plots/regional_prediction_gravity_portugal_week", idx, sep="_"))
	aucresult[6] <- riskdata$AUC

	# 7 is portual/radiation
	riskdata <- getData(as.movementmatrix(all_cdr_europe[,c(1,2,7)]), idx, predictionModelNames[6])
	# plotRegionalRisks(vals, districts, countries, country_borders, riskdata$predictedRegions, riskdata$reportedCases, "Regional relative risk of Ebola importation\n using original radiation model", paste("historical/plots/regional_prediction_radiation_portugal_week", idx, sep="_"))
	aucresult[7] <- riskdata$AUC

	# 10 is portual/radiation-with-selection
	riskdata <- getData(as.movementmatrix(all_cdr_europe[,c(1,2,10)]), idx, predictionModelNames[7])
	# plotRegionalRisks(vals, districts, countries, country_borders, riskdata$predictedRegions, riskdata$reportedCases, "Regional relative risk of Ebola importation\n using radiation with selection model", paste("historical/plots/regional_prediction_radsel_portugal_week", idx, sep="_"))
	aucresult[8] <- riskdata$AUC

	# 13 is portual/uniform
	riskdata <- getData(as.movementmatrix(all_cdr_europe[,c(1,2,13)]), idx, predictionModelNames[8])
	# plotRegionalRisks(vals, districts, countries, country_borders, riskdata$predictedRegions, riskdata$reportedCases, "Regional relative risk of Ebola importation\n using uniform selection model", paste("historical/plots/regional_prediction_uniform_portugal_week", idx, sep="_"))
	aucresult[9] <- riskdata$AUC
	
	
	# 5 is spain/gravity
	riskdata <- getData(as.movementmatrix(all_cdr_europe[,c(1,2,5)]), idx, predictionModelNames[9])
	# plotRegionalRisks(vals, districts, countries, country_borders, riskdata$predictedRegions, riskdata$reportedCases, "Regional relative risk of Ebola importation\n using gravity model", paste("historical/plots/regional_prediction_gravity_spain_week", idx, sep="_"))
	aucresult[10] <- riskdata$AUC

	# 8 is portual/radiation
	riskdata <- getData(as.movementmatrix(all_cdr_europe[,c(1,2,8)]), idx, predictionModelNames[10])
	# plotRegionalRisks(vals, districts, countries, country_borders, riskdata$predictedRegions, riskdata$reportedCases, "Regional relative risk of Ebola importation\n using original radiation model", paste("historical/plots/regional_prediction_radiation_spain_week", idx, sep="_"))
	aucresult[11] <- riskdata$AUC

	# 11 is portual/radiation-with-selection
	riskdata <- getData(as.movementmatrix(all_cdr_europe[,c(1,2,11)]), idx, predictionModelNames[11])
	# plotRegionalRisks(vals, districts, countries, country_borders, riskdata$predictedRegions, riskdata$reportedCases, "Regional relative risk of Ebola importation\n using radiation with selection model", paste("historical/plots/regional_prediction_radsel_spain_week", idx, sep="_"))
	aucresult[12] <- riskdata$AUC

	# 14 is portual/uniform
	riskdata <- getData(as.movementmatrix(all_cdr_europe[,c(1,2,14)]), idx, predictionModelNames[12])
	# plotRegionalRisks(vals, districts, countries, country_borders, riskdata$predictedRegions, riskdata$reportedCases, "Regional relative risk of Ebola importation\n using uniform selection model", paste("historical/plots/regional_prediction_uniform_spain_week", idx, sep="_"))
	aucresult[13] <- riskdata$AUC
	
	# overall africa gravity model
	riskdata <- getData(as.movementmatrix(west_africa_gravity[,c(8,14,19)]), idx, "West Africa Gravity")
	# plotRegionalRisks(vals, districts, countries, country_borders, riskdata$predictedRegions, riskdata$reportedCases, "Regional relative risk of Ebola importation\n using West Africa gravity model", paste("historical/plots/regional_prediction_gravity_west_africa_week", idx, sep="_"))
	aucresult[14] <- riskdata$AUC
	
	aucresult
}
stopCluster(cl)

colnames(aucmatrix) <- c("Week index", predictionModelNames, "West Africa Gravity")

write.csv(aucmatrix, "regional_results/aucdata.csv")