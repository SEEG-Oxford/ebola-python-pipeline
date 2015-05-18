# functions (and examples) to plot maps of human movement and EVD spread

# 3 is france/gravity
francegravityriskdata <- getData(as.movementmatrix(all_cdr_europe[,c(1,2,3)]), mostRecent, "France Gravity", auc=FALSE)

# 6 is france/radiation
franceradiationriskdata <- getData(as.movementmatrix(all_cdr_europe[,c(1,2,6)]), mostRecent, "France Original Radiation", auc=FALSE)

# 9 is france/radiation-with-selection
franceradselriskdata <- getData(as.movementmatrix(all_cdr_europe[,c(1,2,9)]), mostRecent, "France Radiation with Selection", auc=FALSE)

# 12 is france/uniform
franceuniformriskdata <- getData(as.movementmatrix(all_cdr_europe[,c(1,2,12)]), mostRecent, "France Uniform", auc=FALSE)

# 4 is portugal/gravity
portugalgravityriskdata <- getData(as.movementmatrix(all_cdr_europe[,c(1,2,4)]), mostRecent, "Portugal Gravity", auc=FALSE)

# 7 is portugal/radiation
portugalradiationriskdata <- getData(as.movementmatrix(all_cdr_europe[,c(1,2,7)]), mostRecent, "Portugal Original Radiation", auc=FALSE)

# 10 is portugal/radiation-with-selection
portugalradselriskdata <- getData(as.movementmatrix(all_cdr_europe[,c(1,2,10)]), mostRecent, "Portugal Radiation with Selection", auc=FALSE)

# 13 is portugal/uniform
portugaluniformriskdata <- getData(as.movementmatrix(all_cdr_europe[,c(1,2,13)]), mostRecent, "Portugal Uniform", auc=FALSE)

# 5 is spain/gravity
spaingravityriskdata <- getData(as.movementmatrix(all_cdr_europe[,c(1,2,5)]), mostRecent, "Spain Gravity", auc=FALSE)

# 8 is spain/radiation
spainradiationriskdata <- getData(as.movementmatrix(all_cdr_europe[,c(1,2,8)]), mostRecent, "Spain Original Radiation", auc=FALSE)

# 11 is spain/radiation-with-selection
spainradselriskdata <- getData(as.movementmatrix(all_cdr_europe[,c(1,2,11)]), mostRecent, "Spain Radiation with Selection", auc=FALSE)

# 14 is spain/uniform
spainuniformriskdata <- getData(as.movementmatrix(all_cdr_europe[,c(1,2,14)]), mostRecent, "Spain Uniform", auc=FALSE)

# read all aucs
aucs <- read.csv('regional_results/aucdata.csv')
# pull out the last 3
latestaucs <- tail(aucs,3)
# get the average ignore NaNs, nulls etc
avgauc <- colMeans(latestaucs[,c(-1:-2)], na.rm = TRUE)
names(avgauc) <- predictionModelNames

# multiply each models predicted risk by its avg auc value
franceweighted_gravity <- francegravityriskdata$predictedRegions * avgauc[1]
franceweighted_radiation <- franceradiationriskdata$predictedRegions * avgauc[2]
franceweighted_radsel <- franceradselriskdata$predictedRegions * avgauc[3]
franceweighted_uniform <- franceuniformriskdata$predictedRegions * avgauc[4]
portugalweighted_gravity <- portugalgravityriskdata$predictedRegions * avgauc[5]
portugalweighted_radiation <- portugalradiationriskdata$predictedRegions * avgauc[6]
portugalweighted_radsel <- portugalradselriskdata$predictedRegions * avgauc[7]
portugalweighted_uniform <- portugaluniformriskdata$predictedRegions * avgauc[8]
spainweighted_gravity <- spaingravityriskdata$predictedRegions * avgauc[9]
spainweighted_radiation <- spainradiationriskdata$predictedRegions * avgauc[10]
spainweighted_radsel <- spainradselriskdata$predictedRegions * avgauc[11]
spainweighted_uniform <- spainuniformriskdata$predictedRegions * avgauc[12]

# sum the weighted risks
weighted_riskdata <- franceweighted_gravity + franceweighted_radiation + franceweighted_radsel + franceweighted_uniform + portugalweighted_gravity + portugalweighted_radiation + portugalweighted_radsel + portugalweighted_uniform + spainweighted_gravity + spainweighted_radiation + spainweighted_radsel + spainweighted_uniform
# normalise 0..1
# if the weightings are all zero then don't do anything (means AUC calculation was really bad)
if(max(weighted_riskdata) > 0) {
	weighted_riskdata <- weighted_riskdata / max(weighted_riskdata)
}

# plot
plotRegionalRisks(districts, countries, country_borders, weighted_riskdata, francegravityriskdata$reportedCases, "Regional relative risk of Ebola importation\n using weighted prediction model data", "regional_prediction_weighted")
write.csv((avgauc[1:12] / max(avgauc[1:12])) /sum(avgauc[1:12]), "regional_results/weightings.csv")
