# functions (and examples) to plot maps of human movement and EVD spread

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
		write.csv((avgauc[1:12] / max(avgauc[1:12])) /sum(avgauc[1:12]), "regional_results/weightings.csv")
	}
	return(weighted_riskdata)
}
