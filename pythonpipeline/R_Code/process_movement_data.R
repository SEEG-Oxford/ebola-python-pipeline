source("AUC.R")

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
getData <- function(raw_movement_matrix, endWeek, name, auc=TRUE) {
	startWeek <- endWeek-3
	if (startWeek < 1) startWeek <- 0
	casedata <- colSums(allcasedata[c(startWeek:(endWeek-1)),][,-1])
	
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
	# these are the regions we have case data for, so we want to see how accurate the predictions are
	#write.csv(t(summedRegions[grep("GIN|LBR|SLE", names(summedRegions))]), paste("regional_prediction_history/historical/data/core_risk_week", startWeek, name, ".csv", sep="_"))
	# these are the risks for the non-core regions
	#write.csv(t(predictedRegions), paste("regional_prediction_history/historical/data/non-core_risk_week", startWeek, name, ".csv", sep="_"))
	
	if(auc) {
		# calculate AUC
		# select the week being predicted
		weekbeingpredicted <- allcasedata[endWeek,][,-1]
		# make it logical (either there are cases or not)
		weekbeingpredicted[weekbeingpredicted > 0] <- 1
		
		# remove regions where the are already cases to prevent skewing the data
		weekbeingpredicted <- weekbeingpredicted[!(names(weekbeingpredicted) %in% names(reportedCases))]
		
		predictions <- summedRegions[grep("GIN|LBR|SLE", names(summedRegions))]
		predictions <- predictions[!(names(predictions) %in% names(reportedCases))]
		
		AUC <- AUC(data.frame(weekbeingpredicted), data.frame(predictions),plot=FALSE,error_bars=FALSE,ci=0.95,res=100,main=name)
	}
	
	return (list(reportedCases=reportedCases, predictedRegions=predictedRegions, AUC=AUC, name=name))
}

getSimpleData <- function(endWeek) {
	startWeek <- endWeek-2
	if(startWeek < 0) startWeek <- 0
	casedata <- colSums(allcasedata[c(startWeek:endWeek),][,-1])
	
	# these are the "core" districts
	reportedCases <- casedata[casedata > 0]
	
	return (reportedCases)
}