source("AUC.R")
# n= number of weeks data to read from the end of the file
allcasedata <- read.csv('../data/EVD_conf_prob_.csv')

# helper function
as.movementmatrix <- function(dataframe) {
	nrows <- length(unique(dataframe[1])[,])
	ncols <- length(unique(dataframe[2])[,])
	if(nrows != ncols) {
		stop ("Error: Expected a square matrix!")
	}
	
	mat <- matrix(ncol = ncols, nrow = nrows, dimnames = list(sort(unique(dataframe[1])[,]),sort(unique(dataframe[2])[,])))
	for(idx in 1:nrow(dataframe)) {
		mat[as.character(dataframe[idx,2]),as.character(dataframe[idx,1])] <- dataframe[idx,3]
	}
	
	mat[is.na(mat)] <- 0
	
	return (mat)
}

# pull out origin, destination and radiation with selection_france
getData <- function(column, startWeek, name, auc=TRUE) {
	endWeek <- startWeek+2
	casedata <- colSums(allcasedata[c(startWeek:endWeek),][,-1])
	all_crd <- read.csv('../data/all_cdr_europe.csv')[,c(1,2,column)]

	raw_movement_matrix <- as.movementmatrix(all_crd)

	for(idx in 1:nrow(raw_movement_matrix)) {
		nametofind <- gsub("\\s", ".", row.names(raw_movement_matrix)[idx])
		casecount <- casedata[match(nametofind, names(casedata))]
		raw_movement_matrix[idx,] <- (raw_movement_matrix[idx,] * casecount)
	}

	raw_movement_matrix[is.na(raw_movement_matrix)] <- 0
	summedRegions <- colSums(raw_movement_matrix)
	summedRegions[is.na(summedRegions)] <- 0
	summedRegions <- summedRegions / max(summedRegions)

	## prepare the region names
	districtNames <- names(summedRegions)

	districtNames <- gsub("\\s", "_", districtNames)
	districtNames <- gsub("'", "_", districtNames)

	# Correct names of certain regions which are different in the shapefile
	districtNames <- gsub("RIVER_GEE", "RIVER_GHEE", districtNames)
	districtNames <- gsub("CIV_GBEKE", "CIV_GBÊKE", districtNames)
	districtNames <- gsub("CIV_GBOKLE", "CIV_GBÔKLE", districtNames)
	districtNames <- gsub("CIV_GOH", "CIV_GÔH", districtNames)
	districtNames <- gsub("CIV_LOH_DJIBOUA", "CIV_LÔH_DJIBOUA", districtNames)


	names(summedRegions) <- districtNames

	# these are the "core" districts
	reportedCases <- casedata[casedata > 0]
	reportedNames <- names(reportedCases)
	reportedNames <- gsub("\\s", "_", reportedNames)
	reportedNames <- gsub("'", "_", reportedNames)
	reportedNames <- gsub("[.]", "_", reportedNames)

	# Correct names of certain regions which are different in the core dataset
	reportedNames <- gsub("RIVER_GEE", "RIVER_GHEE", reportedNames)
	names(reportedCases) <- reportedNames

	predictedRegions <- summedRegions[!(names(summedRegions) %in% names(reportedCases))]
	predictedRegions = predictedRegions / max(predictedRegions)
	# these are the regions we have case data for, so we want to see how accurate the predictions are
	write.csv(t(summedRegions[grep("GIN|LBR|SLE", names(summedRegions))]), paste("historical/data/risk_week", startWeek, name, ".csv", sep="_"))
	
	if(auc) {
		# calculate AUC
		# select the week being predicted
		weekbeingpredicted <- allcasedata[endWeek+1,][,-1]
		# make it logical (either there are cases or not)
		weekbeingpredicted[weekbeingpredicted > 0] <- 1
		
		# remove regions where the are already cases to prevent skewing the data
		weekbeingpredicted <- weekbeingpredicted[!(names(weekbeingpredicted) %in% names(reportedCases))]
		
		predictions <- summedRegions[grep("GIN|LBR|SLE", names(summedRegions))]
		predictions <- predictions[!(names(predictions) %in% names(reportedCases))]
		
		AUC <- AUC(data.frame(weekbeingpredicted), data.frame(predictions),plot=FALSE,error_bars=FALSE,ci=0.95,res=100,main=name)
	}
	
	return (list(reportedCases=reportedCases, predictedRegions=predictedRegions, AUC=AUC))
}