# functions (and examples) to plot maps of human movement and EVD spread

cl <- makeCluster(8, outfile="out.log")
registerDoParallel(cl)

aucmatrix <- foreach(idx=1:mostRecent,.combine=rbind, .packages=c("aqfig")) %dopar% {
	# plot
	rawdate <- paste(gsub("-W", " ", as.character(evdcasedata[idx,1])), "1", sep=" ")
	formattedDate <- format(as.POSIXct(rawdate, format="%Y %U %u"), format="%B %d %Y")
	source('plotFunctions.R')
	plotDate(formattedDate, filename=paste(formatC(idx, width=2, flag="0"), "date", sep="_"))
	plotHistoricCases(districts, countries, country_borders, 0, getSimpleData(idx), plotTitle="", filename=paste(formatC(idx, width=2, flag="0"), "regional_cases_week", sep="_"))
}
stopCluster(cl)