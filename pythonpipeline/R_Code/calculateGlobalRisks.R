calculateGlobalRisks <- function(worldData, allcasedata) {

	twentyonedaycasedata <- tail(allcasedata, n=3)

	# guinea
	g <- sum(twentyonedaycasedata[,grep("^GIN", names(twentyonedaycasedata))])
	# liberia
	l <- sum(twentyonedaycasedata[,grep("^LBR", names(twentyonedaycasedata))])
	# sierra leone
	s <- sum(twentyonedaycasedata[,grep("^SLE", names(twentyonedaycasedata))])

	g_pop <- 12347766
	l_pop <- 4503439
	s_pop <- 6318575

	# calculate each index separately
	# migration
	worldData$migration_sum <- worldData$sum_guinea*(g/g_pop) + worldData$sum_liberia*(l/l_pop) + worldData$sum_sierra_leone*(s/s_pop)

	# define max
	colMax <- max(worldData$migration_sum) 

	worldData$migration_relative <- ((worldData$migration_sum)/colMax)*10

	# gravity 
	worldData$gravity_sum <- worldData$movement_from_guinea*(g/g_pop) + worldData$movement_from_liberia*(l/l_pop) + worldData$movement_from_sierra_leone*(s/s_pop)
	# define max
	colMax <- max(worldData$gravity_sum) 

	worldData$gravity_relative <- ((worldData$gravity_sum)/colMax)*10

	# adjacency calculations 
	worldData$adjacency_sum <- (4-worldData$from_guinea)*(g/g_pop) + (4-worldData$from_liberia)*(l/l_pop) + (4-worldData$from_sierra_leone)*(s/s_pop)

	# define max
	colMax <- max(worldData$adjacency_sum) 

	worldData$adjacency_relative <- ((worldData$adjacency_sum)/colMax)*10

	# put worldData together
	# define importance for each
	migration <- 1
	gravity <- .5
	adjacency <- 1
	worldData$importation_risk <- (worldData$migration_relative*migration + worldData$gravity_relative*gravity + worldData$adjacency_relative*adjacency) / 3
	
	return(worldData)
}