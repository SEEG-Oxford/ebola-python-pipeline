# functions (and examples) to plot maps of human movement and EVD spread

source("global_results/calculateRisks.R")

countrycodes <- as.vector(all$country.1)

risks <- data.frame(country=countrycodes, risk=all$importation_risk)
plotGlobalRisks(risks, informCountries, allCountries, "global_Overall_prediction", "Global relative risk of Ebola importation\n from overall model")

risks <- data.frame(country=countrycodes, risk=all$adjacency_relative)
plotGlobalRisks(risks, informCountries, allCountries, "global_Adjacency_prediction", "Global relative risk of Ebola importation\n from Adjacency model")

risks <- data.frame(country=countrycodes, risk=all$gravity_relative)
plotGlobalRisks(risks, informCountries, allCountries, "global_Gravity_prediction", "Global relative risk of Ebola importation\n from Gravity model")

risks <- data.frame(country=countrycodes, risk=all$migration_relative)
plotGlobalRisks(risks, informCountries, allCountries, "global_Migration_prediction", "Global relative risk of Ebola importation\n from Migration model")

