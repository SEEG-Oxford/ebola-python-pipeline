all <- read.csv('../../data/all.csv')

#define prevalence frome some other sheet 
allcasedata <- read.csv('../../data/EVD_conf_prob_.csv')
# this must be exactly the same format as allcasedata and will also need curating
# when cases move from the sitrep to the patientdb
additionalcasedata <- read.csv('../../data/EVD_conf_prob_additional.csv')

allcasedata <- allcasedata + additionalcasedata

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
all$migration_sum <- all$sum_guinea*(g/g_pop) + all$sum_liberia*(l/l_pop) + all$sum_sierra_leone*(s/s_pop)

# define max
colMax <- max(all$migration_sum) 

all$migration_relative <- ((all$migration_sum)/colMax)*10

# gravity 
all$gravity_sum <- all$movement_from_guinea*(g/g_pop) + all$movement_from_liberia*(l/l_pop) + all$movement_from_sierra_leone*(s/s_pop)
# define max
colMax <- max(all$gravity_sum) 

all$gravity_relative <- ((all$gravity_sum)/colMax)*10

# adjacency calculations 
all$adjacency_sum <- (4-all$from_guinea)*(g/g_pop) + (4-all$from_liberia)*(l/l_pop) + (4-all$from_sierra_leone)*(s/s_pop)

# define max
colMax <- max(all$adjacency_sum) 

all$adjacency_relative <- ((all$adjacency_sum)/colMax)*10

# put all together
# define importance for each
migration <- 1
gravity <- .5
adjacency <- 1
all$importation_risk <- (all$migration_relative*migration + all$gravity_relative*gravity + all$adjacency_relative*adjacency) / 3
