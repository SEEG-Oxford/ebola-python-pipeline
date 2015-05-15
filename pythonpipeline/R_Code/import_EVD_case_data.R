# sort out sitrep district-level case data

# clear workspace
rm(list = ls())

createImages <- FALSE

# data sources
# 'http://apps.who.int/gho/data/view.ebola-sitrep.ebola-country-GIN-new-conf-prob-districs-20150121-data?lang=en'
# 'http://apps.who.int/gho/data/view.ebola-sitrep.ebola-country-LBR-new-conf-prob-districs-20150121-data?lang=en'
# 'http://apps.who.int/gho/data/view.ebola-sitrep.ebola-country-SLE-new-conf-prob-districs-20150121-data?lang=en'
# new data by epi week and district, download csv (text and codes)

# load guinea dataset
guinea_raw <- read.csv('GIN.csv',
                       stringsAsFactors = FALSE)

liberia_raw <- read.csv('LBR.csv',
                        stringsAsFactors = FALSE)

sierra_leone_raw <- read.csv('SLE.csv',
                        stringsAsFactors = FALSE)

all_raw <- rbind(guinea_raw, liberia_raw, sierra_leone_raw)
all_raw <- all_raw[all_raw$EBOLA_DATA_SOURCE..CODE. == 'PATIENTDB', ]

# subset to fields we want
all <- data.frame(country = all_raw$COUNTRY..CODE.,
                     district = all_raw$LOCATION..DISPLAY.,
                     case_type = all_raw$CASE_DEFINITION..CODE.,
                     indicator_type = all_raw$INDICATOR_TYPE..CODE.,
                     week = all_raw$EPI_WEEK..CODE.,
                     number = all_raw$Numeric)

# combine to get unique district names
all$country_district <- factor(paste(all$country,
                                     all$district,
                                     sep = '_'))

# set NAs to 0s (want number of *reported* cases in the model)
all$number[is.na(all$number)] <- 0

# get all weeks and districts
weeks <- sort(unique(all$week))
districts <- sort(unique(all$country_district))

nweek <- length(weeks)
ndistrict <- length(districts)

# create a matrix to store results in
confirmed <- matrix(NA,
                    nrow = nweek,
                    ncol = ndistrict)

rownames(confirmed) <- weeks
colnames(confirmed) <- districts

# copy for probables
probable <- confirmed

# loop through all combinations, adding the data
for (i in 1:nweek) {
  for (j in 1:ndistrict) {
    
    week <- weeks[i]
    district <- districts[j]

    # add in numbers for the relevant matrices
    confirmedCase <- all$number[all$week == week &
                                  all$country_district == district &
                                  all$case_type == 'CONFIRMED']
    # fix for broken WHO data were some districts are simply missing the CONFIRMED case count
    if(length(confirmedCase) == 0) {
      confirmedCase <- 0
    }
    cat(paste(week, district, confirmedCase,sep="-"),"\n")
    
    # fix for WHO data that have multiple copies of the same region for the CONFIRMED case count in a given week
    # we assume the last one in the list is correct
    confirmed[i, j] <- tail(confirmedCase,n=1)

    probableCase <- all$number[all$week == week &
                                 all$country_district == district &
                                 all$case_type == 'PROBABLE']
    
    # fix for broken WHO data were some districts are simply missing the PROBABLE case count
    if(length(probableCase) == 0) {
      probableCase <- 0
    }
    
    # fix for WHO data that have multiple copies of the same region for the CONFIRMED case count in a given week
    # we assume the last one in the list is correct
    probable[i, j] <- tail(probableCase,n=1)
  }
}


confirmed_probable <- confirmed + probable

if(createImages) {
	par(mfrow = c(1, 3))
	image(log1p(confirmed))
	image(log1p(probable))
	image(log1p(confirmed_probable))
}
# fix column names as the WHO decided to rename FREETOWN to WESTERN AREA URBAN, whereas all the maps etc still list it as FREETOWN
colnames(confirmed_probable) <- gsub("WESTERN AREA URBAN", "FREETOWN", colnames(confirmed_probable))
colnames(confirmed_probable) <- gsub("WESTERN AREA RURAL", "WESTERN RURAL", colnames(confirmed_probable))
colnames(confirmed_probable) <- gsub("GIN_KISSIDOUGOU", "GIN_KISSIDOUGO", colnames(confirmed_probable))
# put the order back again
confirmed_probable <- confirmed_probable[,order(colnames(confirmed_probable))]

if(createImages) {
	pal <- colorRampPalette(c(grey(0.9),
							  'darkorange',
							  'darkred'))
	par(mfrow = c(1, 1),
		mar = c(3, 8, 3, 3))
	image(log1p(confirmed_probable),
		  col = pal(100),
		  axes = FALSE)
	title(main = 'New cases per week',
		  cex.main = 1.5,
		  line = 1,
		  col.main = grey(0.3))
	axis(1,
		 at = seq(0, 1, len = 5),
		 labels = weeks[ceiling(seq(1 / nweek, 1, len = 5) * nweek)],
		 las = 1,
		 tick = FALSE,
		 col.axis = grey(0.3))
	axis(2, at = seq(0, 1, len = ndistrict),
		 labels = districts,
		 cex.axis = 0.6,
		 las = 1,
		 line = -0.5,
		 col.axis = grey(0.3),
		 tick = FALSE)
	box(col = grey(0.7))
	abline(v = (ndistrict - 2) / ndistrict + 0.5 / ndistrict,
		   col = grey(0.5))
}

write.csv(confirmed_probable, file = 'EVD_conf_prob_.csv')
