# sort out sitrep district-level case data

# clear workspace
rm(list = ls())

# data sources
# 'http://apps.who.int/gho/data/view.ebola-sitrep.ebola-country-GIN-new-conf-prob-districs-20150121-data?lang=en'
# 'http://apps.who.int/gho/data/view.ebola-sitrep.ebola-country-LBR-new-conf-prob-districs-20150121-data?lang=en'
# 'http://apps.who.int/gho/data/view.ebola-sitrep.ebola-country-SLE-new-conf-prob-districs-20150121-data?lang=en'
# new data by epi week and district, download csv (text and codes)

# load guinea dataset
guinea_raw <- read.csv('gin.csv',
                       stringsAsFactors = FALSE)

liberia_raw <- read.csv('lbr.csv',
                        stringsAsFactors = FALSE)

sierra_leone_raw <- read.csv('sle.csv',
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

# combine to get uniwque district names
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
    confirmed[i, j] <- all$number[all$week == week &
                                           all$country_district == district &
                                           all$case_type == 'CONFIRMED']

    probable[i, j] <- all$number[all$week == week &
                                           all$country_district == district &
                                           all$case_type == 'PROBABLE']
  }
}


confirmed_probable <- confirmed + probable

par(mfrow = c(1, 3))
image(log1p(confirmed))
image(log1p(probable))
image(log1p(confirmed_probable))


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
# lines(x = 0:1,
#       y = rep((which(substr(districts, 1, 3) == 'SLE')[1] - 0.5) / (ndistrict), 2), 
#       lty = 1,
#        col = grey(0.4))
# lines(x = 0:1,
#       y = rep((which(substr(districts, 1, 3) == 'LBR')[1] - 0.5) / (ndistrict), 2),
#       lty = 1,
#       col = grey(0.4))


write.csv(confirmed_probable, file = 'EVD_conf_prob_.csv')
