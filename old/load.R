Cancer <- read.csv('cancer.csv', header = T, as.is = T, row.names = 1)

Cancer$color <- '#098154'
Cancer$color[Cancer$MedianAge>100] <- '#c72e29'
Cancer$color[Cancer$AvgHouseholdSize<1] <- '#c72e29'
Cancer$color[Cancer$avgAnnCount==1962.667684] <- '#c72e29'

Cancer$MedianAge[Cancer$MedianAge>100] <- Cancer$MedianAge[Cancer$MedianAge>100] / 12 # correct median ages over 100 years which were provided in months
Cancer$AvgHouseholdSize[Cancer$AvgHouseholdSize<1] <- Cancer$AvgHouseholdSize[Cancer$AvgHouseholdSize<1] * 100 # correct average household sizes below 1 which were divided by 100

incidenceMean <- mean(Cancer$avgAnnCount[Cancer$avgAnnCount!=1962.667684] / Cancer$popEst2015[Cancer$avgAnnCount!=1962.667684] * 100000) # mean incidence across the u.s.
Cancer$avgAnnCount[Cancer$avgAnnCount==1962.667684] <- incidenceMean * Cancer$popEst2015[Cancer$avgAnnCount==1962.667684] / 100000 # replace anomalous values with imputation
Cancer$incidenceRate <- Cancer$avgAnnCount / Cancer$popEst2015 * 100000 # new cancer cases per 100,000 persons per year
