Cancer           <- read.csv('cancer.csv', header = T, as.is = T, row.names = 1)

Cancer$incidenceRate <- Cancer$avgAnnCount / Cancer$popEst2015 * 100000

Cancer$incidenceRate[Cancer$avgAnnCount==1962.667684] <- NA
