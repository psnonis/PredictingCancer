Cancer <- read.csv('cancer.csv', header = T, as.is = T, row.names = 1)

Cancer$incedenceCount <- Cancer$avgAnnCount # renaming for consistency
Cancer$incedenceCount[Cancer$incedenceCount==1962.667684] <- NA # remove anomalous values
Cancer$incedenceProportion <- Cancer$incedenceCount/Cancer$popEst2015 # incedence as proportion of population (risk)

incedenceProportionMean <- mean(Cancer$incedenceProportion, na.rm = T) # mean of incedence as proportion of population
Cancer$incedenceCount[is.na(Cancer$incedenceCount)] <- incedenceProportionMean * Cancer$popEst2015[is.na(Cancer$incedenceCount)] # replace anomalous values with imputation
Cancer$incedenceProportion <- Cancer$incedenceCount/Cancer$popEst2015 # incedence as proportion of population (risk)

Cancer$incedenceRate <- Cancer$incedenceCount / Cancer$popEst2015 * 100000 # incedence rate per 100K population (comparable to deathRate)

plot(Cancer$popEst2015,Cancer$incedenceCount, pch = 16, col = rgb(red = 1, green = 0, blue = 1, alpha = 0.5))
abline(lm(Cancer$incedenceCount~Cancer$popEst2015), col ='red')

plot(Cancer$incedenceCount,Cancer$deathRate * 100000, pch = 16, col = rgb(red = 1, green = 0, blue = 1, alpha = 0.5))

Cancer$deathsPerIncedencePercent <- Cancer$deathRate / Cancer$incedenceRate * 100 # deaths per incedence
library(plotly)

set.seed(100)
d <- diamonds[sample(nrow(diamonds), 1000), ]
plot_ly(d, x = ~carat, y = ~price, color = ~carat,
        size = ~carat, text = ~paste("Clarity: ", clarity))

library(caret)

library(reshape2)

CancerNUM <- Cancer[, !names(Cancer) %in% c('Geography', 'binnedInc')]
CancerCOR <- cor(CancerNUM, use = 'complete.obs')

subset(melt(CancerCOR), value < -0.7)
subset(melt(CancerCOR), value > +0.7 & value != 1.0)

