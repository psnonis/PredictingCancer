source('install.R')

source('load.R')

# Univariate Analysis

source('plots.R')

yxScatter(Cancer$avgAnnCount)
yxScatter(Cancer$MedianAge)
yxScatter(Cancer$incidence)

# Multivariate Analysis

Cancer.Numerical   <- Cancer[, !names(Cancer) %in% c('Geography','binnedInc','color')]
Cancer.Correlation <- cor(Cancer.Numerical, use = 'pairwise.complete.obs')

corrplot(Cancer.Correlation, method = 'circle', type = 'lower',  order = 'FPC', diag = F)

## Network Diagram

links <- subset(melt(Cancer.Correlation), value != 1.0 & abs(value) > 0.4)
links <- links[!duplicated(t(apply(links, 1, sort))),]

names(links)[1] = 'from'
names(links)[2] = 'to'
names(links)[3] = 'correlation'

links$magnitude <- abs(links$correlation)
links$width     <- 10^links$magnitude
links$color     <- ifelse(links$correlation < 0, 'red', 'green')

links.deathRate     <- links[links$from ==     'deathRate',]
links.incidenceRate <- links[links$from == 'incidenceRate',]

nodes <- data.frame('id' = names(Cancer.Numerical))

nodes$label                      <- nodes$id
nodes$shadow                     <- T
nodes$color.background           <- 'tomato'
nodes$color.border               <- 'black'
nodes$color.highlight.background <- 'orange'
nodes$color.highlight.border     <- 'darkred'

nodes$color.background[nodes$id=='deathRate'    ] = 'navy'
nodes$color.background[nodes$id=='incidenceRate'] = 'purple'

visNetwork(nodes, links.deathRate)
visNetwork(nodes, links.incidenceRate)
visNetwork(nodes, rbind(links.incidenceRate,links.deathRate))

## Top Correlated Variables to deathRate

yxScatter(Cancer$deathRate, Cancer$PctBachDeg25_Over)
yxScatter(Cancer$deathRate, Cancer$PctHS25_Over)
yxScatter(Cancer$deathRate, Cancer$medIncome)
yxScatter(Cancer$deathRate, Cancer$povertyPercent)
yxScatter(Cancer$deathRate, Cancer$PctPublicCoverage)
yxScatter(Cancer$deathRate, Cancer$PctEmployed16_Over)

## Top Correlated Variables to incidenceRate

yxScatter(Cancer$incidenceRate, Cancer$AvgHouseholdSize)
yxScatter(Cancer$incidenceRate, Cancer$MedianAgeFemale)
yxScatter(Cancer$incidenceRate, Cancer$MedianAge)
yxScatter(Cancer$incidenceRate, Cancer$MedianAgeMale)
yxScatter(Cancer$incidenceRate, Cancer$PctPublicCoverage)
yxScatter(Cancer$incidenceRate, Cancer$PctEmployed16_Over)
