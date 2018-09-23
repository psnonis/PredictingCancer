#library(showtext)
#windows()
#font.add.google('Gloria Hallelujah', 'gloria')
#showtext.auto()

Cancer <- read.csv('cancer.csv', header = T, as.is = T, row.names = 1)

Cancer$color <- '#FF34B380'
Cancer$color[Cancer$avgAnnCount==1962.667684] <- '#8B1C6280'

incedenceMean <- mean(Cancer$avgAnnCount[Cancer$avgAnnCount!=1962.667684] / Cancer$popEst2015[Cancer$avgAnnCount!=1962.667684] * 100000) # mean incedence across the u.s.
Cancer$avgAnnCount[Cancer$avgAnnCount==1962.667684] <- incedenceMean * Cancer$popEst2015[Cancer$avgAnnCount==1962.667684] / 100000 # replace anomalous values with imputation
Cancer$incedence <- Cancer$avgAnnCount / Cancer$popEst2015 * 100000 # new cancer cases per 100,000 persons per year

plot(Cancer$incedence, pch = 16, col = Cancer$color, family = 'gloria')

CancerNUM <- Cancer[, !names(Cancer) %in% c('Geography','binnedInc','color')]
CancerCOR <- cor(CancerNUM, use = 'complete.obs')

library(visNetwork)
library(reshape2)

links <- subset(melt(CancerCOR), value != 1.0 & abs(value) > 0.4)
links <- links[!duplicated(t(apply(links, 1, sort))),]

names(links)[1] = 'from'
names(links)[2] = 'to'
names(links)[3] = 'correlation'

links$width <- 10^abs(links$correlation)
links$color <- ifelse(links$correlation < 0, 'red', 'green')

nodes <- data.frame('id' = names(CancerCOR[1,]))

nodes$label <- nodes$id
nodes$shadow <- T

nodes$color.background <- 'tomato'
nodes$color.border <- 'black'
nodes$color.highlight.background <- 'orange'
nodes$color.highlight.border <- 'darkred'

visNetwork(nodes, links)
