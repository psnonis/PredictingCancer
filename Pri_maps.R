  setwd("C:/Berkeley/w203-lab1")

  library(tidyverse)

  Cancer <- read.csv('cancer.csv', header = T, as.is = T, row.names = 1)
# Cancer <- separate(Cancer, col = Geography, into = c('county_name','state_name'), sep = ', ', remove = FALSE)

  CShape <- read_rds('cshape.Rds') # read county geometries
  CShape$Geography <- sprintf("%s, %s", CShape$county_name, CShape$state_name)

  CanMap <- left_join(Cancer, CShape, by = 'Geography')

  CanMap %>%
    filter(state %in% c('NV')) %>%
    ggplot(aes(lon, lat, group = group, fill = deathRate)) +
    geom_polygon(color = NA, size = 0.05) +
    coord_map(projection = 'albers', lat0 = 39, lat1 = 45)

  CanMap %>%
    ggplot(aes(lon, lat, group = group, fill = BirthRate)) +
    geom_polygon(color = NA, size = 0.05) +
    coord_map(projection = "albers", lat0 = 39, lat1 = 45)

  sum(Cancer$deathRate)
  
  
  
h <- hist(Cancer$AvgHouseholdSize, labels = TRUE, col = "pink", breaks = 5)

sort(Cancer$AvgHouseholdSize[Cancer$AvgHouseholdSize < 2.0])
summary(Cancer$AvgHouseholdSize)
Cancer$Geography[Cancer$AvgHouseholdSize < 2.0]
Cancer <- separate(Cancer, col = Geography, into = c("County","State"), sep = ", ", remove = FALSE)


# Death Rate
# The ratio of deaths to the population of a particular area during a particular period of time, usually calculated as the number of deaths per one thousand people per year.

Cancer$deathEst2015 <- Cancer$popEst2015/100000*Cancer$deathRate
Cancer$deathPercent2015 <- Cancer$deathEst2015 / Cancer$avgAnnCount * 100

sum(Cancer$deathEst2015)

hist(Cancer$popEst2015)


sum(Cancer$popEst2015)

sum(Cancer$avgAnnCount)

library(ggplot2)

library(RColorBrewer)

boxHist <- function(v,name)
{
  vLen    <- length(v)
  vMin    <- min(v, na.rm = T)
  vMax    <- max(v, na.rm = T)
  vBreaks <- seq(vMin, vMax, length.out = 10)
  vMean   <- mean(v, na.rm = T)
  vMedian <- median(v, na.rm = T)
  vSD     <- sd(v)

  l       <- layout(mat = matrix(c(1,2),2,1, byrow=T), height = c(1,8))

  p       <- par(mar = c(0, 3.1, 1.1, 2.1))
  b       <- boxplot(v, horizontal = T, xaxt = 'n', col ='#5E4FA2', frame = F)

  par(mar=c(4, 3.1, 1.1, 2.1))
  h        <- hist(v, col = c('#9E0142','#D53E4F','#F46D43','#FDAE61','#FEE08B','#FFFFBF','#E6F598','#ABDDA4','#66C2A5','#3288BD','#5E4FA2'),
                   breaks = vBreaks,
                   xlab = name,
                   labels = T )

  xFit     <- seq(vMin, vMax, length.out = 100)
  yFit     <- dnorm(xFit, mean = vMean, sd = vSD)
  yFit     <- yFit * diff(h$mids[1:2]) * vLen
  
  lines(xFit, yFit, col = 'darkblue', lwd = 3)
  abline(v = vMean, col = 'red', lwd = 3, lty = 1)
  abline(v = vMedian, col = 'red', lwd = 3, lty = 2)
}

boxHist(Cancer$deathRate, '2015 Cancer Deaths per 100K People')

print(brewer.pal(11,'Spectral'))
Cancer[Cancer$deathEst2015>4000,"Geography"]
