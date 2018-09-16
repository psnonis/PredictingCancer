setwd("C:/Berkeley/w203-lab1")

library(tidyverse)

Cancer <- read.csv('cancer.csv', header = T, as.is = T, row.names = 1)
#Cancer <- separate(Cancer, col = Geography, into = c('county_name','state_name'), sep = ', ', remove = FALSE)

CShape <- read_rds('cshape.Rds') # read county geometries
CShape$Geography <- sprintf("%s, %s", CShape$county_name, CShape$state_name)

Cancer2 <- left_join(Cancer, CShape, by = 'Geography')

Cancer2 %>%
  filter(state %in% c("CA")) %>%
  ggplot(aes(lon, lat, group = group, fill = deathRate)) +
  geom_polygon(color = NA, size = 0.05) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45)

Cancer2 %>%
  ggplot(aes(lon, lat, group = group, fill = BirthRate)) +
  geom_polygon(color = NA, size = 0.05) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45)


h <- hist(Cancer$AvgHouseholdSize, labels = TRUE, col = "pink", breaks = 5)

sort(Cancer$AvgHouseholdSize[Cancer$AvgHouseholdSize < 2.0])
summary(Cancer$AvgHouseholdSize)
Cancer$Geography[Cancer$AvgHouseholdSize < 2.0]
Cancer <- separate(Cancer, col = Geography, into = c("County","State"), sep = ", ", remove = FALSE)

