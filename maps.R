
# Cancer <- separate(Cancer, col = Geography, into = c('county_name','state_name'), sep = ', ', remove = FALSE)
# Cancer <- separate(Cancer, col = Geography, into = c("County","State"), sep = ", ", remove = FALSE)

CShape <- read_rds('cshape.Rds') # read county shape geometries

CShape$Geography <- sprintf("%s, %s", CShape$county_name, CShape$state_name)

# saveRDS( CShape, file = 'cshape.Rds' )

# windows()

CanMap <- left_join(Cancer, CShape, by = 'Geography')

map1 <- function()
{
    CanMap %>%
        filter(state %in% c('WV','KY')) %>%
        ggplot(aes(lon, lat, group = group, fill = deathRate)) +
        theme_wsj() +
        theme(legend.position = 'bottom',
              text = element_text(size = 16, family = 'gloria'),
              title = element_text(size = 30, family = 'gloria'),
              plot.title = element_text(hjust = 0.5),
              plot.subtitle = element_text(hjust = 0.5)) +
        ggtitle('Death Rate per 100,000 Population',
                subtitle = 'Kansas + West Virginia') +
        geom_polygon(color = NA, size = 0.05) +
        coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
        scale_fill_gradientn(colors = rev(brewer.pal(11, 'RdYlGn')),
                             limits = c(min(Cancer$deathRate),
                                        max(Cancer$deathRate)))
}

map2 <- function()
{
    CanMap %>%
        ggplot(aes(lon, lat, group = group, fill = deathRate)) +
        theme_wsj() +
        theme(legend.position = 'bottom',
              text = element_text(size = 16, family = 'gloria'),
              title = element_text(size = 30, family = 'gloria'),
              plot.title = element_text(hjust = 0.5),
              plot.subtitle = element_text(hjust = 0.5)) +
        ggtitle('Death Rate per 100,000 Population',
                subtitle = 'United States') +
        geom_polygon(color = NA, size = 0.05) +
        coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
        scale_fill_gradientn(colors = rev(brewer.pal(11, 'RdYlGn')),
                             limits = c(min(Cancer$deathRate),
                                        max(Cancer$deathRate)))
}

map3 <- function()
{
    CanMap %>%
        ggplot(aes(lon, lat, group = group, fill = incidenceRate)) +
        theme_wsj() +
        theme(legend.position = 'bottom',
              text = element_text(size = 16, family = 'gloria'),
              title = element_text(size = 30, family = 'gloria'),
              plot.title = element_text(hjust = 0.5),
              plot.subtitle = element_text(hjust = 0.5)) +
        ggtitle('Incidence Rate per 100,000 Population',
                subtitle = 'United States') +
        geom_polygon(color = NA, size = 0.05) +
        coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
        scale_fill_gradientn(colors = rev(brewer.pal(11, 'RdYlGn')),
                             limits = c(min(Cancer$incidenceRate),
                                        max(Cancer$incidenceRate)))
}
