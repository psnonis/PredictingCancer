map_setup <- function()
{
    CShape <- read_rds('cshape.Rds') # read county shape geometries, from census beauru

    CShape$State     <- CShape$state_name
    CShape$County    <- CShape$county_name
    CShape$Geography <- sprintf("%s, %s", CShape$county_name, CShape$state_name)

    CanFoo                                              <- Cancer
    CanFoo.incidenceMean                                <- mean(CanFoo$avgAnnCount[CanFoo$avgAnnCount!=1962.667684] / CanFoo$popEst2015[CanFoo$avgAnnCount!=1962.667684] * 100000)
    CanFoo$avgAnnCount[CanFoo$avgAnnCount==1962.667684] <- CanFoo.incidenceMean * CanFoo$popEst2015[CanFoo$avgAnnCount==1962.667684] / 100000
    CanFoo$incidenceRate                                <- CanFoo$avgAnnCount / CanFoo$popEst2015 * 100000
    CanFoo$deathRate                                    <- CanFoo$deathRate

    CanMap                 <<- left_join(CanFoo, CShape, by = 'Geography') # export to global scope
    CanMap.deathLimits     <<- c(min(CanFoo$deathRate),
                                 max(CanFoo$deathRate))
    CanMap.incidenceLimits <<- c(min(CanFoo$incidenceRate),
                                 max(CanFoo$incidenceRate))

    print('Loaded Cancer Map Data')
}

map_ca_deathRate <- function()
{
    CanMap %>%
        filter(state %in% c('CA')) %>%
        ggplot(aes(lon, lat, group = group, fill = deathRate)) +
        theme_wsj() +
        theme(legend.position = 'bottom',
              text = element_text(size = 12, family = family),
              title = element_text(size = 16, family = family),
              plot.title = element_text(hjust = 0.5),
              plot.subtitle = element_text(hjust = 0.5)) +
        ggtitle('Death Rate per 100K Population',
                subtitle = 'California') +
        geom_polygon(color = NA, size = 0.05) +
        coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
        scale_fill_gradientn(colors = rev(brewer.pal(11, 'RdYlGn')),
                             limits = CanMap.deathLimits)
}

map_wv_ky_deathRate <- function()
{
    CanMap %>%
        filter(state %in% c('WV','KY')) %>%
        ggplot(aes(lon, lat, group = group, fill = deathRate)) +
        theme_wsj() +
        theme(legend.position = 'bottom',
              text = element_text(size = 12, family = family),
              title = element_text(size = 16, family = family),
              plot.title = element_text(hjust = 0.5),
              plot.subtitle = element_text(hjust = 0.5)) +
        ggtitle('Death Rate per 100K Population',
                subtitle = 'Kansas + West Virginia') +
        geom_polygon(color = NA, size = 0.05) +
        coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
        scale_fill_gradientn(colors = rev(brewer.pal(11, 'RdYlGn')),
                             limits = CanMap.deathLimits)
}

map_us_deathRate <- function()
{
    CanMap %>%
        ggplot(aes(lon, lat, group = group, fill = deathRate)) +
        theme_wsj() +
        theme(legend.position = 'bottom',
              text = element_text(size = 12, family = family),
              title = element_text(size = 16, family = family),
              plot.title = element_text(hjust = 0.5),
              plot.subtitle = element_text(hjust = 0.5)) +
        ggtitle('Death Rate per 100K Population',
                subtitle = 'United States') +
        geom_polygon(color = NA, size = 0.05) +
        coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
        scale_fill_gradientn(colors = rev(brewer.pal(11, 'RdYlGn')),
                             limits = CanMap.deathLimits)
}

map_us_incidenceRate <- function()
{
    CanMap %>%
        ggplot(aes(lon, lat, group = group, fill = incidenceRate)) +
        theme_wsj() +
        theme(legend.position = 'bottom',
              text = element_text(size = 12, family = family),
              title = element_text(size = 16, family = family),
              plot.title = element_text(hjust = 0.5),
              plot.subtitle = element_text(hjust = 0.5)) +
        ggtitle('Incidence Rate per 100K Population',
                subtitle = 'United States') +
        geom_polygon(color = NA, size = 0.05) +
        coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
        scale_fill_gradientn(colors = rev(brewer.pal(11, 'RdYlGn')),
                             limits = CanMap.incidenceLimits)
}
