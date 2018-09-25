map_setup <- function()
{
  # Cancer <- separate(Cancer, col = Geography, into = c('county_name','state_name'), sep = ', ', remove = FALSE)
  # Cancer <- separate(Cancer, col = Geography, into = c("County","State"), sep = ", ", remove = FALSE)

    CShape <- read_rds('cshape.Rds') # read county shape geometries, from census beauru

    CShape$Geography <- sprintf("%s, %s", CShape$county_name, CShape$state_name)

  # saveRDS( CShape, file = 'cshape.Rds' )

    CanMap <<- left_join(Cancer, CShape, by = 'Geography') # export to global scope

    print('Loaded Cancer Map')
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
                             limits = c(min(Cancer$deathRate),
                                        max(Cancer$deathRate)))
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
                             limits = c(min(Cancer$deathRate),
                                        max(Cancer$deathRate)))
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
                             limits = c(min(Cancer$deathRate),
                                        max(Cancer$deathRate)))
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
                             limits = c(min(Cancer$incidenceRate),
                                        max(Cancer$incidenceRate)))
}
