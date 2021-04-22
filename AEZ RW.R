rwandaBound <- readOGR(dsn = '~/R/Data files/Shapefiles/aez', 'acz_geo')

# convert to dataframe

RwBound_df <- fortify(rwandaBound)

RwMap <-ggplot(RwBound_df, aes(long, lat, group = group)) +
  geom_polygon(color = factor(id)) +
  coord_equal() +
  labs(x = 'Longitude (degrees)', 
       y = 'Latitude (degrees)',
       title = 'Rwanda agroecological zones', 
       subtitle = ' ')

