# install.packages(c("sf", "dplyr", "ggplot2", "ggspatial", "prettymapr"))
library(sf)

rwandaBound <- st_read('~/R/Data files/Shapefiles/aez/aez.shp')

# convert to dataframe

RwBound_df <- fortify(rwandaBound)

RwMap <-ggplot(RwBound_df, aes(long, lat, group = group)) +
  geom_polygon(color = factor(id)) +
  coord_equal() +
  labs(x = 'Longitude (degrees)', 
       y = 'Latitude (degrees)',
       title = 'Rwanda agroecological zones', 
       subtitle = ' ')

