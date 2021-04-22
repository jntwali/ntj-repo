##########################

#load twitter library-the retweet is recommended now over tweetr
library(rjson)
library(jsonlite)
#plotting and pipes - tidyvers
library(ggplot2)
library(dplyr)
library(tidyr)

#animated maps
# to install: devtools::install_github("dgrtwo/gganimate")
#note this required imagemagick to be installed

library(leaflet)
library(gganimate)
library(lubridate)
library(maps)
library(ggthemes)

options(stringsAsFactors = F)

#####################################

#create a file path

json_file <- "~/R/boulder_flood_geolocated_tweets.json"

boulder_flood_tweets <- stream_in(file(json_file))

#################################

#create new df with just the tweet texts and usernames

tweet_data <- data.frame(date_time = boulder_flood_tweets$created_at,
                         username = boulder_flood_tweets$user$screen_name,
                         tweet_text = boulder_flood_tweets$text,
                         coords = boulder_flood_tweets$coordinates)

# flood start date spet 13 24 (end of incident)

start_date <- as.POSIXct('2013-09-13 00:00:00')
end_date <- as.POSIXct('2013-09-24 00:00:00')

# cleanup & and filter to just the time period around the flood
flood_tweet <- tweet_data %>%
  mutate(coords.coordinates = gsub("\\)|c\\(","", coords.coordinates),
    date_time = as.POSIXct(date_time, format= "%a %b %d %H:%M:%S +0000 %Y"))%>%
  separate(coords.coordinates, c("long", "lat"),sep = ",")%>%
  mutate_at(c("lat", "long"), as.numeric)%>%
  filter(date_time >= start_date & date_time <= end_date)
  
################################

# create a base map of the globe


