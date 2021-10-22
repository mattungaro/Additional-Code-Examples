# Open Street Map API

# https://dominicroye.github.io/en/2018/accessing-openstreetmap-data-with-r/
  
#install the osmdata, sf, tidyverse and ggmap package
if(!require("osmdata")) install.packages("osmdata")
if(!require("tidyverse")) install.packages("tidyverse")
if(!require("sf")) install.packages("sf")
if(!require("ggmap")) install.packages("ggmap")

#load packages
library(tidyverse)
library(osmdata)
library(sf)
library(ggmap)


head(available_tags("amenity"))
head(available_tags("shop"))

# anime shops hahaha

#building the query
q <- getbb("Madrid") %>%
  opq() %>%
  add_osm_feature("amenity", "cinema")

str(q) #query structure


cinema <- osmdata_sf(q)
cinema

#our background map
mad_map <- get_map(getbb("Madrid"), maptype = "toner-background")

#final map
ggmap(mad_map)+
  geom_sf(data = cinema$osm_points,
          inherit.aes = FALSE,
          colour = "#238443",
          fill = "#004529",
          alpha = .5,
          size = 4,
          shape = 21)+
  labs(x = "", y = "")


p <- getbb("San Francisco Bay Area") %>% 
  opq() %>% 
  add_osm_feature("shop", "anime")

str(p)


anime <- osmdata_sf(p)
anime

#our background map
mad_map <- get_map(getbb("San Francisco Bay Area"), maptype = "toner-background")

#final map
ggmap(mad_map)+
  geom_sf(data = anime$osm_points,
          inherit.aes = FALSE,
          colour = "red",
          fill = "red",
          alpha = .5,
          size = 4,
          shape = 21)+
  labs(x = "", y = "")


library(ggmap)


api <- "AIzaSyDIcjAnBDD1eRdJO06DNFaOdALrayfSDYg"
register_google(key = api)

has_google_key()
google_key()
has_google_client()
has_google_signature()

geocode("waco, texas", urlonly = TRUE)
ggmap_show_api_key()
geocode("waco, texas", urlonly = TRUE)
ggmap_hide_api_key()
geocode("waco, texas", urlonly = TRUE)

library(googleway)

google_places(search_string = "Restaurants in Melbourne, Australia",
              key = api)

# https://www.r-bloggers.com/2019/03/ranking-places-with-google-to-create-maps/ 
ramen1 <- google_places(search_string = "Ramen shops in Florida", key = api)

ramen1_loc <- ramen1$results

token <- ramen1$next_page_token

ramen1_loc$rating

ramen1_results <- ramen1_loc

ramen2 <- google_places(search_string = "Ramen shops in Florida", key = api, page_token = token)
ramen2_results <- ramen2$results


token <- gmaps_request$next_page_token
Sys.sleep(5)
continue <- TRUE
  while (continue){
  
  gmaps_request <- google_places(search_string = "Ramen shops in Florida", 
                                 key = api, page_token = token)
  gmaps_data <- gmaps_request$results
  
  if (!is.null(gmaps_request$next_page_token)) {
    place_id = c(place_id,gmaps_data$place_id)
    rating = c(rating,gmaps_data$rating)
    token <- gmaps_request$next_page_token
    Sys.sleep(5)
  }
  else{continue <- FALSE}
  
  
  }

ramen1_results %>% select(-c(photos)) -> ramen1_results
ramen2_results %>% select(-c(photos)) -> ramen2_results
ramen3_results %>% select(-c(photos)) -> ramen3_results
ramen1_results <- as_tibble(ramen1_results) 
ramen2_results <- as_tibble(ramen2_results) 
ramen3_results <- as_tibble(ramen3_results)
# need to make new tables
ramen1_a <- ramen1_results$geometry
ramen1_b <- ramen1_results %>% select(name, rating, price_level, reference, user_ratings_total,
                        formatted_address, reference, icon)
ramen1_mod <- cbind(ramen1_a, ramen1_b)

ramen2_a <- ramen2_results$geometry
ramen2_b <- ramen2_results %>% select(name, rating, price_level, reference, user_ratings_total,
                                      formatted_address, reference, icon)
ramen2_mod <- cbind(ramen2_a, ramen2_b)

ramen3_a <- ramen3_results$geometry
ramen3_b <- ramen3_results %>% select(name, rating, price_level, reference, user_ratings_total,
                                      formatted_address, reference, icon)
ramen3_mod <- cbind(ramen3_a, ramen3_b)

ramen_total <- vctrs::vec_c(ramen1_mod, ramen2_mod, ramen3_mod)

ramen_total %>% select(location) -> ramen_location
do.call(data.frame, ramen_total) -> ramen_attempt

install.packages(c("cowplot", "googleway", "ggplot2", "ggrepel", 
                   "ggspatial", "libwgeom", "sf", "rnaturalearth", "rnaturalearthdata"))

library("rnaturalearth")
library("rnaturalearthdata")

world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)


ggplot(data = world) +
  geom_sf() +
  geom_point(data = ramen_attempt, aes(x = location.lng, y = location.lat), size = 2, 
             shape = 23, fill = "darkred") +
  coord_sf(xlim = c(-88, -78), ylim = c(24.5, 33), expand = FALSE)
