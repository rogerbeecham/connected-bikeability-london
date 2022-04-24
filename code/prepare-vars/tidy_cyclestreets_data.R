# -------------------------------------------
# R script for tidying-up cyclestreets routes data
# -------------------------------------------

# -------------------------------------------
# L I B R A R I E S
# -------------------------------------------
library(tidyverse)
library(sf)
library(geojsonsf)
library(jsonlite)
library(stringr)
library(tmap)

# In the cyclestreets batch request, users are able to choose different api version
# where API v1 is able to give detailed info about the journe , but slightly more difficult to get the geometry of the route (especially when data quantity is huge)
# data from API V2 has route geometry as a json text attribute. It can be easily converted to sf, but API V2 do not provide rich info as V1
# The solution is to combine results from both version,

#read the two datasets that are obtained from different API
data_route_details <- read.table("data/cyclestreets/cyclehire-data-API-V1.csv.gz", header=T, quote="\"", sep=",") %>%
  filter(!is.na(json))
data_route_geojson <- read.table("data/cyclestreets/cyclehire-data-API-V2.csv.gz", header=T, quote="\"", sep=",") %>%
  filter(!is.na(distance))

# (re)arrange the records to the same order
data_route_geojson <- data_route_geojson %>% dplyr::arrange(start_id,end_id)
data_route_details <- data_route_geojson %>% dplyr::select(start_id,end_id) %>%
  left_join(data_route_details, by = c('start_id'='start_id','end_id'='end_id'))

# convert geojson to sf geometry
data_route_geojson_sf<-geojson_sf(data_route_geojson$json) %>% as.data.frame() %>% filter(is.na(waypoint))

# attach geometry back to data_route_geojson dataframe
# could take up to 1 hour on an 10-Core Apple M1 PRO chip macbookpro with 32 GB RAM
data_route_geojson<- data_route_geojson %>% cbind(data_route_geojson_sf)

# parse the data from API V1 - rich info about the routes
# could take up to 5 hours on an 10-Core Apple M1 PRO chip macbookpro with 32 GB RAM
data_route_details_parse<- jsonlite::stream_in(textConnection(gsub("\\n", "", data_route_details$json)))
data_route_details_parse <- data_route_details_parse %>% dplyr::select(-waypoint)
data_route_details_parse<-data_route_details_parse %>% as.matrix() %>% as.data.frame()

# tidy up (shorten) the column names for the parsed data
colnames_temp<-colnames(data_route_details_parse)
colnames_temp<-str_sub(colnames_temp,start = 20, end= -1)
colnames(data_route_details_parse) <- colnames_temp


colnames(data_route_geojson)
colnames(data_route_details_parse)

# merge (cbind) the two data
routes_cyclestreets <- data_route_geojson %>% dplyr::select(start_id, end_id,
                                                            strategy,
                                                            distance, time_seconds,
                                                            geometry) %>%
  cbind(data_route_details_parse %>% dplyr::select(start,  finish,
                                                   start_longitude,     start_latitude,  
                                                   finish_longitude,    finish_latitude,
                                                   crow_fly_distance,   speed,
                                                   clientRouteId,       plan,               
                                                   time,                busynance,
                                                   quietness,
                                                   signalledJunctions,  signalledCrossings,
                                                   name,                walk,
                                                   leaving,             arriving,
                                                   coordinates,         elevations,
                                                   distances,           grammesCO2saved,
                                                   calories)) %>%
  rename(start_station_id = start_id,
         end_station_id = end_id) %>%
  st_as_sf()



head(routes_cyclestreets)
# output - cleaned and reformatted (sf) cyclestreets routes 
save(routes_cyclestreets, file = "data/cyclestreets/processed/routes_cyclestreets.Rdata")

