# -------------------------------------------
# R script for obtaining connected bikeability variables part 2
# focus on counting the number of different point locations and infrastructures (e.g. junctions) along cycle routes
# - the number of junctions along the routes (from OS openroad data)
# - the number of junctions that has advanced stop lines or signals (compare with the cyclestreets data) (from OS open road and CID)
# - the number of cycle parking along the routes (from CID)
# - the number of signage (relate to navigation) along the routes

# dataset used in this script
# (1) cyclestreets routes data (https://www.cyclestreets.net)
#
# (2) Transport for London Cycling Infrastructure Database (https://data.london.gov.uk/dataset/cycling-infrastructure-database;
#                                                           https://github.com/PublicHealthDataGeek/CycleInfraLnd )
# (3) Ordnance Survey Open Roads Data (https://www.ordnancesurvey.co.uk/business-government/products/open-map-roads)
# Author: Yuanxuan Yang
# -------------------------------------------

# -------------------------------------------
# L I B R A R I E S
# -------------------------------------------

# Required libraries.


# load the packages
# install.packages("devtools")
#devtools::install_github("PublicHealthDataGeek/CycleInfraLnd",force = T)
library(tidyverse)
library(CycleInfraLnd)
library(units)
library(sf)
library(tmap)
library(geosphere)
library(magicfor)
library(lubridate)
library(future.apply)


#####################
load("data/cyclestreets/processed/routes_cyclestreets_reorder.Rdata")
stations_sf<- st_read('data/lchs/lchs-stations.geojson')
routes_cyclestreets<-routes_cyclestreets %>% st_transform(crs= 27700)
bbox_lchs_routes<- st_bbox(routes_cyclestreets)



# Buffer width is 9.65 m,  This allowed for 3.65m lane width (Highways England, 2020) and 6m potential positional inaccuracy in OpenStreetMap (Haklay, 2010).
# Reference:
# Haklay, M., 2010. How Good is Volunteered Geographical Information? A Comparative Study of OpenStreetMap and Ordnance Survey Datasets. Environ. Plan. B Plan. Des. 37, 682–703. https://doi.org/10.1068/b35097 
# Highways England, 2020. Design Manual for Roads and Bridges: Road Layout Design CD 127 Cross-sections and headrooms (Version 1). Highways England [Online]. https://www.standardsforhighways.co.uk/dmrb/search/66f2661f-959d-4b13-8139-92fbd491cbcf (accessed 7.20.21). 

routes_cyclestreets_buffer= routes_cyclestreets %>% st_buffer(dist = 9.65) 

cid_lines_advanced_stopline <- get_cid_lines(type = "advanced_stop_line")%>% st_transform(crs = 27700)  %>%
  st_crop(bbox_lchs_routes) 

cid_points_signal <- get_cid_points(type = "signal") %>% st_transform(crs = 27700)  %>%
  st_crop(bbox_lchs_routes) 
# cid_points_restricted_point <- get_cid_points(type = "restricted_point") %>% st_transform(crs = 27700) %>%
#   st_crop(bbox_lchs_routes) 

cid_points_signage <- get_cid_points(type = "signage") %>% st_transform(crs = 27700) %>%
  st_crop(bbox_lchs_routes) 

cid_points_cycle_parking <- get_cid_points(type = "cycle_parking") %>% st_transform(crs = 27700) %>%
  st_crop(bbox_lchs_routes) 


cid_points_signage_navi <- cid_points_signage %>%
  filter((SS_DESTN == "TRUE")|(SS_QUIETW== "TRUE")|(SS_LCN == "TRUE")|(SS_SUPERH == "TRUE"))



road_junctions <- st_read('data/roads&boundaries/oproad_essh_gb/data/TQ_RoadNode.shp')

road_junctions<- road_junctions %>% 
  filter(formOfNode == "junction") %>%
  st_transform(crs = st_crs(routes_cyclestreets)) %>%
  st_crop(bbox_lchs_routes) 


safety_supported_junction_infras_buffer_1 <-
  cid_lines_advanced_stopline$geometry %>% st_buffer(dist = 25) %>% st_as_sf()

safety_supported_junction_infras_buffer_2 <-
  cid_points_signal$geometry %>% st_buffer(dist = 25) %>% st_as_sf()

safety_supported_junction_infras_buffer <- rbind(safety_supported_junction_infras_buffer_1,safety_supported_junction_infras_buffer_2) %>% st_union() %>% st_as_sf()

safety_supported_junction <- road_junctions %>% st_intersection(safety_supported_junction_infras_buffer) %>% st_as_sf()



start_time = Sys.time()
routes_junctions = st_intersects(routes_cyclestreets_buffer,road_junctions) %>% as.data.frame() %>%
  setNames(c("route_index","junction_index")) %>%
  group_by(route_index) %>%
  summarise(num_j=n())
end_time = Sys.time()
end_time - start_time



start_time = Sys.time()
routes_safety_supported_junction = st_intersects(routes_cyclestreets_buffer,safety_supported_junction) %>% as.data.frame() %>%
  setNames(c("route_index","safety_supported_junction_index")) %>%
  group_by(route_index) %>%
  summarise(num_sj=n())
end_time = Sys.time()
end_time - start_time




start_time = Sys.time()
routes_cycle_parking = st_intersects(routes_cyclestreets_buffer,cid_points_cycle_parking) %>% as.data.frame() %>%
  setNames(c("route_index","cycle_parking_index")) %>%
  group_by(route_index) %>%
  summarise(num_cp=n())
end_time = Sys.time()
end_time - start_time



start_time = Sys.time()
routes_signage_navi = st_intersects(routes_cyclestreets_buffer,cid_points_signage_navi) %>% as.data.frame() %>%
  setNames(c("route_index","signage_navi_index")) %>%
  group_by(route_index) %>%
  summarise(num_sign_navi=n())
end_time = Sys.time()
end_time - start_time



# bikeability_vars_part2 <- data.frame(route_index = c(1:nrow(routes_cyclestreets))) %>%
#   left_join(routes_junctions, by = c("route_index" = "route_index")) %>%
#   left_join(routes_safety_supported_junction, by = c("route_index" = "route_index")) %>%
#   left_join(routes_cycle_parking, by = c("route_index" = "route_index")) %>%
#   left_join(routes_signage_navi, by = c("route_index" = "route_index")) 

bikeability_vars_part2_seg <- data.frame(route_index = c(1:nrow(routes_cyclestreets_buffer))) %>%
  left_join(routes_junctions, by = c("route_index" = "route_index")) %>%
  left_join(routes_safety_supported_junction, by = c("route_index" = "route_index")) %>%
  left_join(routes_cycle_parking, by = c("route_index" = "route_index")) %>%
  left_join(routes_signage_navi, by = c("route_index" = "route_index")) %>%
  mutate(route_index = route_index+(min(routes_cyclestreets_buffer$route_id)-1))


#save(bikeability_vars_part2,file = "output/temp/bikeability_vars_part2.Rdata")
#save(bikeability_vars_part2_seg,file = "output/temp/bikeability_vars_part2_seg_1_200000.Rdata")
#save(bikeability_vars_part2_seg,file = "output/temp/bikeability_vars_part2_seg_200001_400000.Rdata")
#save(bikeability_vars_part2_seg,file = "output/temp/bikeability_vars_part3_seg_400001_600000.Rdata")
save(bikeability_vars_part2_seg,file = "output/temp/bikeability_vars_part4_seg_600001_end.Rdata")
