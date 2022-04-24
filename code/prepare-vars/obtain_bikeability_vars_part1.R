# -------------------------------------------
# R script for obtaining connected bikability variables - part 1
# Datasets used in this script
# (1) cyclestreets routes data (https://www.cyclestreets.net)
#
# (2) Transport for London Cycling Infrastructure Database (https://data.london.gov.uk/dataset/cycling-infrastructure-database;
#                                                           https://github.com/PublicHealthDataGeek/CycleInfraLnd )
# (3) Ordnance Survey Open Roads Data (https://www.ordnancesurvey.co.uk/business-government/products/open-map-roads)
# 
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



# -------------------------------------------
# LOAD DATASETS
# -------------------------------------------


# load cyclestreets routing data
load("data/cyclestreets/processed/routes_cyclestreets.Rdata")
# transform projection to "OSGB 1936 / British National Grid - EPSG:27700"
routes_cyclestreets<-routes_cyclestreets %>% st_transform(crs= 27700)
bbox_lchs_routes<- st_bbox(routes_cyclestreets)

# load london cycle hire scheme station data
stations_sf<- st_read('data/lchs/lchs-stations.geojson')


#--------------------------------------------------------------------------------------
# Load London's Cycling Infrastructure Database (CID) data
# Cycle lanes and tracks – including whether they are segregated or painted lanes
# Cycle parking, including the type and capacity of parking
# Signalised crossings for cycles
# Restricted route – Modal filters and traffic gates which allow cycles to pass but restrict car traffic
# Traffic calming, including the location of all speed humps in Greater London
# Advanced stop lines – boxes at junctions for people cycling
# Signals – early-release signals at junctions
# Signage – Signed cycle routes and other wayfinding
# Restricted Points – points where people cycling will have to dismount Paths through parks and other green spaces that can, and cannot, be cycled on
#--------------------------------------------------------------------------------------

cid_points_signal <- get_cid_points(type = "signal") %>% st_transform(crs = 27700)
cid_points_signage <- get_cid_points(type = "signage") %>% st_transform(crs = 27700)
cid_points_cycle_parking <- get_cid_points(type = "cycle_parking") %>% st_transform(crs = 27700)
cid_lines_cycle_lane_track <- get_cid_lines(type = "cycle_lane_track") %>% st_transform(crs = 27700)

#---- 5 level of segregation (on road)--------------
# Recode Cycle Lane Track (CLT) in CID into 5 categories based on segregation
# 4 on-road categories and 1 off-road category

# Convert Factors to numeric in cycle lanes track dataset
# e.g.  CLT_SEGREG: Factor w/ 2 levels "FALSE","TRUE": 1 1 1 1 1 1 1 1 1 1 ...
on_road <- cid_lines_cycle_lane_track %>% filter(CLT_CARR == "TRUE") 
on_road$CLT_SEGREG <- factor(on_road$CLT_SEGREG, levels = c("FALSE","TRUE"))
on_road$CLT_STEPP <- factor(on_road$CLT_STEPP, levels = c("FALSE","TRUE"))
on_road$CLT_PARSEG <- factor(on_road$CLT_PARSEG, levels = c("FALSE","TRUE"))
on_road$CLT_MANDAT <- factor(on_road$CLT_MANDAT, levels = c("FALSE","TRUE"))
on_road$CLT_ADVIS <- factor(on_road$CLT_ADVIS, levels = c("FALSE","TRUE"))
on_road$CLT_SHARED <- factor(on_road$CLT_SHARED, levels = c("FALSE","TRUE"))
on_road$CLT_CONTRA <- factor(on_road$CLT_CONTRA, levels = c("FALSE","TRUE"))
on_road$CLT_PARKR <- factor(on_road$CLT_PARKR, levels = c("FALSE","TRUE"))

on_road_numeric = on_road %>%
  mutate(CLT_SEGREG_NUMERIC = as.numeric(on_road$CLT_SEGREG)) %>%
  mutate(CLT_STEPP_NUMERIC = as.numeric(on_road$CLT_STEPP))  %>%
  mutate(CLT_PARSEG_NUMERIC = as.numeric(on_road$CLT_PARSEG))  %>%
  mutate(CLT_MANDAT_NUMERIC = as.numeric(on_road$CLT_MANDAT))  %>%
  mutate(CLT_ADVIS_NUMERIC = as.numeric(on_road$CLT_ADVIS)) %>%
  mutate(CLT_SHARED_NUMERIC = as.numeric(on_road$CLT_SHARED)) %>%
  mutate(CLT_CONTRA_NUMERIC = as.numeric(on_road$CLT_CONTRA)) %>%
  mutate(CLT_PARKR_NUMERIC = as.numeric(on_road$CLT_PARKR))
# converts all False to 1 and True to 2

# Convert 1(false) to 0 and 2(true) to 1
on_road_numeric$CLT_SEGREG_NUMERIC = ifelse(on_road_numeric$CLT_SEGREG_NUMERIC == 1, 0, 1)
on_road_numeric$CLT_STEPP_NUMERIC = ifelse(on_road_numeric$CLT_STEPP_NUMERIC == 1, 0, 1)
on_road_numeric$CLT_PARSEG_NUMERIC = ifelse(on_road_numeric$CLT_PARSEG_NUMERIC == 1, 0, 1)
on_road_numeric$CLT_MANDAT_NUMERIC = ifelse(on_road_numeric$CLT_MANDAT_NUMERIC == 1, 0, 1)
on_road_numeric$CLT_ADVIS_NUMERIC = ifelse(on_road_numeric$CLT_ADVIS_NUMERIC == 1, 0, 1)


# Recode to give weighted value with segregated weighted highest and advisory cycle lane weighted lowest
on_road_numeric$CLT_SEGREG_weight = ifelse(on_road_numeric$CLT_SEGREG_NUMERIC == 1, 10000, 0)
on_road_numeric$CLT_STEPP_weight = ifelse(on_road_numeric$CLT_STEPP_NUMERIC == 1, 1000, 0)
on_road_numeric$CLT_PARSEG_weight = ifelse(on_road_numeric$CLT_PARSEG_NUMERIC == 1, 100, 0)
on_road_numeric$CLT_MANDAT_weight = ifelse(on_road_numeric$CLT_MANDAT_NUMERIC == 1, 10, 0)
on_road_numeric$CLT_ADVIS_weight = ifelse(on_road_numeric$CLT_ADVIS_NUMERIC == 1, 1, 0)

# Create new column with the sum of the weights for the 5 classes of separation
on_road_numeric = on_road_numeric %>%
  rowwise() %>%
  mutate(weight_5 = sum(c_across(CLT_SEGREG_weight:CLT_ADVIS_weight)))

#       weight_5 count
#           <dbl> <int>
# 1            0  3372  # none of the 5 categories - might be shared or contraf
# 2            1  7196  advisory cycle lane only
# 3           10  1672  mand cycle lane only
# 4          100   100  part segregated only
# 5          101    73  part seg + advisory
# 6          110   176  part seg + mand
# 7         1000     3  stepped only
# 8         1001     2  stepped + advisory 
# 9        10000  1274  segregated only
# 10       10001     2  segregated + advisory
# 11       10010     6  segregated + mandatory
# 12       11000    89  segregated + stepped


##  Create factored column where labelled by the 'highest' degree of separation
# Factor weight_5
on_road_factor = on_road_numeric %>%
  mutate(Highest_separation = factor(weight_5))
rm(on_road_numeric) # remove this dataframe to avoid confusion


# Convert factored numbers to relevant labels
on_road_factor = on_road_factor %>%
  mutate(Highest_separation = fct_collapse(Highest_separation, 
                                           "Full segregation" = c("10000","10001","10010", "11000"),
                                           "Stepped/part segregation" = c("1000", "1001", "100", "101", "110"),
                                           "Mandatory/Advisory cycle lane" = c("1", "10"),
                                           "No separation" = c("0")))
# Relevel order of factors in the degree of separation
on_road_factor = on_road_factor %>%
  mutate(Highest_separation = fct_relevel(Highest_separation, 
                                          c("Full segregation", 
                                            "Stepped/part segregation",
                                            "Mandatory/Advisory cycle lane",
                                            "No separation")))


off_road = cid_lines_cycle_lane_track %>% filter(CLT_CARR == "FALSE")


#------------------------------------------------------------------------------
# Output the cyclestreets route data - reorder and add a column of "route_id"
#------------------------------------------------------------------------------
routes_cyclestreets<-routes_cyclestreets %>%
  dplyr::arrange(start_station_id, end_station_id) %>%
  st_as_sf()

routes_cyclestreets$route_id=c(1:nrow(routes_cyclestreets))

save(routes_cyclestreets, file = "data/cyclestreets/processed/routes_cyclestreets_reorder.Rdata")


#------------------------------------------------------------------------------
# Matching CID cycle lane track and Cyclestreets routes data
# CID cycle lane tracks are obtained by surveying, although they are consistent with the road network, 
#------------------------------------------------------------------------------



# Buffers are used to match CID Cycle lane track data and Cyclestreets routes (based on OSM)
# Buffer width is 9.65 m,  This allowed for 3.65m lane width (Highways England, 2020) and 6m potential positional inaccuracy in OpenStreetMap (Haklay, 2010).
# Reference:
# Haklay, M., 2010. How Good is Volunteered Geographical Information? A Comparative Study of OpenStreetMap and Ordnance Survey Datasets. Environ. Plan. B Plan. Des. 37, 682–703. https://doi.org/10.1068/b35097 
# Highways England, 2020. Design Manual for Roads and Bridges: Road Layout Design CD 127 Cross-sections and headrooms (Version 1). Highways England [Online]. https://www.standardsforhighways.co.uk/dmrb/search/66f2661f-959d-4b13-8139-92fbd491cbcf (accessed 7.20.21). 

on_road_factor_buffer <- on_road_factor %>% st_transform(crs = 27700) %>%
  st_buffer(dist = 9.65, endCapStyle = "FLAT")

off_road_buffer <- off_road %>% st_transform(crs = 27700) %>%
  st_buffer(dist = 9.65, endCapStyle = "FLAT")

# crop cycle lane track in central london 
on_road_factor_buffer <- on_road_factor_buffer %>% sf::st_crop(bbox_lchs_routes)
off_road_buffer <- off_road_buffer %>% sf::st_crop(bbox_lchs_routes)


#cltb is short for cycle lane track buffer
cid_lines_cltb_fullseg <- on_road_factor_buffer %>% 
  filter(CLT_CARR == "TRUE") %>%
  filter(Highest_separation == "Full segregation") %>% st_union() %>% st_as_sf()

cid_lines_cltb_stepped_part_seg <- on_road_factor_buffer %>%
  filter(CLT_CARR == "TRUE") %>%
  filter(Highest_separation == "Stepped/part segregation") %>% st_union() %>% st_as_sf()

cid_lines_cltb_mand_adv <- on_road_factor_buffer %>% 
  filter(CLT_CARR == "TRUE") %>%
  filter(Highest_separation == "Mandatory/Advisory cycle lane") %>% st_union() %>% st_as_sf()

cid_lines_cltb_no_sep <- on_road_factor_buffer %>% 
  filter(CLT_CARR == "TRUE") %>%
  filter(Highest_separation == "No separation") %>% st_union() %>% st_as_sf()

cid_lines_cltb_off_road <- off_road_buffer %>% st_union() %>% st_as_sf()

cid_lines_cltb_park_river <- cid_lines_cycle_lane_track %>% 
  sf::st_crop(bbox_lchs_routes) %>% 
  filter((CLT_PARKR == "TRUE")|CLT_WATERR == "TRUE")%>%
  st_buffer(dist = 9.65, endCapStyle = "FLAT") %>%
  st_union() %>% st_as_sf()

tmap_options(check.and.fix = TRUE)
tmap_mode("view")
tm_shape(cid_lines_cltb_fullseg)+
  tm_polygons(col="#50C1E9", border.alpha = 0,alpha = 0.6)+
  tm_shape(cid_lines_cltb_stepped_part_seg)+
  tm_polygons(col = "#fdc4b6", border.alpha = 0,alpha = 0.6)+
  tm_shape(cid_lines_cltb_mand_adv)+
  tm_polygons(col= "#96ceb4", border.alpha = 0,alpha = 0.6)+
  tm_shape(cid_lines_cltb_no_sep)+
  tm_polygons(col= "#dadada", border.alpha = 0,alpha = 0.6)+
  tm_shape(on_road_factor)+
  tm_lines()


# function for calculating the distance of each type of cycle track in cyclestreets routes

get_dist_routes_on_clt<-function(cyclestreets_routes,cid_clt){
  cyclestreets_routes_seg <- st_intersection(cyclestreets_routes,cid_clt)
  cyclestreets_routes_seg <- cyclestreets_routes_seg %>% mutate(seg_cid_dist = st_length(.))
  cyclestreets_routes_cid_seg_dist <- cyclestreets_routes_seg %>% st_drop_geometry() %>%
    dplyr::select(start_station_id, end_station_id, seg_cid_dist)
  return(cyclestreets_routes_cid_seg_dist)
}


# gather the distance of cyclestreets routes on different types (segregation) of CID cycle lane tracks
cyclestreets_routes_length_on_clt <- routes_cyclestreets %>%
  st_drop_geometry() %>%
  dplyr::select(start_station_id, end_station_id) %>%
  left_join(get_dist_routes_on_clt(routes_cyclestreets, cid_lines_cltb_fullseg) %>%
              rename(dist_fullseg=seg_cid_dist),
            by = c("start_station_id"="start_station_id", "end_station_id"="end_station_id") ) %>%
  left_join(get_dist_routes_on_clt(routes_cyclestreets, cid_lines_cltb_stepped_part_seg) %>%
              rename(dist_stepped_part_seg=seg_cid_dist),
            by = c("start_station_id"="start_station_id", "end_station_id"="end_station_id") ) %>%
  left_join(get_dist_routes_on_clt(routes_cyclestreets, cid_lines_cltb_mand_adv) %>%
              rename(dist_mand_adv=seg_cid_dist),
            by = c("start_station_id"="start_station_id", "end_station_id"="end_station_id") ) %>%
  left_join(get_dist_routes_on_clt(routes_cyclestreets, cid_lines_cltb_no_sep) %>%
              rename(dist_no_sep=seg_cid_dist),
            by = c("start_station_id"="start_station_id", "end_station_id"="end_station_id") ) %>%
  left_join(get_dist_routes_on_clt(routes_cyclestreets, cid_lines_cltb_park_river) %>%
              rename(dist_park_river=seg_cid_dist),
            by = c("start_station_id"="start_station_id", "end_station_id"="end_station_id") ) %>%
  left_join(get_dist_routes_on_clt(routes_cyclestreets, cid_lines_cltb_off_road) %>%
              rename(dist_off_road=seg_cid_dist),
            by = c("start_station_id"="start_station_id", "end_station_id"="end_station_id") ) 

# replace NA with 0
cyclestreets_routes_length_on_clt[is.na(cyclestreets_routes_length_on_clt)] = 0



# obtain variables from cyclestreets data and join the data of distance of cyclestreets routes on different types (segregation) of CID cycle lane tracks
bikeability_vars_part1 <- routes_cyclestreets %>% st_drop_geometry() %>%
  dplyr::select(start_station_id, end_station_id, 
                crow_fly_distance,distance,
                busynance, 
                quietness,
                signalledJunctions,
                signalledCrossings,
                walk
                ) %>%
  mutate(directness = as.numeric(crow_fly_distance)/as.numeric( distance))


bikeability_vars_part1<-bikeability_vars_part1 %>% left_join(cyclestreets_routes_length_on_clt, 
                                                 by = c("start_station_id"="start_station_id",
                                                        "end_station_id"="end_station_id"))

head(bikeability_vars_part1)


############################################################################
## calculate distance on low slope roads (uphill) using cyclestreets data
############################################################################

# define the function for geting low slope ratio
# low slope is defined as a maximum gradients of 6% 
# the supporting link/reference of using 6% are as follows
# https://cyclehighways.eu/design-and-build/design-principles/slopes-and-gradients.html
# Millet, G. P., Tronche, C., & Grappe, F. (2014). Accuracy of indirect estimation of power output from uphill performance in cycling. International Journal of Sports Physiology and Performance, 9(5), 777-782.

get_low_slope_ratio <- function(cyclestreets_routes){
  distance <- cyclestreets_routes$distances %>% str_split(",") %>% unlist() %>% as.numeric()
  elevation <- cyclestreets_routes$elevations %>% str_split(",") %>% unlist() %>% as.numeric()
  
  elv_diff_df <- data.frame(elv = elevation) %>%
    mutate(elv_lead = lead(elv,n=1)) %>%
    mutate(elv_diff = elv_lead - elv)
  
  slope_df <- elv_diff_df[1:(nrow(elv_diff_df)-1),] %>%
    cbind(distance) %>%
    mutate(slp = elv_diff/distance) 
  
  low_slope_ratio <- slope_df %>%
    mutate(low_slope = slp<= 0.06) %>%
    mutate(dist_ratio = prop.table(distance)) %>%
    group_by(low_slope) %>%
    dplyr::summarise(low_slope_ratio = sum(dist_ratio))
  
  low_slope_ratio = low_slope_ratio$low_slope_ratio[(low_slope_ratio$low_slope)=="TRUE"]
  
  return(low_slope_ratio)
    
}

start_time = Sys.time()
bikeability_vars_part1$low_slope_ratio =  apply(routes_cyclestreets,1,FUN = get_low_slope_ratio)
end_time = Sys.time()
end_time - start_time

##################################################
# Obtain the Number of Bearings (Left and Right)
##################################################

func_bearing<-function(lon_i,lat_i,lon_lead_i,lat_lead_i){
  p1<- c(lon_i,lat_i)
  p2<- c(lon_lead_i,lat_lead_i)
  bearing_value <- bearing(p1,p2)
  return(bearing_value) # bearing in degree
}

func_get_bearing_change <- function(cyclestreets_routes){
  coord_pairs <- cyclestreets_routes$coordinates %>% 
    str_replace_all(pattern = ",", replacement = " ") %>%
    str_split(pattern = " ") %>% unlist() %>% as.numeric() %>%
    matrix(ncol = 2, byrow=TRUE) %>%
    as.data.frame %>% setNames(c("lon","lat")) %>%
    mutate(lon_lead = lead(lon, n = 1),
           lat_lead = lead(lat, n = 1)) %>%
    rowwise() %>% 
    mutate(bearing = func_bearing(lat_i = lat, lon_i = lon, lat_lead_i = lat_lead,lon_lead_i = lon_lead)) %>%
    as.data.frame() %>%
    mutate(bearing = case_when( # change the degree from "-179 to 0" to "181-360", keep the "0-180" as it is.
      bearing< 0 ~ bearing+360,
      bearing> 0 ~ bearing
    )) %>%
    mutate(bearing_lead = dplyr::lead(bearing)) %>%
    mutate(bearing_change = case_when(
      (abs(bearing_lead-bearing) <=180) ~ (bearing_lead-bearing),
      (bearing_lead-bearing) >180 ~ (360-(bearing_lead-bearing)),
      (bearing_lead-bearing) < (-180) ~ (360+(bearing_lead-bearing))
    ))

  # The threshold will impact the sensitivity of identifying left/right bearings
  # we use absolute value of 55 to identify a bearing change
  # >=55 is right bearing, <-55 is left bearing
  num_left_bearing = coord_pairs %>% filter( (bearing_change <=-55)) %>% nrow() # get the number of left bearing
  num_right_bearing = coord_pairs %>% filter( (bearing_change >= 55)) %>% nrow() # get the number of right bearing

  return(c(num_left_bearing,num_right_bearing))
}

start_time = Sys.time()
bearing_change_num<-data.frame(apply(routes_cyclestreets,1,FUN = func_get_bearing_change) %>% t()) %>%
  rename(num_left_bearing = X1, num_right_bearing = X2)
end_time = Sys.time()
end_time-start_time

bikeability_vars_part1<-bikeability_vars_part1 %>% cbind(bearing_change_num)



# output the results (part 1)
# the remaining varibales are obtained in the "part 2" code, see "obtain_bikeability_vars_part2.R"
save(bikeability_vars_part1,file = "data/temp/bikeability_vars_part1.Rdata")
