# -------------------------------------------
#
# R script for calculating the connected bikeability index
# using data(variables) that are obtained from "obtain_bikeability_vars_part1.R" and "obtain_bikeability_vars_part1.R"
#
# -------------------------------------------
# L I B R A R I E S
# -------------------------------------------

# Required libraries.

# load the packages
# install.packages("devtools")
#devtools::install_github("PublicHealthDataGeek/CycleInfraLnd",force = T)
library(tidyverse)
library(units)
library(sf)
library(tmap)
library(geosphere)
library(caret)
library(MetBrewer)
library(RColorBrewer)
library(reshape2)


################################################################################
# Load data and preparing variables
################################################################################


# load data of connected bikeability variables
# the variables are generated with different approaches, therefore they are stored in different files 
load("data/temp/bikeability_vars_part1.Rdata")
load("data/temp/bikeability_vars_part2.Rdata")
load("data/temp/bikeability_vars_road_speed.Rdata")

# replace NA with 0 - e.g. number of junctions along the routes
bikeability_vars_part2[is.na(bikeability_vars_part2)]=0

# join all variables (raw) to an single dataframe
bikeability_vars_raw <- cbind(bikeability_vars_part1,bikeability_vars_part2) %>% 
  left_join(cyclestreets_routes_length_on_roads_speed, 
            by =c("start_station_id"="start_station_id","end_station_id"="end_station_id")) %>%
  mutate(dist_roads_high_speed=drop_units(dist_roads_high_speed)) %>%
  mutate(dist_roads_high_speed = replace_na(dist_roads_high_speed,0)) %>% 
  filter(distance*directness>=500) 
# filter out trips that are shorter than 500m in crow-fly distance
# Analysis by TfL identifies a minimum straight-line distance of potentially cyclable trips as c.500m (TfL 2010). 
# Given the high variability of values for the short routed trips, 
# we used this threshold for excluding all trips whose straight-line distances were < 500m.


# average speed of travelling by bike - 203.5588 m/min
# this value is derived from london cycle hire scheme trip records
ave_speed_min = 203.5588

# for trips that are more than 30 minutes, a distance penalty (coefficient) is applied
# its value will drop (linearly) from 1 until 0, depending on the route distance
bikeability_vars_raw<- bikeability_vars_raw %>%
  mutate(distance_coef = case_when(
    distance<ave_speed_min*30 ~ 1,
    distance>=ave_speed_min*30 ~ 1-((distance -  ave_speed_min*30) / (max(bikeability_vars_raw$distance) - ave_speed_min*30))
  ))

# visualise the relationship between distance penalty and route distance, using randomly sampled 2,000 routes
bikeability_vars_raw %>% sample_n(2000) %>%
  ggplot()+
  geom_point(aes(x=distance,y = distance_coef))

# import the (1) london cycle hire scheme docking station data, and (2) bike village boundaries
lchs_station_sf<-st_read("data/lchs/lchs-docking-stations.geojson")
lchs_station_sf <- lchs_station_sf %>% st_transform(crs = 27700)
lchs_station_village<-st_read("data/villages_agg_extent.geojson")

# visualise the docking stations and bike villages on maps
tmap_mode("view")
tm_shape(lchs_station_village)+
  tm_polygons()+
  tm_shape(lchs_station_sf)+
  tm_dots()

# applied spatial intersection to identify which bikeshare village each docking station falls in
lchs_station_sf <- lchs_station_sf %>% st_intersection(lchs_station_village) %>%
  dplyr::select(station_id,station_name,name_agg) %>%
  rename(bv_name = name_agg)


# filter out routes whose od is in the same bikeshare villages
bikeability_vars_raw <- bikeability_vars_raw %>% 
  left_join(lchs_station_sf %>% st_drop_geometry() %>% dplyr::select(station_id,bv_name), by = c("start_station_id" = "station_id")) %>%
  rename(start_bv_name = bv_name) %>%
  left_join(lchs_station_sf %>% st_drop_geometry() %>% dplyr::select(station_id,bv_name), by = c("end_station_id" = "station_id")) %>%
  rename(end_bv_name = bv_name) %>%
  filter(start_bv_name!=end_bv_name) %>%
  dplyr::select(-start_bv_name,-end_bv_name)




################################################################################
# Check statistical distribution of raw variables
# Aplly suitable transformation functions
################################################################################

# Domain #1. Comfort

domain_comfort <- bikeability_vars_raw %>% 
  mutate(rate_r_bearing = num_right_bearing/distance,
         rate_junction = num_j/distance,
         ratio_low_speed_road = (1-dist_roads_high_speed/distance)
  ) %>%
  dplyr::select(start_station_id, end_station_id,
                rate_r_bearing, rate_junction, 
                ratio_low_speed_road
  )


# Domain #2. Safety
domain_safety<- bikeability_vars_raw %>% 
  mutate(dist_fullseg = drop_units(dist_fullseg),
         dist_stepped_part_seg = drop_units(dist_stepped_part_seg),
         dist_mand_adv = drop_units(dist_mand_adv),
         dist_off_road = drop_units(dist_off_road),
  ) %>%
  mutate(ratio_on_clt_road_full_seg = dist_fullseg/distance,
         ratio_on_clt_stepped_part_seg = dist_stepped_part_seg/distance,
         ratio_on_clt_mand_adv = dist_mand_adv/distance,
         ratio_off_clt = dist_off_road/distance,
         safe_j_ratio = num_sj/num_j
  ) %>%
  dplyr::select(start_station_id,end_station_id,
                ratio_on_clt_road_full_seg, # higher value is better, weight 100%
                ratio_on_clt_stepped_part_seg, # higher value is better, weight 80%
                ratio_on_clt_mand_adv, # higher value is better, weight 75%
                ratio_off_clt, # higher value is better, weight 100%
                safe_j_ratio
  )

# replace NA with 1. If there is no junction along the route, 
# then the ratio junctions with safety-supported is calculated as 0/0 and will lead to NA values
# in this case, NA should be replaced with 1 (favorable), indicating no junctions (safety-supported or not), 
# this is considered as safe (favorable). 
domain_safety$safe_j_ratio[is.na(domain_safety$safe_j_ratio)]=1



# Domain #3. Attractiveness
domain_attractiveness <- bikeability_vars_raw %>% 
  mutate(dist_park_river =  drop_units(dist_park_river)) %>%
  mutate(rate_cycle_parking = num_cp/distance,
         ratio_on_clt_park_water = dist_park_river/distance
  ) %>%
  dplyr::select(start_station_id,end_station_id,
                rate_cycle_parking, 
                ratio_on_clt_park_water, 
                distance_coef
  ) 



# Domain #4 Coherence
domain_coherence <- bikeability_vars_raw %>% 
  mutate(rate_signage_navi = num_sign_navi/distance) %>%
  dplyr::select(start_station_id,end_station_id,
                directness, 
                rate_signage_navi
                
  )

colnames(domain_comfort)
colnames(domain_safety)
colnames(domain_attractiveness)
colnames(domain_coherence)



## data normalisation and transformation functions
# range scale [0,1]
range01 <- function(x){(x-min(x))/(max(x)-min(x))}

# Box-Cox Transformation (follwed by range scale)
get_trans_boxcox<-function(df){
  input_data_boxcox_scale=df
  # BoxCox Transformation requires input values to be positive
  # add an constant (absolute value of the min value) to make the variables/features only contains positive values
  input_data_boxcox_scale[3:ncol(input_data_boxcox_scale)] <-input_data_boxcox_scale[3:ncol(input_data_boxcox_scale)] %>%
    mutate_all(function(x) x+0.0001) %>%
    mutate_all(funs( BoxCoxTrans(.) %>% predict(.))) %>% # boxcox transformation
    mutate_all(funs(range01(.))) # range scale
  return(input_data_boxcox_scale)
}



# address the bias - introducing distance-adjusted rates
# Domain Comfort
domain_comfort <- domain_comfort %>%
  left_join(bikeability_vars_part1 %>% dplyr::select(start_station_id, end_station_id, distance ))
model_bearing = lm(formula = (rate_r_bearing) ~ distance, data = domain_comfort )
model_junction = lm(formula = (rate_junction) ~ distance, data = domain_comfort )
model_low_speed_road = lm(formula = (ratio_low_speed_road) ~ distance, data = domain_comfort )

domain_comfort_balance <- domain_comfort
domain_comfort_balance$pred_rate_r_bearing <- predict(model_bearing,domain_comfort)
domain_comfort_balance$pred_rate_junction <- predict(model_junction,domain_comfort)
domain_comfort_balance$pred_ratio_low_speed_road <- predict(model_low_speed_road, domain_comfort)

domain_comfort_balance<- domain_comfort_balance %>%
  mutate(pred_rate_r_bearing_diff = (rate_r_bearing - pred_rate_r_bearing)/pred_rate_r_bearing) %>%
  mutate(pred_rate_junction_diff = (rate_junction - pred_rate_junction)/pred_rate_junction) %>%
  mutate(pred_ratio_low_speed_road_diff = (ratio_low_speed_road - pred_ratio_low_speed_road)/pred_ratio_low_speed_road)


# replacing variables with distance-adjusted variables/rates
domain_comfort<- domain_comfort_balance %>%
  dplyr::select(start_station_id, end_station_id, 
                pred_rate_r_bearing_diff, pred_rate_junction_diff, pred_ratio_low_speed_road_diff, distance) %>%
  rename(
         "rate_r_bearing" = pred_rate_r_bearing_diff,
         "rate_junction" = pred_rate_junction_diff,
         "ratio_low_speed_road" = pred_ratio_low_speed_road_diff
         )

# Cap variables in comfort domain - addressing outliers issue
domain_comfort <- domain_comfort %>%
  mutate(
    rate_r_bearing = case_when(
      rate_r_bearing >=2 ~2,
      rate_r_bearing <2 ~rate_r_bearing
    ),
    rate_junction = case_when(
      rate_junction >=0.75 ~ 0.75,
      rate_junction <0.75 ~rate_junction
    ),
    ratio_low_speed_road = case_when(
      ratio_low_speed_road<=-0.7 ~ -0.7,
      ratio_low_speed_road> -0.7 ~ ratio_low_speed_road
    )
  )

# visualise distribution of variable in comfort domain
ggplot(data = domain_comfort %>% 
         dplyr::select(-start_station_id,-end_station_id) %>%
         melt(),aes(x = value)) + 
  facet_wrap(~variable,scales = "free") + 
  geom_histogram()



# address the bias - introducing distance-adjusted rates
# Domain Safety

domain_safety <- domain_safety %>%
  left_join(bikeability_vars_part1 %>% dplyr::select(start_station_id, end_station_id, distance ))

model_full_seg = lm(formula = (ratio_on_clt_road_full_seg) ~ distance, data = domain_safety)
model_stepped_part_seg = lm(formula = (ratio_on_clt_stepped_part_seg) ~ distance, data = domain_safety )
model_clt_mand_adv = lm(formula = (ratio_on_clt_mand_adv) ~ distance, data = domain_safety )
model_off_clt = lm(formula = (ratio_off_clt) ~ distance, data = domain_safety )
model_safe_j = lm(formula = (safe_j_ratio) ~ distance, data = domain_safety )



domain_safety_balance <- domain_safety
domain_safety_balance$pred_ratio_on_clt_road_full_seg <- predict(model_full_seg,domain_safety)
domain_safety_balance$pred_ratio_on_clt_stepped_part_seg <- predict(model_stepped_part_seg,domain_safety)
domain_safety_balance$pred_ratio_on_clt_mand_adv <- predict(model_clt_mand_adv,domain_safety)
domain_safety_balance$pred_ratio_off_clt <- predict(model_off_clt,domain_safety)
domain_safety_balance$pred_safe_j_ratio <- predict(model_safe_j,domain_safety)


domain_safety_balance<- domain_safety_balance %>%
  mutate(pred_ratio_on_clt_road_full_seg_diff = (ratio_on_clt_road_full_seg-pred_ratio_on_clt_road_full_seg)/pred_ratio_on_clt_road_full_seg) %>%
  mutate(pred_ratio_on_clt_stepped_part_seg_diff = (ratio_on_clt_stepped_part_seg - pred_ratio_on_clt_stepped_part_seg)/pred_ratio_on_clt_stepped_part_seg) %>%
  mutate(pred_ratio_on_clt_mand_adv_diff = (ratio_on_clt_mand_adv - pred_ratio_on_clt_mand_adv)/pred_ratio_on_clt_mand_adv) %>%
  mutate(pred_ratio_off_clt_diff = (ratio_off_clt- pred_ratio_off_clt)/pred_ratio_off_clt) %>%
  mutate(pred_safe_j_ratio_diff = (safe_j_ratio - pred_safe_j_ratio)/pred_safe_j_ratio) 

# replacing variables with distance-adjusted variables/rates
domain_safety <- domain_safety_balance %>%
  dplyr::select(start_station_id, end_station_id, 
                pred_ratio_on_clt_road_full_seg_diff, 
                pred_ratio_on_clt_stepped_part_seg_diff, 
                pred_ratio_on_clt_mand_adv_diff, 
                pred_ratio_off_clt_diff, 
                pred_safe_j_ratio_diff,
                distance) %>%
  rename("ratio_on_clt_road_full_seg" = pred_ratio_on_clt_road_full_seg_diff,
         "ratio_on_clt_stepped_part_seg" = pred_ratio_on_clt_stepped_part_seg_diff,
         "ratio_on_clt_mand_adv" = pred_ratio_on_clt_mand_adv_diff,
         "ratio_off_clt" = pred_ratio_off_clt_diff,
         "safe_j_ratio"  = pred_safe_j_ratio_diff
  )


# Cap variables in safety domain - addressing outlier issue
domain_safety<-domain_safety %>%
  mutate(
    ratio_on_clt_road_full_seg = case_when(
      ratio_on_clt_road_full_seg>= 8 ~ 8,
      ratio_on_clt_road_full_seg<=8 ~ ratio_on_clt_road_full_seg),
    ratio_on_clt_stepped_part_seg = case_when(
      ratio_on_clt_stepped_part_seg >= 10 ~ 10,
      ratio_on_clt_stepped_part_seg < 10 ~ ratio_on_clt_stepped_part_seg),
    ratio_on_clt_mand_adv = case_when(
      ratio_on_clt_mand_adv >= 4 ~ 4,
      ratio_on_clt_mand_adv< 4 ~ratio_on_clt_mand_adv),
    ratio_off_clt = case_when(
      ratio_off_clt >= 6 ~ 6,
      ratio_off_clt< 6~ratio_off_clt),
    safe_j_ratio = case_when(
      safe_j_ratio >= 3 ~ 3,
      safe_j_ratio < 3 ~safe_j_ratio
    )
    
  )

# visualise distribution of variable in safety domain
ggplot(data = domain_safety %>% 
         dplyr::select(-start_station_id,-end_station_id) %>%
         melt(),aes(x = value)) + 
  facet_wrap(~variable,scales = "free") + 
  geom_histogram()


# address the bias - introducing distance-adjusted rates
# Domain Attractiveness

domain_attractiveness <- domain_attractiveness %>%
  left_join(bikeability_vars_part1 %>% dplyr::select(start_station_id, end_station_id, distance ))

model_cycle_parking = lm(formula = (rate_cycle_parking) ~ distance, data = domain_attractiveness)
model_clt_park_water = lm(formula = (ratio_on_clt_park_water) ~ distance, data = domain_attractiveness )

domain_attractiveness_balance <- domain_attractiveness
domain_attractiveness_balance$pred_rate_cycle_parking <- predict(model_cycle_parking,domain_attractiveness)
domain_attractiveness_balance$pred_ratio_on_clt_park_water  <- predict(model_clt_park_water,domain_attractiveness)


domain_attractiveness_balance<- domain_attractiveness_balance %>%
  mutate(pred_rate_cycle_parking_diff = (rate_cycle_parking - pred_rate_cycle_parking)/pred_rate_cycle_parking) %>%
  mutate(pred_ratio_on_clt_park_water_diff = (ratio_on_clt_park_water - pred_ratio_on_clt_park_water)/pred_ratio_on_clt_park_water) #%>%

# replacing variables with distance-adjusted variables/rates
# note that distance_coef does not need further distance-adjusted, therefore the original values are used
domain_attractiveness <- domain_attractiveness_balance %>%
  dplyr::select(start_station_id, end_station_id, 
                pred_rate_cycle_parking_diff, pred_ratio_on_clt_park_water_diff,
                distance_coef,
                distance
  ) %>%
  rename(
    "rate_cycle_parking"= pred_rate_cycle_parking_diff,
    "ratio_on_clt_park_water" = pred_ratio_on_clt_park_water_diff#,
  )


# Cap variables in attractiveness domain - addressing outlier issue
domain_attractiveness <- domain_attractiveness %>% 
  mutate(rate_cycle_parking = case_when(
    rate_cycle_parking >= 3 ~ 3,
    rate_cycle_parking < 3 ~rate_cycle_parking
  ),
  ratio_on_clt_park_water = case_when(
    ratio_on_clt_park_water >= 8 ~ 8,
    ratio_on_clt_park_water < 8 ~ratio_on_clt_park_water
  )
  )

# visualise distribution of variable in attractiveness domain
ggplot(data = domain_attractiveness %>% 
         dplyr::select(-start_station_id,-end_station_id) %>%
         melt(),aes(x = value)) + 
  facet_wrap(~variable,scales = "free") + 
  geom_histogram()



# address the bias - introducing distance-adjusted rates
# Domain Coherence

domain_coherence <- domain_coherence %>%
  left_join(bikeability_vars_part1 %>% dplyr::select(start_station_id, end_station_id, distance ))

model_directness = lm(formula = (directness) ~ distance, data = domain_coherence)
model_signage_navi = lm(formula = (rate_signage_navi) ~ distance, data = domain_coherence )

domain_coherence_balance <- domain_coherence
domain_coherence_balance$pred_directness <- predict(model_directness,domain_coherence)
domain_coherence_balance$pred_rate_signage_navi  <- predict(model_signage_navi,domain_coherence)

domain_coherence_balance<- domain_coherence_balance %>%
  mutate(pred_directness_diff = (directness - pred_directness)/pred_directness) %>%
  mutate(pred_rate_signage_navi_diff = (rate_signage_navi - pred_rate_signage_navi)/pred_rate_signage_navi)

# replacing variables with distance-adjusted variables/rates
domain_coherence <- domain_coherence_balance %>%
  dplyr::select(start_station_id, end_station_id, 
                pred_directness_diff, pred_rate_signage_navi_diff, 
                distance
                ) %>%
  rename(
    "directness"=pred_directness_diff,
    "rate_signage_navi" = pred_rate_signage_navi_diff
  )

# Cap variables in coherence domain - addressing outlier issue
domain_coherence <- domain_coherence %>% 
  mutate(directness = case_when(
    directness<= -0.35 ~ -0.35,
    directness> -0.35 ~ directness,
  ),
  rate_signage_navi = case_when(
    rate_signage_navi >= 5.5 ~5.5,
    rate_signage_navi <5.5 ~ rate_signage_navi
  )
  )

# visualise distribution of variable in coherence domain
ggplot(data = domain_coherence %>% 
         dplyr::select(-start_station_id,-end_station_id) %>%
         melt(),aes(x = value)) + 
  facet_wrap(~variable,scales = "free") + 
  geom_histogram()




################################################################################
# Assemble the variables, create a summary score for each domain
################################################################################
# Each variable will have the same weight in the respondnet domain, except the Cycle Lane Track ones with different level of segregation
# their weights are suggested by https://www.cyclestreets.net/help/journey/howitworks/

summary(domain_comfort)
# offset the value - making all positive 
domain_comfort$rate_r_bearing <- domain_comfort$rate_r_bearing + abs(min(domain_comfort$rate_r_bearing))
domain_comfort$rate_junction <- domain_comfort$rate_junction + abs(min(domain_comfort$rate_junction))
domain_comfort$ratio_low_speed_road <- domain_comfort$ratio_low_speed_road + abs(min(domain_comfort$ratio_low_speed_road))

domain_comfort_trans_boxcox <- domain_comfort %>% get_trans_boxcox()



# offset the value - making all positive 
summary(domain_safety)
domain_safety$ratio_on_clt_road_full_seg <- domain_safety$ratio_on_clt_road_full_seg +abs(min(domain_safety$ratio_on_clt_road_full_seg))
domain_safety$ratio_on_clt_stepped_part_seg <- domain_safety$ratio_on_clt_stepped_part_seg + abs(min(domain_safety$ratio_on_clt_stepped_part_seg))
domain_safety$ratio_on_clt_mand_adv <- domain_safety$ratio_on_clt_mand_adv+ abs(min(domain_safety$ratio_on_clt_mand_adv))
domain_safety$ratio_off_clt <- domain_safety$ratio_off_clt + abs(min(domain_safety$ratio_off_clt))
domain_safety$safe_j_ratio <- domain_safety$safe_j_ratio + abs(min(domain_safety$safe_j_ratio))

domain_safety_trans_boxcox <- domain_safety %>% get_trans_boxcox()



# offset the value - making all positive 
summary(domain_attractiveness)
domain_attractiveness$rate_cycle_parking <- domain_attractiveness$rate_cycle_parking + abs(min(domain_attractiveness$rate_cycle_parking))
domain_attractiveness$ratio_on_clt_park_water <- domain_attractiveness$ratio_on_clt_park_water + abs(min(domain_attractiveness$ratio_on_clt_park_water))

domain_attractiveness_trans_boxcox <- domain_attractiveness %>% get_trans_boxcox()




# offset the value - making all positive 
summary(domain_coherence)
domain_coherence$directness  <- domain_coherence$directness  + abs(min(domain_coherence$directness ))
domain_coherence$rate_signage_navi <- domain_coherence$rate_signage_navi + abs(min(domain_coherence$rate_signage_navi))

domain_coherence_trans_boxcox <- domain_coherence %>% get_trans_boxcox()


# Assemble domain values

score_domain_comfort <- domain_comfort_trans_boxcox %>%
  mutate(score_comfort =
           (1-rate_r_bearing) + # higher bearing rate is unfavorable, therefore 1 - value is used
           (1-rate_junction) + # higher junction rate is unfavorable, therefore 1 - value is used
           ratio_low_speed_road # higher low speed road ratio is favorable, therefore value (*1) is used
  ) 


score_domain_safety <- domain_safety_trans_boxcox %>%
  mutate(score_safety = ratio_on_clt_road_full_seg*1+
           ratio_on_clt_stepped_part_seg*0.7+
           ratio_on_clt_mand_adv*0.4+ratio_off_clt*1+safe_j_ratio*1
  ) 

score_domain_attractiveness <- domain_attractiveness_trans_boxcox %>%
  mutate(score_attractiveness = ratio_on_clt_park_water+rate_cycle_parking+distance_coef
  ) 

score_domain_coherence <- domain_coherence_trans_boxcox %>%
  mutate(score_coherence = directness+rate_signage_navi) 


# join all domain values - macthing route of same origin and destination docking stations
connected_bikeability_index <-
  score_domain_comfort %>% dplyr::select(start_station_id, end_station_id, score_comfort) %>%
  left_join(score_domain_safety %>% dplyr::select(start_station_id, end_station_id, score_safety),
            by = c("start_station_id"="start_station_id", "end_station_id" ="end_station_id")) %>%
  left_join(score_domain_attractiveness %>% dplyr::select(start_station_id, end_station_id, score_attractiveness),
            by = c("start_station_id"="start_station_id", "end_station_id" ="end_station_id")) %>%
  left_join(score_domain_coherence %>% dplyr::select(start_station_id, end_station_id, score_coherence),
            by = c("start_station_id"="start_station_id", "end_station_id" ="end_station_id")) 

ggplot(data = connected_bikeability_index %>% 
         dplyr::select(-start_station_id,-end_station_id) %>%
         melt(),aes(x = value)) + 
  facet_wrap(~variable,scales = "free") + 
  geom_histogram(fill = "#336699")+
  theme_minimal()+
  ggtitle("scores")

# Cap values - adjust distribution to solve outliers
connected_bikeability_index <- connected_bikeability_index %>% 
  mutate(
    score_comfort = case_when(
      score_comfort<=0.5 ~ 0.5,
      (score_comfort>0.5)&(score_comfort<2.8) ~ score_comfort,
      score_comfort >=2.8 ~ 2.8
    ),

    score_safety = case_when(
      score_safety >=3.6 ~ 3.6,
      score_safety <3.6 ~ score_safety
    ),
    score_attractiveness = case_when(
      score_attractiveness >= 2.7 ~ 2.7,
      score_attractiveness < 2.7 ~ score_attractiveness
    ),
    score_coherence = case_when(
      score_coherence >=2.7 ~ 2.7,
      (score_coherence<2.7)&(score_coherence>0.1) ~ score_coherence,
      score_coherence <=0.1 ~0.1
    )
  ) %>%
  get_trans_boxcox() %>%
  mutate(cb_index = score_comfort*1 + score_safety*1 + score_attractiveness*1 + score_coherence*1)



# Visualise domain scores
ggplot(data = connected_bikeability_index %>% 
         dplyr::select(-start_station_id,-end_station_id) %>%
         melt(),aes(x = value)) + 
  facet_wrap(~variable,scales = "free") + 
  geom_histogram(fill = "#336699")+
  theme_minimal()+
  ggtitle("all scores")


ggplot(data = connected_bikeability_index %>% 
         dplyr::select(-start_station_id,-end_station_id, -cb_index ) %>%
         melt(),aes(x = value)) + 
  facet_wrap(~variable,scales = "free") + 
  geom_histogram(fill = "#336699")+
  theme_minimal()+
  ggtitle("domain scores")


ggplot(data = connected_bikeability_index , aes(x = cb_index)) + 
  geom_histogram(fill = "#336699")+
  theme_minimal()+
  ggtitle("connected bikeability scores")


# transform and normalise scores - the scores will range from 0 to 1, higher values indicate better connected bikeability (domain or overall)
connected_bikeability_index <- connected_bikeability_index %>% get_trans_boxcox()

# join route distance information
connected_bikeability_index<- connected_bikeability_index %>%
  left_join(bikeability_vars_raw %>% dplyr::select(start_station_id,end_station_id,distance))

head(connected_bikeability_index)

connected_bikeability_index %>% ggplot()+
  geom_histogram(aes(x=cb_index), fill = "#336699")+
  theme_minimal()+
  ggtitle("connected bikeability")

# output result, the result is presented as docking station o-d pair level rather than aggregated bikeshare village level.
write.csv(connected_bikeability_index, "data/connected_bikeability_index_od_level.csv")


