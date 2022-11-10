##################################################################################
#            Obtain expected count data from London bike hire scheme             #
#                                                                                #
# This code takes the bike hire trips dataset where the bikes are routed over    #
# one of six bridges (Westminster, Lambeth, Southwark, London, Waterloo,         #
# Blackfriars) with the direction of travel. The midpoint of the hire period is  #
# used as the date/time for the 'crossing' of the bridge. Any bike hire lasting  #
# longer than an hour is dropped.                                                #
#                                                                                #
# It then identifies which of these trips occurred on dates and times where we   #
# have actual bike hire cycle counts from TFL cycle counters.  It computes the   #
# actual number of bike hire cycles (observed) and the estimated number of bike  #
# hire cycles from the trips dataset (expected) for each bridge, cycle direction #
# and survey date.  It generates a scatterplot and correlation cooefficient.     #
#                                                                                #
##################################################################################

# Load packages
library(sf)
library(tidyverse)
library(readxl)
library(lubridate)


##################################################################
# Load and examine the bike share trips that go over the bridges #
##################################################################

# Load datasets
load("data/trips_2018_new.Rdata")
crossbridge_od = read.csv("data/cross_bridge_od.csv")
stations_sf<- st_read('data/lchs-stations.geojson')

# Examine OD pairs that go over the bridges
dim(crossbridge_od) #114439 od pairs
names(crossbridge_od)
# [1] "X"                            "start_station_id"            
# [3] "end_station_id"               "route_id"                    
# [5] "bridge.name"                  "start_station_name"          
# [7] "start_station_location_river" "end_station_name"            
# [9] "end_station_location_river"
unique(crossbridge_od$bridge.name)
# [1] "Southwark Bridge"   "London Bridge"      "Waterloo Bridge"    "Lambeth Bridge"    
# [5] "Blackfriars Bridge" "Westminster Bridge"

crossbridge_od %>% group_by(bridge.name, start_station_location_river) %>%
  summarise(number_od_pairs_per_bridge_by_direction = n())
# bridge.name        start_station_location_river       number_od_pairs_per_bridge_by_direction
# 1 Blackfriars Bridge North                                             10262
# 2 Blackfriars Bridge South                                             10477
# 3 Lambeth Bridge     North                                              7660
# 4 Lambeth Bridge     South                                              7432
# 5 London Bridge      North                                              9360
# 6 Southwark Bridge   North                                             10156
# 7 Southwark Bridge   South                                              9616
# 8 Waterloo Bridge    North                                               730
# 9 Waterloo Bridge    South                                             12600
# 10 Westminster Bridge North                                             17085
# 11 Westminster Bridge South                                             19061


# Examine trips dataset
dim(trips_df) # n = 9675423
names(trips_df)
# [1] "start_time"       "stop_time"        "start_station_id" "end_station_id"  
# [5] "year_month" 

# Identify all Bike share trips that travel on routes that go over one of the 6 bridges
bike_share_trips_over_bridges = inner_join(trips_df, 
                                           crossbridge_od %>% 
                                             mutate(start_station_id = as.character(start_station_id),
                                                    end_station_id = as.character(end_station_id)),
                                           by = c("start_station_id", "end_station_id"))
dim(bike_share_trips_over_bridges) # n = 1073299

# # Validate this approach
# bike_share_trips_over_bridges %>% group_by(route_id) %>%
#   summarise(count = n()) %>%
#   arrange(desc(count)) %>%
#   head()
# # route_id count
# # 1   123187  4136
# # 2   123210  3964
# # 3   123814  3320
# # 4   123273  3284
# # 5   123661  2544
# # 6   298407  2304
# 
# crossbridge_od %>% filter(route_id == "123187") 
# # X start_station_id end_station_id route_id        bridge.name           start_station_name start_station_location_river            end_station_name end_station_location_river
# # 1 31200              154             48   123187    Waterloo Bridge Waterloo Station 3, Waterloo                        South Godliman Street, St. Paul's                      North
# # 2 61742              154             48   123187 Blackfriars Bridge Waterloo Station 3, Waterloo                        South Godliman Street, St. Paul's                      North
# # So this route goes over 2 bridges - this is important as potentially it 
# # gets counted on each bridge - depending on when the count is performed 
# # relativeto the journey 
# 
# validate_route_123187_trips = trips_df %>%
#   filter(start_station_id == 154 & end_station_id == 48) # n = 2068 observations
# 
# validate_route_123187_bike_share = bike_share_trips_over_bridges %>%
#   filter(start_station_id == 154 & end_station_id == 48) # n = 4136 observations


################################################################################
# Wrangle bike share trip data to get into format that can join to survey data #
################################################################################

# Westminster and Lambeth Bridges run E/W - the rest run N/S
# so in our bike_share_trips_over_bridges df where start station is North of the 
# and end station is SOuth of the river, it means the person is cycling West to East
# and vice versa. 

bike_share_trips_over_bridges_tidy = bike_share_trips_over_bridges %>%
  mutate(start_date = date(start_time),
         stop_date = date(stop_time), 
         duration_of_hire = difftime(stop_date, start_date, units = "hours"), # obtain duration of bike hire so can drop those that are held onto to long time
         mid_date_time = as.POSIXct((as.numeric(stop_time) + as.numeric(start_time)) / 2, origin = '1970-01-01'), # get midpoint of hire time to use as theoretical time for crossing the bridge
         mid_time = format(mid_date_time, format = "%H:%M"),
         start_hour = as.character(format(mid_date_time, format = "%H")), 
         start_min_precise = format(mid_date_time, format = "%M"), 
         start_min = case_when(start_min_precise < 15 ~ "00",
                               (start_min_precise >= 15 & start_min_precise <30) ~ "15",
                               (start_min_precise >= 30 & start_min_precise <45) ~ "30",
                               (start_min_precise >= 45 & start_min_precise <60) ~ "45"), # this is the 4 quarters in an hour which will match the survey data which is in 15 min time periods
         survey_time = paste0(start_hour, ":", start_min),
         survey_time_hm = hm(survey_time),
         direction = factor(
           case_when(
             ((bridge.name == "Lambeth Bridge") | (bridge.name == "Westminster Bridge")) & 
               ((start_station_location_river == "South") & (end_station_location_river == "North")) ~ "Westbound",
             ((bridge.name == "Lambeth Bridge") | (bridge.name == "Westminster Bridge")) & 
               ((start_station_location_river == "North") & (end_station_location_river == "South")) ~ "Eastbound",
             ((bridge.name != "Lambeth Bridge") | (bridge.name != "Westminster Bridge")) & 
               ((start_station_location_river == "South") & (end_station_location_river == "North")) ~ "Northbound",
             ((bridge.name != "Lambeth Bridge") | (bridge.name != "Westminster Bridge")) & 
               ((start_station_location_river == "North") & (end_station_location_river == "South")) ~ "Southbound"))) # Get direction of travel so can match to survey data

# Drop bike share trips that occur outside the TFL count period of 06:00 to 22:00 and
# those where the duration is longer than an hour
bike_share_trips_over_bridges_tidy = bike_share_trips_over_bridges_tidy %>% # 1073299
  filter(hour(mid_date_time) >= 06 & hour(mid_date_time) <= 21) %>% # n = 1015154 
  filter(duration_of_hire <= 1) %>%  #  n = 1014927
  select(-c(6, 19)) # drop unnecessary columns
 


################################################
# Get TFL traffic count data for the 6 Bridges #
################################################

# Load dataframes
count_locations = read_xlsx("/home/bananafan/Documents/PhD/Paper3/data/TFL_cycle_counters/Downloaded_25_08_22/Counter_locations_28_04_22.xlsx")

# Convert Easting and Northing to geometry column that can be plotted
counter_geo = count_locations %>%
  st_as_sf(coords = c("Easting", "Northing"), crs = 27700) %>%
  select(1:7)

#Identify counters located on the 6 Bridges of interest
mapview(counter_geo)
bridge_counters = c("CENCY003", "CENCY012", "CENCY016", "CENCY017", "CENCY030", "CENCY031")

# Load survey data
central_area_survey = read_xlsx("/home/bananafan/Documents/PhD/Paper3/data/TFL_cycle_counters/Downloaded_25_08_22/Central_London_area_28_04_22.xlsx")

# Get survey data from bridge counters
bridge_survey = central_area_survey %>%
  filter(`Site ID` %in% bridge_counters)
# n = 22272
# conducted 4x per year, in both directions for 64 time segments (06:00-22:00) using 6 counter locations
# in 2020 it was only conducted in Q1 and Q4 and in 2021 Q1 was missed

# Check for any survey date NAS
sum(is.na(bridge_survey$`Survey date`)) # 0 have no survey date
sum(is.na(bridge_survey$`Survey wave (calendar quarter)`)) # n = 0 so all are labelled with year and quarter

# Sort out survey year and quarter
bridge_survey_tidy = bridge_survey %>%
  separate(`Survey wave (calendar quarter)`, into = c("survey_year", "survey_quarter", "survey_months"), sep = "\\s", remove = FALSE) %>%
  select(-c(survey_months)) %>%
  mutate(counter_id = as_factor(`Site ID`),
         weather = as_factor(Weather),
         survey_date = ymd(`Survey date`),
         survey_year = as.numeric(survey_year),
         survey_quarter = as_factor(survey_quarter),
         survey_time_slot = as_factor(substr(Time, 1, 4)),
         survey_time_period = as_factor(Period),
         direction = as_factor(Direction), 
         survey_wave = as_factor(`Survey wave (calendar quarter)`),
         start_hour = str_sub(Time, 1, 2),
         start_min = str_sub(Time, 3, 4),
         survey_time = paste0(start_hour, ":", start_min)) %>% 
  select(-c(1, 4, 5:13)) %>%
  rename_with(~ tolower(gsub(" ", "_", .x, fixed = TRUE))) %>%
  rename(n_observed_cycle_hire_bikes = number_of_cycle_hire_bikes) %>%
  select(c(6, 1, 2, 12, 8:10, 13:15, 7, 11, 3:5)) 

# Limit survey data to 2018
bridge_survey_2018 = bridge_survey_tidy %>%
  filter(survey_year == 2018) # n = 3072 counting periods of 15 mins

# Get traffic counter on bridges 
bridge_counters = bridge_survey_2018 %>%
  filter(survey_quarter == "Q3" & survey_time == "06:00") %>%
  select(c(counter_id, direction)) %>%
  mutate(values = rep(1:2, 6)) %>%
  pivot_wider(names_from = values, names_prefix = "direction_", values_from = direction) %>%
  left_join(counter_geo, by = c("counter_id" = "UnqID")) %>%
  select(-c(4, 5, 8))
# saveRDS(bridge_survey_yuan, "bridge_survey_data.Rds")

# Add column to survey data with the Bridge names matching the TFL traffic counter
bridge_survey_2018 = left_join(bridge_survey_2018, bridge_counters %>% 
                                 st_drop_geometry%>%
                                 select(c(1, 4))) %>%
  rename(bridge_counter = Location) %>%
  select(c(16, 1:15))



##############################################################
#   Create dataframe of observed v expected bike hire counts #
##############################################################

# Minimise df
bridge_survey_variables = bridge_survey_2018 %>%
  select(c(1, 2, 3, 4, 6, 11, 13))

# Join bike share trips data to actual bridge survey data  
matched_bike_share_trips_survey_datetime = left_join(bridge_survey_variables, bike_share_trips_over_bridges_tidy,  
                 by = c("survey_date" = "start_date", 
                        "survey_time" = "survey_time", 
                        "direction" = "direction", 
                        "bridge_counter" = "bridge.name"))

# Get expected (based on the routing from bike hire OD) number of trips over each bridge by direction and survey date 
expected_trips_over_bridge_date = matched_bike_share_trips_survey_datetime %>% 
  group_by(bridge_counter, direction, survey_date) %>%
  summarise(n_expected_trips_over_bridge_on_that_survey_date = n())

# Get observed (actual survey data) number of trips over each bridge by direction and survey date 
observed_trips_over_bridge_date = bridge_survey_2018 %>%
  group_by(bridge_counter, direction, survey_date) %>%
  summarise(n_observed_cycle_hire_bikes_date = sum(n_observed_cycle_hire_bikes))

# Join these dataframes together to get obs-exp df
oe_date_only = left_join(expected_trips_over_bridge_date, 
                         observed_trips_over_bridge_date,
                         by = c("survey_date", "direction", "bridge_counter"))

saveRDS(oe_date_only, "data/obs_exp_bike_counts.Rds")

# Plot the data
cor = cor(oe_date_only$n_expected_trips_over_bridge_on_that_survey_date, oe_date_only$n_observed_cycle_hire_bikes_date)
# 0.7664561

ggplot(oe_date_only, 
       aes(x = n_observed_cycle_hire_bikes_date, 
           y = n_expected_trips_over_bridge_on_that_survey_date)) +
  geom_point(size = 0.5) +
  geom_smooth(method = lm) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  theme_classic() +
  xlab("Number of observed bike hire pedal cycles") +
  ylab("Number of expected bike hire pedal cycles based on routed bike hire trips") +
  annotate("text", label = "Cor = 0.77", x = 675, y = 250)

