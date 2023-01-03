Validate bikeability scores
================

## Introduction

This document explores how OD-level bikeability scores vary with
observed trips made in the bikeshare scheme. Such an approach has been
used previously to *validate* bikeability scores under the assumption
that trips are more likely to be made in parts of a city that are more
amenable to cycling.

This assumption is difficult to analyse empirically without conditioning
on some fairly substantial confounding context. First, the amount of
cycling between OD locations in the LCHS is heavily conflated with
demand – where work and other activities are concentrated in London.
Second, heavy competition for bikes and docking stations leads to
service pressure, making certain parts of the scheme more viable than
others. Third, related to demand, LCHS trip patterns are dominated by
two distinct functions that are difficult to adjust for: a leisure-type
function characterised by trips coinciding with London’s parks and
tourist attractions and a commuter-type function where so-called
‘last-mile’ trips connect major rail terminals and workplace centres.
Finally, bikeshare schemes incentivise short trips, both in their
physical design and pricing regimes; journeys connecting more remote OD
village pairs therefore become quite impractical. For our model to make
sense, it is necessary to account for at least some of this confounding
context.

We set up a regression model exploring how morning trip counts in the
LCHS (outcome) varies with bikeability, net of *demand* (derived from
Census 2011 travel-to-work data), *service pressure* (estimated from
bikeshare station occupancy data), whether an OD pair involves a key
*hub* docking station and also a random intercept term on *destination*
village to reflect the fact that the pattern of trip frequencies will
also vary systematically on the destination village (‘workplace’).

Later in the document we use OD maps ([Wood et
al. 2010](https://www.tandfonline.com/doi/abs/10.1179/000870410X12658023467367))
to validate and explore patterns of bikeability visually and in the
context of London’s dedicated cycling infrastructure.

Please cite:

Beecham, R., Yang, Y., Tait, C. and Lovelace, R. (2023) “Connected
bikeability in London: which localities are better connected by bike and
does this matter?”, *Environment & Planning B: Urban Analytics and City
Science*. DOI: [osf.io/gbfz8](https://osf.io/gbfz8).

## Setup

### Required libraries

Required packages can be installed individually with
`install.packages(<package_name>)`. Core packages are imported into the
session with `library(<package_name>)`. Occasional use of packages is
made with the `<package-name>::<function-name>()` syntax to avoid
polluting the workspace.

``` r
pkgs <- c("tidyverse","sf", "here")
# If not already installed.
# install.packages(pkgs)
# Core packages
library(here)
library(patchwork)
library(tidyverse)
library(lubridate)

library(sf)
library(geosphere)

library(tidymodels)
install.packages("multilevelmod")
library(multilevelmod)
install.packages("performance")

library(distributional)
library(ggdist)

# ggplot theme for paper
source(here("code","theme_paper.R"))
source(here("code","plot_helpers.R"))
```

## Load data

Load in LCHS trips data (docking station OD pairs) commute trips
(between MSOAs), the village geographies and the village-village
bikeability scores (with zoning adjustment).

``` r
# Read in villages.
villages <- st_read(here("data", "grid_real_sf.geojson"))
villages_real <- villages |> filter(type=="real") |> mutate(row_id=row_number())

# Read in OD commute data : msoa-msoa
commute_data  <- read_csv(here("data", "wicid_output_occupation.csv")) |>
  select(origin_msoa, destination_msoa, all)
# MSOA boundary data (from https://geoportal.statistics.gov.uk/s)
# Simplified using rmapshaper
msoa_boundaries <- st_read(here("data", "msoa_boundaries.geojson"))
# Filter on all MSOAs within London.
region_boundaries <- st_read(here("data", "regions.geojson")) %>%
  filter(RGN20NM=="London")
temp <- msoa_boundaries %>% st_filter(region_boundaries %>% select(RGN20NM))
# Not very simplified as City of London so small.
temp <- temp %>% rmapshaper::ms_simplify(keep=.1)
# Cast to OSGB.
temp <- temp %>% st_transform(crs=27700)

# Read in bikeshare trip data : docking-station>docking-station
bs_data <- fst::read_fst(here("data", "trips_2018.fst"))
# Load stations data.
bike_stations <-  st_read(here("data", "bike_stations.geojson")) |> st_transform(crs=27700)
# Join on villages.
bike_stations  <- bike_stations |> st_join(villages %>% filter(type=="real") %>%  select(village=name), .predicate=st_intersects())
# Stations coords
bike_stations_coords <- bike_stations |> st_centroid() |> st_coordinates() |> as_tibble() |> rename_all(tolower)
bike_stations <- bind_cols(bike_stations, bike_stations_coords)

# Load stations data.
service_pressure <- read_csv(here("data", "service_pressure.csv"))

# MAUP-adjusted bikeability.
bikeability_village <- read_csv(here("data", "simulated_data.csv")) |>
  filter(!is.na(d_village), o_village!=d_village) |>
  group_by(o_village, d_village) |>
  summarise(index=mean(index)) |> ungroup()
```

## Assign 2011 Census commutes to villages

The two geographies, MSOAs and villages, intersect in many different
ways and cannot be easily reconciled. Our solution is to generate
individual records for each commute – to detach commutes from their
aggregated MSOA-MSOA geography. For each observation (commuter record)
we generate estimated point locations by random spatial sampling within
the polygon area of that commute’s origin and destination MSOA. These
estimated point locations are then used to assign commute origin and
destination locations to the bikeshare villages in which they are
contained.

We define a function for the random spatial sampling, using
[`st_sample`](https://r-spatial.github.io/sf/reference/st_sample.html).

``` r
#' Random spatial sample within polygon.
#'
#'
#' @param geo An sf MULTIPOLYGON object.
#' @param n Desired sample size.
#'
#' @return A tidy data frame (tibble) with ST_POINT geometry defining point locations.
#'
#' @export
geo_sample <- function(geo, n) {
  return(
    sf::st_sample(x=geo, size=n) %>%
    st_coordinates()
  )
}
```

We then generate a large set ($n=2000$) of sampled points for each MSOA,
held in `sampled_msoas`.

``` r
# Buffer around bikeshare scheme villages.
temp_buffer <- villages %>% filter(type=="real") %>%
  st_buffer(dist=0, .predictate=st_intersects) %>%
  summarise()
temp_filtered <- temp  %>% st_filter(temp_buffer, .predictate=st_intersects())

# For quick searching, generate sampled point locations for each MSOA.
# Resample from these locations to then generate origin and destination points
# for each commute.
sampled_msoas <- temp_filtered %>% select(msoa=MSOA11CD) %>%
  nest(data=-c(msoa)) %>%
  mutate(
    sampled_points=map(data,~geo_sample(geo=.x,n=2000) %>%
                     as_tibble(.name_repair=~c("east", "north"))
    )
  ) %>%
  unnest(-data) %>%
  select(-data)

# Filter all Census msoas whose destinations are within the buffer.
commute_data_filtered <- commute_data %>%
    rowwise() %>%
    mutate(all=sum(all)) %>%
    ungroup() %>%
    select(origin_msoa, destination_msoa, all) %>%
    filter(
        origin_msoa %in% (temp_filtered %>% pull(MSOA11CD)) &
        destination_msoa %in% (temp_filtered %>% pull(MSOA11CD))
      )

commute_data_filtered <- commute_data %>%
   filter(
        destination_msoa %in% (temp_filtered %>% pull(MSOA11CD))
    ) |>
    group_by(destination_msoa) |>
    summarise(all=sum(all))
```

For each commute we then search in `sampled_msoas` to attach point
locations. This is achieved by taking each MSOA-MSOA OD pair and
sampling MSOA point locations by destination according to the commute
count of that OD pair.

``` r
# Generate points.
commute_points <- commute_data_filtered %>% mutate(od_pair=paste0(origin_msoa,"-",destination_msoa)) %>%
  filter(all>0) %>%
  nest(data=-c(od_pair)) %>%
  mutate(
    o_count=map(
      data,
      ~sample_n(
        sampled_msoas %>% filter(msoa==.x %>% pull(origin_msoa)),
        size=.x %>% pull(all)
        ) %>%
      as_tibble(.name_repair=~c("o_msoa","o_east", "o_north"))
    ),
   d_count=map(
     data,
     ~sample_n(
       sampled_msoas %>% filter(msoa==.x %>% pull(destination_msoa)),
       size=.x %>% pull(all)
       ) %>%
      as_tibble(.name_repair=~c("d_msoa","d_east", "d_north"))
   ),
  ) %>%
  unnest(-data) %>%
  select(-data)


commute_points <- commute_data_filtered %>% mutate(all=round(all), msoa=destination_msoa) |>
  nest(data=-msoa) |>
   mutate(d_count=map(
     data,
     ~sample_n(
       sampled_msoas %>% filter(msoa==.x %>% pull(destination_msoa)),
       size=.x %>% pull(all), replace=TRUE
       ) %>%
      as_tibble(.name_repair=~c("d_msoa","d_east", "d_north"))
     ),
  ) %>%
  unnest(-data) %>%
  select(-data)
```

Finally, commutes are assigned to the bikeshare village in which they
are contained using
[`st_join`](https://r-spatial.github.io/sf/reference/st_join.html), and
we then summarise over village-village OD pairs.

``` r
commute_villages  <- commute_points %>% select(-c(od_pair,o_msoa, d_msoa)) %>%
  st_as_sf(coords=c("o_east", "o_north"), crs=27700) %>%
  st_join(villages %>% filter(type=="real") %>%  select(o_village=name), .predicate=st_intersects()) %>%
  st_drop_geometry() %>%
  st_as_sf(coords=c("d_east", "d_north"), crs=27700) %>%
  st_join(villages %>% filter(type=="real") %>%  select(d_village=name), .predicate=st_intersects())  %>%
  st_drop_geometry() %>%
  filter(!is.na(o_village) & !is.na(d_village)) %>%
  group_by(o_village, d_village) %>%
  summarise(count=n())

commute_villages  <- commute_points %>% select(-msoa) |>
  st_as_sf(coords=c("d_east", "d_north"), crs=27700) |>
  st_join(villages |>  filter(type=="real") %>%  select(d_village=name), .predicate=st_intersects()) %>%
  st_drop_geometry() %>%
  group_by(d_village) %>%
  summarise(count=n())
```

## Assign bikeshare trips to villages

Bikeshare trips can be straightforwardly assigned to villages. Given
that trajectories are not known, and so estimated using a routing
algorithm, we have more confidence in the routes being reliable
approximations of actually cycled trajectories for utility, commuter
journeys, and so we filter on those trip pairs more likely to be
commutes – those occurring during the weekday morning peak.

``` r
# Identify trip types.
# Trip type time bins.
am_peak_int <- interval(hms::as_hms("06:00:00"), hms::as_hms("09:59:59"))
pm_peak_int <- interval(hms::as_hms("16:00:00"), hms::as_hms("19:59:59"))
interpeak_int <- interval(hms::as_hms("10:00:00"), hms::as_hms("15:59:59"))
night_int <- interval(hms::as_hms("21:00:00"), as.POSIXct(hms::as_hms("05:59:59"))+days(1))

bs_data <- bs_data |>
  mutate(
    t=as.POSIXct(hms::as_hms(start_time)),
    wkday=!wday(start_time, label=TRUE) %in% c("Sat", "Sun"),
    commute= wkday & (t %within% am_peak_int ) | (t %within% pm_peak_int ),
    am_commute=wkday & (t %within% am_peak_int )
  ) |>
  select(-c(t,wkday))

# Join on trips and summarise over village>village counts.
bs_trips_villages  <- bs_data |>
  # Join trips on villages.
  left_join(bike_stations |> mutate(id=as.character(ucl_id)) |> select(id, o_village=village, o_x=x, o_y=y) |> st_drop_geometry(), by=c("start_station_id"="id")) |>
  left_join(bike_stations |> mutate(id=as.character(ucl_id)) |> select(id, d_village=village, d_x=x, d_y=y) |> st_drop_geometry(), by=c("end_station_id"="id")) |>
   # Clean out trips < 500m
   mutate(dist_straight= sqrt( ((o_x-d_x)^2) + ((o_y-d_y)^2) ) ) |>
   filter(start_station_id != end_station_id, dist_straight > 500) |>
  group_by(o_village, d_village) |>
  summarise(
    count=n()+1, commute=sum(as.numeric(commute))+1, leisure=(count-commute)+1,
    am_commute=sum(as.numeric(am_commute))+1
    ) |> ungroup()
```

## Model bikeability against counts

In order to set-up the model, we create the dataset by joining the
bikeability data on the bikeshare trip and commute data aggregated to
bikeshare village level. We also identify bikeshare villages that
contain hub docking stations and so code up OD pairs for those hub
stations (stations at major rail hubs). Inspecting a frequency
distribution of OD trips reinforces the fact that a large number of
village OD pairs are cycled infrequently.

``` r
service_pressure <- service_pressure |>
  left_join(
    bike_stations |> st_drop_geometry() |> select(ucl_id, village),
    by=c("stationId"="ucl_id")
    ) |>
  filter(hour<11) |>
  group_by(village) |> summarise(pressure=mean(low_avail_count))

# Join OD village bikeability data on the commute and bikeshare trip count datasets.
model_data <- bikeability_village |>
  left_join(commute_villages |> ungroup() |> rename(commute_count=count), by=c("o_village"="o_village", "d_village"="d_village")) |>
    left_join(bs_trips_villages |> ungroup() |>  rename(bs_count=count, bs_commute=commute, bs_leisure=leisure), by=c("o_village"="o_village", "d_village"="d_village")) |>
  left_join(service_pressure, by=c("d_village"="village"))  |> rename(d_pressure=pressure) |>
    left_join(service_pressure, by=c("o_village"="village")) |>  rename(o_pressure=pressure) |>
  # Identify OD village pairs involving hub stations.
  mutate(is_hub=case_when(
           d_village %in% (c("Waterloo | South Bank", "King's Cross", "Liverpool Street", "Euston", "Marylebone", "Victoria | Pimlico", "Borough | Bermondsey")) ~ TRUE,
           o_village %in% (c("Waterloo | South Bank", "King's Cross", "Liverpool Street", "Euston", "Marylebone", "Victoria | Pimlico", "Borough | Bermondsey")) ~ TRUE,
           TRUE ~ FALSE
           )
         ) |>
  filter(o_village!=d_village)


plot <- model_data |> mutate(bs_rank=min_rank(-bs_commute)) |>
  filter(bs_commute>0) |>
  ggplot(aes(x=bs_rank, bs_commute)) +
  geom_line() +
  labs(x="ranked OD pair", y="OD pair trip count", title="Rank-size plot of London bikeshare OD pairs", subtitle="-- Peak time trips collected from 2018")
ggsave(filename=here("figs", "rank-size.png"), plot=plot,width=7, height=5, dpi=300)
ggsave(filename=here("figs","rank-size.svg"), plot=plot,width=7, height=5)
```

<img src="./figs/rank-size.svg" style="width:60.0%" />

We specify the model.

``` r
lmer_spec <-
  linear_reg() |>
  set_engine("lmer")

model <- model_data |>
  mutate(bs_rank=min_rank(-bs_commute), pressure=o_pressure+d_pressure) |>
  mutate(
    across(
      .cols=c(bs_commute, commute_count, am_commute),
      .fns=~replace_na(.x,1)
    ),
     across(
      .cols=c(bs_commute, commute_count, am_commute),
      .fns=~log(.x)
    ),
    across(
      .cols=c(bs_commute, am_commute, commute_count, index, attractiveness, comfort, safety, coherence, pressure, dist),
      .fns=~(.x-mean(.x, na.rm=TRUE))/sd(.x, na.rm=TRUE),
      .names = "z_{.col}"
    ),
  )  |>
  mutate(type="full_dataset") |>
  nest(data=-type) |>
  mutate(
    model=map(data, ~
                lmer_spec |>
                parsnip::fit(z_am_commute ~  z_index + z_commute_count + z_pressure + z_dist + is_hub + (1 | d_village), data=.x)),
    values=map2(model, data, ~augment(.x, new_data=.y)),
    fits=map(model, glance),
    coefs=map(model, tidy)
  )

performance::r2_nakagawa(model$model)
```

And plot model outputs.

``` r
plot <- model |> select(coefs) |> unnest() |> filter(!term %in% c("(Intercept)"), effect=="fixed") |>
  mutate(term= case_when(
    term=="z_index" ~ "bikeability",
    term=="z_commute_count" ~ "census commuting",
    term=="z_pressure" ~ "service pressure",
    term=="z_dist" ~ "routed distances",
    term=="is_hubTRUE" ~ "contains hub stations",
    TRUE ~ "",
  ),
  is_index=term=="bikeability") |>
  ggplot(aes(x=reorder(term, estimate), y=estimate)) +
  stat_dist_gradientinterval(
    aes(dist=dist_normal(mu=estimate, sigma=std.error), fill=is_index),
    show_point=FALSE, show_interval=FALSE) +
  geom_hline(yintercept=0, size=.2, alpha=.5)+
  geom_text( data=. %>% filter(estimate>0), aes(x=term, y=-.05, label=term), hjust="right", family="Avenir Book") +
  geom_text( data=. %>% filter(estimate<0, term != "routed distances"), aes(x=term, y=+.05, label=term), hjust="left", family="Avenir Book") +
  geom_text( data=. %>% filter(term == "routed distances"), aes(x=term, y=estimate+.05, label=term), hjust="left", family="Avenir Book") +
  geom_spoke(aes(colour=is_index), angle=0, radius=.5, position="center_spoke",alpha=.3, size=.3) +
  scale_y_continuous(limits=c(-.9,.9), expand = c(0, 0)) +
  labs(y="coefficient estimate (z-score units)", x="explanatory variable", title="Outputs from model estimating trip counts", subtitle="Morning-peak bikeshare trip between bikeshare villages", caption="Conditional R^2 .85, Marginal R^2 .78")+
  coord_flip() +
  scale_colour_manual(values=c("#252525", "#08519c"))+
  scale_fill_manual(values=c("#252525", "#08519c")) +
  guides(colour="none", fill="none")+
  theme(
    plot.title = element_text(size=13),
    axis.text.y = element_blank()
  )

ggsave(filename=here("figs", "model_outputs.png"), plot=plot,width=5.5, height=3, dpi=300)
ggsave(filename=here("figs","model_outputs.svg"), plot=plot,width=5.5, height=3)
```

<img src="./figs/model-outputs.svg" style="width:60.0%" />

## Plot OD bikeability

![](./figs/index.png)

## Compare against infrastructure provision

<img src="./figs/infra_comp.svg" style="width:80.0%" />
