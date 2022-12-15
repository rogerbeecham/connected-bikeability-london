Generate villages
================

## Introduction

This document describes how bikeshare villages – the spatial unit of
analysis customised for connected bikeability – was generated.

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
made with the `<package-name>::<function-name>()` syntax so as to avoid
polluting the workspace.

``` r
pkgs <- c("tidyverse","sf", "here", "gganimate")
# If not already installed.
# install.packages(pkgs)
# Core packages
library(tidyverse)              # Bundle of packages for data manipulation.
library(sf)                     # For working with geospatial data.

# ggplot theme for paper
source(here("code","theme_paper.R"))
```

## Load villages from [jwoLondon](https://github.com/jwoLondon/mobv/blob/master/data/london/geo/localities.json)

In analysing changes in London bikeshare use pre, during and after
Covid-19 restrictions, Jo Wood and colleagues define to 84 bikeshare
‘villages’ – labelled neighbourhoods which, assuming some familiarity
with central London, are reasonably coherent and discriminating. We
borrow these villages, but merge several to reduce the total number to
66.

First load villages and their corresponding centroids from
[github.com/jwoLondon/mobv](https://github.com/jwoLondon/mobv).

``` r
# Localities from https://github.com/jwoLondon/mobv.
localities <- st_read(here("data", "localities.geojson"), crs=27700) %>%
  left_join(read_csv(here("data", "locality_centroids.csv")) %>% select(name, region))

locality_centroids <- read_csv(here("data", "locality_centroids.csv")) %>%
  st_as_sf(coords=c("lon","lat"), crs=4326) %>%
  st_transform(crs=27700) %>%
  st_coordinates() %>%
  as_tibble() %>%
  bind_cols(localities %>% st_drop_geometry() %>% select(name, region)) %>%
  rename(easting=X, northing=Y) %>%
  relocate(name, .before=easting)
```

## Aggregate [jwoLondon](https://github.com/jwoLondon/mobv/blob/master/data/london/geo/localities.json) villages

Then we manually aggregate selected localities and create new centroids
for the aggregated localities.

``` r
localities_agg <- locality_centroids %>%
  mutate(name_agg = case_when(
    name == "Putney" ~ "Putney | Wandsworth",
    name == "Wandsworth" ~ "Putney | Wandsworth",
    name == "Wandsworth Road" ~ "Stockwell | Wandsworth Road",
    name == "Stockwell" ~ "Stockwell | Wandsworth Road",
    name == "Vauxhall" ~ "Vauxhall | Kennington",
    name == "Kennington" ~ "Vauxhall | Kennington",
    name ==  "Elephant & Castle" ~ "Elephant & Castle | Walworth",
    name ==  "Borough" ~ "Borough | Bermondsey",
    name ==  "Bermondsey" ~ "Borough | Bermondsey",
    name ==  "Walworth" ~ "Elephant & Castle | Walworth",
    name ==  "Waterloo" ~ "Waterloo | South Bank",
    name ==  "South Bank" ~ "Waterloo | South Bank",
    name ==  "Southwark" ~ "Southwark | Bankside",
    name ==  "Bankside" ~ "Southwark | Bankside",
    name ==  "Millwall" ~ "Millwall | Cubitt Town",
    name ==  "Cubitt Town" ~ "Millwall | Cubitt Town",
    name ==  "Blackwall" ~ "Blackwall | Poplar",
    name ==  "Poplar" ~ "Blackwall | Poplar",
    name ==  "Bow" ~ "Bow | Old Ford",
    name ==  "Old Ford" ~ "Bow | Old Ford",
    name ==  "Stepney" ~ "Stepney | Shadwell",
    name ==  "Shadwell" ~ "Stepney | Shadwell",
    name ==  "Finsbury" ~ "Finsbury | St Luke's",
    name ==  "St Luke's" ~ "Finsbury | St Luke's",
    name ==  "Covent Garden" ~ "Covent Garden | Strand",
    name ==  "Strand" ~ "Covent Garden | Strand",
    name == "Bloomsbury" ~ "Bloomsbury | Fitzrovia",
    name == "Fitzrovia" ~ "Bloomsbury | Fitzrovia",
    name == "Bayswater" ~ "Bayswater | Paddington",
    name == "Paddington" ~ "Bayswater | Paddington",
    name == "Notting Hill" ~ "Notting Hill | Ladbroke Grove",
    name == "Ladbroke Grove" ~ "Notting Hill | Ladbroke Grove",
    name == "Sands End" ~ "Sands End | Parson's Green",
    name == "Parson's Green" ~ "Sands End | Parson's Green",
    # name == "Waltham Green" ~ "Waltham Green | West Chelsea",
    # name == "West Chelsea" ~ "Waltham Green | West Chelsea",
    name == "Victoria" ~ "Victoria | Pimlico",
    name == "Pimlico" ~ "Victoria | Pimlico",
    TRUE ~ name
  )) %>%
  group_by(name_agg) %>%
  summarise(easting=mean(easting), northing=mean(northing), region=first(region))
```

Next we create a voronoi tessellation around our new localities.

``` r
# Define outline of original localities in order to clip voronoi.
localities_outline <- localities %>% st_buffer(dist=50, .predictate=st_intersects) %>%
  summarise()
# Voronoi tessellation on new points.
v <- st_voronoi(st_union(localities_agg %>%
                           st_as_sf(coords=c("easting","northing"), crs=27700)),
                localities_outline$geometry)


# Clip Voronoi, extract centroids and write to file.
v <- st_intersection(st_cast(v), st_union(localities_outline)) %>%
  as_tibble() %>% st_as_sf() %>%
  st_join(
    localities_agg %>% st_as_sf(coords=c("easting","northing"), crs=27700)
    )
v_centroids <- v %>% st_centroid() %>% st_coordinates() %>% as_tibble() %>%
    rename(easting=X, northing=Y)
v <- v %>% bind_cols(v_centroids)
st_write(v, here("data", "villages_agg.geojson"))
rm(v)
```

Localities (now termed *villages*) were then manually edited in QGIS to
address issues such as villages crossing river or localities on edge of
scheme not missing docking stations on the scheme boundary.

``` r
villages <- st_read(here("data", "villages_agg_extent.geojson"))
```

The semi-spatial grid-layout of villages was generating using the
web-tool published alongside [Meulemans et
al. 2017](https://www.gicentre.net/small-multiples-with-gaps). EDIT:
update with package with LP implementation (under development).

``` r
# SMWG for grid layout,
villages_grid <- read_tsv(here("data", "villages_grid_agg.tsv"))
```

## Plot villages

![Bikeshare villages](./figs/anim_real_grid.gif)

To plot villages using the semi-spatial arrangement we need to construct
geometries representing the grid. To do this generate a grid over the
villages according to the dimensions of the smwg layout – $14x9$.

``` r
# Make sf grid object over localities.
grid_sf <-  st_sf(geom=st_make_grid(villages, n=c(14,9), what="polygons")) %>%
  mutate(id=row_number())
```

Then we must generate a vector of coordinates for each location in the
grid, so that the relevant grid positions can be matched with their
corresponding bikeshare villages defined in `villages_grid`.

``` r
# Helper function for rescaling.
map_scale <- function(value, min1, max1, min2, max2) {
  return  (min2+(max2-min2)*((value-min1)/(max1-min1)))
}

# Store grid cell locations and add as fields.
width <- 14
height <- 9

x <- rep(0:width-1,height)
y <- vector(length=length(x))
for(i in 0:height-1) {
  for(j in 0:width-1) {
    index=1+((i)*width+j)
    y[index] <- i
    y[index] <- map_scale(i,0,height-1,height-1,0)
  }
}

x <- rep(0:width-1,height)
y <- vector(length=length(x))
for(i in 0:height-1) {
  for(j in 0:width-1) {
    index=1+((i)*14+j)
    y[index] <- i
    y[index] <- map_scale(i,0,height-1,height-1,0)
  }
}

# Add to grid_sf and then join on villages_grid to filter only
# cells that are villages.
grid_sf <- grid_sf %>% add_column(x=x, y=y) %>%
  inner_join(villages_grid, by=c("x"="gridX", "y"="gridY")) %>%
  st_cast(to="MULTIPOLYGON") %>%
  rename("geometry"="geom","name"="region") %>%
  arrange(id)
# Calculate grid centroids for labelling.
grid_centroids <- grid_sf %>% st_centroid() %>% st_coordinates() %>% as_tibble() %>%
  rename("east"="X", "north"="Y")
# Add to grid_sf.
grid_sf <- grid_sf %>%
  mutate(east=grid_centroids$east, north=grid_centroids$north, type="grid")
rm(grid_centroids)
```

Finally the two geographies – semi-spatial grid and real – are merged to
a single dataset and written out to file, also to explore displacement
using `gganimate`.

``` r
# Calculate real centroids for relabelling.
real_centroids <- villages %>% st_centroid() %>% st_coordinates() %>% as_tibble() %>%
  rename("east"="X", "north"="Y")
# Add centroids to real_sf.
real_sf <- villages %>%
  mutate(east=real_centroids$east, north=real_centroids$north) %>%
  left_join(villages_grid, by=c("name_agg"="region")) %>%
  select(name=name_agg, east, north, gridX, gridY) %>%
  # Join on grid_sf (geometry dropped) in order to arrange on id.
  left_join(grid_sf %>% select(id, name) %>% st_drop_geometry()) %>%
  mutate(type="real") %>%
  relocate(type, .after=id) %>%
  rename(x=gridX, y=gridY)

# Rename/organise grid_sf so can rbind with real_sf.
grid_sf <- grid_sf %>%
  select(name, east, north, x, y, id, type)
# rbind() in order to lerp between layouts.
grid_real_sf <- rbind(grid_sf %>% arrange(id), real_sf %>% arrange(id)) %>%
  mutate(type=fct_relevel(as_factor(type), "real","grid"))

# Write out.
st_write(grid_real_sf, here("data", "grid_real_sf.geojson"))

# Show displacement by morphing between layouts.
displacement <- grid_real_sf %>%
  ggplot()+
  geom_sf(colour="#616161", size=0.15)+
  coord_sf(crs=27700, datum=NA)+
  geom_text(aes(x=east, y=north, label= gsub("*\\|.", "\n ", name)),
             size=2.1, alpha=1, show.legend=FALSE, family="Avenir Book", colour="#000000")+
  gganimate::transition_states(type, 1, 2)+
  labs(title="Demonstrating grid layout of bikeshare villages for OD map",
        subtitle="--Arrangement using SMWG LP",
        caption="See: https://www.gicentre.net/smwg")+
  theme(axis.title.x=element_blank(), axis.title.y=element_blank())

gganimate::animate(
  displacement, duration=5, fps=10, width=1600, height=1000, res=150,
  renderer=gganimate::gifski_renderer(here("figs", "anim_real_grid.gif"))
)
```

## OD maps

We generate estimates of bikeability for journeys made between all
bikeshare villages – up to $66^2$ (4,326) OD village-village pairs.
Although the bikeshare villages offer a degree of aggregation, inferring
geographic structure with this number of pairs using {de facto} flow
visualizations is challenging. Problems of clutter and salience bias
hinder meaningful analysis. OD maps are one alternative ([Wood et
al. 2010](https://www.tandfonline.com/doi/abs/10.1179/000870410X12658023467367)).
They are essentially origin-destination matrices, but where the cells
are given a two-level geographic arrangement using a map-within-map
layout.

![Map-within-map layout for OD map](./figs/map_map.svg)

In our implementation the large grid squares are bikeshare villages with
an approximate geographic arrangement, and each represents a trip
*destination*. Embedded in every larger cell is a map of bikeshare
origins. The origin maps are then shaded according to a quantity of
interest – bikeability scores in the maps below. This means that each
village–village OD pair is roughly equally visually salient, allowing
detailed patterns in connected bikeability to be analysed concurrently.

![OD map of connected bikeability](./figs/index.png)

To demonstrate the relative benefits of the OD-map layout, we can
compare with a typical flow visualization with lines sized and coloured
according to bikeabillity, also ‘optimised’ to try to reduce the
hairball effect.

![Flow map of connected bikeability –
hairball](./figs/index_hairball.svg)
