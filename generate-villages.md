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
library(tidyverse)              
library(sf)                     

# ggplot theme for paper
source(here("code","theme_paper.R"))
```

## Load villages from [jwoLondon](https://github.com/jwoLondon/mobv/blob/master/data/london/geo/localities.json)

To analyse changes in London bikeshare use pre, during and after
Covid-19 restrictions, Jo Wood and colleagues created 84 bikeshare
‘villages’ – labelled neighbourhoods which, assuming some familiarity
with central London, are reasonably coherent and discriminating. We
borrow these villages, but merge several to reduce the total number to
66.

First load villages and their corresponding centroids from
[github.com/jwoLondon/mobv](https://github.com/jwoLondon/mobv).

``` r
# Localities from https://github.com/jwoLondon/mobv.
localities <- st_read(here("data", "localities.geojson"), crs=27700) |> 
  left_join(read_csv(here("data", "locality_centroids.csv")) |>  select(name, region))

locality_centroids <- read_csv(here("data", "locality_centroids.csv")) |> 
  st_as_sf(coords=c("lon","lat"), crs=4326) |> 
  st_transform(crs=27700) |> 
  st_coordinates() |> 
  as_tibble() |> 
  bind_cols(localities |>  st_drop_geometry() |>  select(name, region)) |> 
  rename(easting=X, northing=Y) |> 
  relocate(name, .before=easting)
```

## Aggregate [jwoLondon](https://github.com/jwoLondon/mobv/blob/master/data/london/geo/localities.json) villages

Then we manually aggregate selected localities and create new centroids
for those aggregated localities. Code hidden for brevity, but can be
viewed in [generate-villages.Rmd](generate-villages.Rmd).

Next we create a voronoi tessellation around our new localities.

``` r
# Define outline of original localities in order to clip Voronoi.
localities_outline <- localities |>  st_buffer(dist=50, .predictate=st_intersects) |> 
  summarise()
# Voronoi tessellation on new points.
v <- st_voronoi(st_union(localities_agg |> st_as_sf(coords=c("easting","northing"), crs=27700)),
                localities_outline$geometry)


# Clip Voronoi, extract centroids and write to file.
v <- st_intersection(st_cast(v), st_union(localities_outline)) |> 
  as_tibble() |>  st_as_sf() |> 
  st_join(
    localities_agg |>  st_as_sf(coords=c("easting","northing"), crs=27700)
    )
v_centroids <- v |>  st_centroid() |>  st_coordinates() |>  as_tibble() |> 
    rename(easting=X, northing=Y)
v <- v |>  bind_cols(v_centroids)
st_write(v, here("data", "villages_agg.geojson"))
rm(v)
```

Localities (now termed *villages*) were then manually edited in QGIS to
address issues such as villages crossing the river or localities on the
edge of scheme missing docking stations on the scheme boundary.

``` r
villages <- st_read(here("data", "villages_agg_extent.geojson"))
```

The semi-spatial grid-layout of villages used in the paper was
generating using the web-tool published alongside [Meulemans et
al. 2017](https://www.gicentre.net/small-multiples-with-gaps). This can
now be achieved using the
[`gridmappr`](https://github.com/rogerbeecham/gridmappr) R package.

``` r
devtools::install_github("gridmappr")
library(gridmappr)
# Make sf grid object over localities.
n_row <- 10
n_col <- 13
pts <- villages |> st_drop_geometry() |> select(name_agg, x=easting, y=northing)
solution <- points_to_grid(pts, n_row, n_col, .6)
villages_grid <- make_grid(villages, n_row, n_col) |> inner_join(solution) 
```

## Plot villages

![Bikeshare villages](./figs/anim_real_grid.gif) The two geographies –
semi-spatial grid and real – are merged to a single dataset and written
out to file, also to explore displacement using `gganimate`.

``` r
# Calculate real centroids for relabelling.
villages <- villages |>
  left_join(solution) |> 
  select(col, row, x=easting, y=northing, name_agg)

grid_real <- bind_rows(
  villages |> arrange(name_agg) |>  mutate(type="real", id=row_number()) |>  arrange(id), 
  villages_grid |> arrange(name_agg) |>  rename(geometry=geom) |> mutate(type="grid", id=row_number()) |>  arrange(id) 
  ) |> 
  mutate(type=fct_relevel(as_factor(type), "real","grid"))
  
# Write out.
st_write(grid_real, here("data", "grid_real_sf.geojson"), append=FALSE)

# Show displacement by morphing between layouts.
displacement <- grid_real |> 
  ggplot()+
  geom_sf(colour="#616161", size=0.15)+
  coord_sf(crs=27700, datum=NA)+
  geom_text(aes(x=x, y=y, label= gsub("*\\|.", "\n ", name_agg)),
             size=2.1, alpha=1, show.legend=FALSE, family="Avenir Book", colour="#000000")+
  gganimate::transition_states(type, 1, 2)+
  labs(title="Demonstrating grid layout of bikeshare villages for OD map",
        subtitle="--Layout generated using `gridmappr` R package")+
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
geographic structure with this number of pairs using *de facto* flow
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
