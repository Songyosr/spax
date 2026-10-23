# Getting started: how accessible is GP care in Cornwall?

This guide measures how accessible general practice (GP) care is across
mainland Cornwall and maps the answer. By the end you will understand
the idea behind a floating catchment area (FCA) index, compute one from
a population grid, a table of practices and travel times, and read its
map. The data are the open Cornwall GP data that ship with spax,
described in [The Cornwall GP example
data](https://songyosr.github.io/spax/articles/cornwall-data.md). The
[`spax()`](https://songyosr.github.io/spax/reference/spax.md) interface
and the allocation model are experimental and may change in later
versions.

The guide uses spax with terra for grids, sf and dplyr for tables and
ggplot2 for the practices map.

``` r

library(spax)
library(terra)
library(sf)
library(dplyr)
library(ggplot2)
```

## The question

Some residents live next to a practice with many GPs. Others live far
from any practice, or near one that many people share. An accessibility
index puts both into one number per place: the **supply** of a service
(here GP full-time equivalents, FTE) relative to the **demand** that
competes for it (here residents), with distant practices counting for
less. Where in Cornwall is that number high, and where is it low?

## The idea in two steps

An FCA index answers the question in two steps. A tiny example with
three areas and two practices shows both. The weights say how much each
practice counts for each area: 1 next door, less further away.

``` r

# Residents of three areas, and GP FTE at two practices
demand <- c(village = 1000, town = 4000, coast = 2000)
supply <- c(north = 3, south = 2)

# Weight of each practice (columns) for each area (rows)
weights <- rbind(
  village = c(north = 1.0, south = 0.1),
  town    = c(north = 0.5, south = 0.5),
  coast   = c(north = 0.1, south = 1.0)
)
weights
```

``` spax-output
        north south
village   1.0   0.1
town      0.5   0.5
coast     0.1   1.0
```

The village is next to the north practice, the coast next to the south
practice, and the town lies between the two.

**Step 1.** Each practice gathers the residents around it, each counted
with its weight, and divides its GP FTE by that weighted count.

``` r

# For each practice: residents times weight, summed over the areas
gathered <- colSums(weights * demand)
gathered
```

``` spax-output
north south 
 3200  4100 
```

``` r

# GP FTE per 1,000 weighted residents
ratio <- supply / gathered
round(1000 * ratio, 2)
```

``` spax-output
north south 
 0.94  0.49 
```

The north practice has 3 GP FTE for 3,200 weighted residents, about 0.94
per 1,000. The south practice has 2 for 4,100, about 0.49 per 1,000.

**Step 2.** Each area adds up the ratios of the practices around it,
weighted again.

``` r

# For each area: practice ratios times weight, summed over the practices
index <- drop(weights %*% ratio)
round(1000 * index, 2)
```

``` spax-output
village    town   coast 
   0.99    0.71    0.58 
```

``` r

# Residents times the index, summed over the areas
sum(index * demand)
```

``` spax-output
[1] 5
```

The village leans on the better-supplied north practice and scores
highest, at 0.99 GP FTE per 1,000 residents. The coast leans on the
south practice and scores lowest, at 0.58. Residents times the index add
up to the 5 GP FTE of the two practices: the index shares the supply out
among residents and creates none.

The weighted residents around a practice are its catchment, which floats
with the practice instead of following area borders. With weights of
only 1 within a travel-time limit and 0 beyond it, these two steps are
the original two-step FCA (2SFCA). Graded weights, as here, give the
enhanced two-step FCA, or **E2SFCA**. In a real study the weights come
from travel times through a decay curve, and spax computes them for
every place and every practice. With the curve used below, a weight of
0.5 means about 12 minutes away and 0.1 about 21 minutes.

## The Cornwall data

spax ships open data for GP care in Cornwall and the Isles of Scilly:
the 55 practices with their GP FTE, 337 small areas with their
registered residents, who is registered at which practice, and travel
times. A population grid of about 1 km cells and one layer of travel
minutes per practice cover the same area. The Isles of Scilly have no
road link to the mainland, so the guides use mainland Cornwall only,
with its 54 practices. [The Cornwall GP example
data](https://songyosr.github.io/spax/articles/cornwall-data.md)
describes each object, its sources and its limits. The map shows the
mainland practices, sized by their GP FTE, with a few towns named for
reference.

``` r

# Mainland practices and small areas, with their geometry
practices <- filter(cornwall_gp, !scilly)
small_areas <- filter(cornwall_lsoa, !scilly)
outline <- st_union(small_areas)

# A few towns, by longitude and latitude, to find places on the maps
towns <- st_as_sf(
  data.frame(
    town = c("Penzance", "Helston", "Redruth", "Truro", "Falmouth", "Bodmin", "Bude", "Launceston"),
    lon = c(-5.54, -5.27, -5.23, -5.05, -5.07, -4.72, -4.54, -4.36),
    lat = c(50.12, 50.10, 50.23, 50.26, 50.15, 50.47, 50.83, 50.64)
  ),
  coords = c("lon", "lat"),
  crs = 4326
)

ggplot() +
  geom_sf(
    data = small_areas,
    fill = "grey95",
    color = "grey70",
    linewidth = 0.1
  ) +
  geom_sf(
    data = practices,
    aes(size = gp_fte_extg),
    shape = 21,
    fill = "orange",
    alpha = 0.8
  ) +
  geom_sf(data = outline, fill = NA, color = "grey20", linewidth = 0.3) +
  geom_sf_text(
    data = towns, aes(label = town), size = 3, vjust = -0.9,
    fun.geometry = sf::st_centroid
  ) +
  scale_size_area(name = "GP FTE") +
  labs(title = "GP practices in mainland Cornwall") +
  theme_void()
```

![Map of mainland Cornwall's small areas in light grey, with the 54
mainland GP practices as orange circles sized by GP full-time
equivalents, and eight towns named: Penzance, Helston, Redruth, Truro,
Falmouth, Bodmin, Bude and
Launceston.](spax_files/figure-html/practices-map-1.png)

Practices are densest in the west, around Redruth, Truro and Falmouth.
The largest has 13.4 GP FTE. Bodmin Moor, the upland north-east of
Bodmin, has none.

## Compute the index

An FCA index needs three inputs that line up: a population grid of
residents per cell (demand), a table of practices with their GP FTE
(supply), and a stack of travel-time grids with one layer of minutes per
practice, named by practice code. These lines read them and keep the
mainland cells that have a travel time to every practice. Every FCA
guide starts from them.

``` r

gp <- cornwall_gp |>
  filter(!scilly) |>
  st_drop_geometry()
pop <- read_spax_example("cornwall_pop.tif")
travel_grid <- read_spax_example("cornwall_gp_travel.tif")[[gp$practice]]
# Keep cells with a travel time to every mainland practice
pop <- trim(mask(pop, sum(travel_grid)))
travel_grid <- crop(travel_grid, pop)
grid_data <- prep_data(pop, gp, travel_grid, id_col = "practice", supply_cols = "gp_fte_extg")
```

`gp` is the practices table without its map geometry.
[`prep_data()`](https://songyosr.github.io/spax/reference/prep_data.md)
takes the table and holds the three inputs together. `id_col` names the
column of practice codes that matches the travel layers, and
`supply_cols` the column of supply.

[`spec_e2sfca()`](https://songyosr.github.io/spax/reference/spec_e2sfca.md)
says which index to compute and how.
[`spax()`](https://songyosr.github.io/spax/reference/spax.md) then
computes it on `grid_data`. The decay turns travel minutes into weights.
Here it is a Gaussian curve whose travel scale σ, in minutes, sets how
fast the weights fall. With σ = 10, a practice 10 minutes away counts
about 61% as much as one next door, and one 20 minutes away about 14%.
The scale is a choice for this guide. [Where do residents
register?](https://songyosr.github.io/spax/articles/allocation-workflow.md)
estimates a travel scale from observed registrations. This line computes
the index on the Cornwall data and prints a summary.

``` r

e2sfca <- spax(grid_data, spec_e2sfca(decay = decay_gaussian(sigma = 10)))
e2sfca
```

``` spax-output
<spax access> E2SFCA, 1 layer on 117 x 187 cells

Spatial Properties:
- Resolution: 0.00833333 x 0.00833333
- Dimensions: 117 x 187

Measures: 1
- gp_fte_extg

Facilities: 54

Parameters:
- Decay function: gaussian (sigma = 10)
- Demand normalization: identity 
```

The print gives the grid: 117 rows by 187 columns of cells about 1 km
across (the resolution is in degrees, 30 arc-seconds). It also gives the
supply column it used (`gp_fte_extg`), the 54 practices and the
settings: the Gaussian decay with σ = 10, and the default normalization
`identity`, which uses the weights unchanged when practices gather
residents. The `accessibility` member is the index on the grid, a raster
in GP FTE per resident. We map it per 1,000 residents, with the
practices as red crosses and the same towns named.

``` r

# Practice and town locations in the grid's coordinates (longitude and latitude)
practice_points <- vect(practices)
town_points <- vect(towns)
outline_lonlat <- vect(outline)

plot(
  1000 * e2sfca$accessibility,
  axes = FALSE,
  col = hcl.colors(50, "viridis"),
  main = "E2SFCA: GP FTE per 1,000 residents (Gaussian, sigma = 10 min)"
)
lines(outline_lonlat, lwd = 0.6)
points(practice_points, pch = 3, cex = 0.5, col = "red")
text(town_points, labels = town_points$town, pos = 3, cex = 0.7, halo = TRUE)
```

![Raster map of the E2SFCA index for mainland Cornwall in GP FTE per
1,000 residents, with practices as red crosses and eight towns named;
values are highest in the west between Redruth, Truro and Falmouth,
lowest on Bodmin Moor and at the tips of the Land's End and Lizard
peninsulas, and lower in the far
north-east.](spax_files/figure-html/e2sfca-map-1.png)

## Read the map

The index runs from 0.03 to 0.81 GP FTE per 1,000 residents. Averaged
over residents it is 0.58, the same as the mainland’s 311 GP FTE for its
541,000 residents on the grid. As in the small example, the index shares
out the supply and creates none.

It is highest in the bright band between Redruth, Truro and Falmouth,
where several practices with many GP FTE lie within a short drive. It is
lowest on Bodmin Moor, the dark patch north-east of Bodmin, and at the
tips of the Land’s End peninsula, west of Penzance, and the Lizard,
south of Helston. There the nearest practices are further away. Read the
values along the border with Devon, at the right edge of the map, with
care: the data cover Cornwall only, so the index leaves out the Devon
practices that some residents there use, and the Devon residents who
compete for Cornish practices.

The index compares GP supply with nearby demand and says nothing about
who uses a practice, waiting times or the care received ([Reading
results](https://songyosr.github.io/spax/articles/reading-results.md)
explains).

## Next

- [Which FCA
  index?](https://songyosr.github.io/spax/articles/fca-methods.md): set
  E2SFCA beside 2SFCA, M2SFCA, 3SFCA and Huff-2SFCA on the same grid,
  and see what changes.
- [Bringing your own
  data](https://songyosr.github.io/spax/articles/data-prep.md): line up
  your own counts, facility IDs and travel times.
- [Where do residents
  register?](https://songyosr.github.io/spax/articles/allocation-workflow.md):
  model which practice the residents of each area register with, and
  estimate the travel scale from those registrations instead of choosing
  it.
- [An FCA index or an allocation
  model?](https://songyosr.github.io/spax/articles/model-choice.md):
  choose between the two for your question.
