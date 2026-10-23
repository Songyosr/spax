# The Cornwall GP example data

spax ships open data on general practice (GP) care in NHS Cornwall and
the Isles of Scilly, in south-west England, and the guides run on it.
This page answers one question: what is in the example data? It
introduces each object once: what it holds, how the objects fit together
and what they cannot tell you. By the end you can load each one and
build the inputs that the guides start from. Nothing needs downloading.
The [`spax()`](https://songyosr.github.io/spax/reference/spax.md)
interface and the allocation model are experimental and may change in
later versions.

The page uses spax with terra for the grids, sf for the points and
polygons, and dplyr and ggplot2 for tables and maps.

``` r

library(spax)
library(terra)
library(sf)
library(dplyr)
library(ggplot2)
```

Four objects load with spax, and two grids are read from files:

| Object | What it holds | How to load it |
|:---|:---|:---|
| `cornwall_gp` | 55 GP practices: location, GP workforce and list size | Loaded with spax |
| `cornwall_lsoa` | 337 small areas: boundaries, population and registered residents | Loaded with spax |
| `cornwall_flows` | Registered residents by small area and practice | Loaded with spax |
| `cornwall_travel` | Travel minutes from each small area to each practice | Loaded with spax |
| `cornwall_pop.tif` | Population grid, about 1 km cells | [`read_spax_example()`](https://songyosr.github.io/spax/reference/read_spax_example.md) |
| `cornwall_gp_travel.tif` | Travel minutes from each grid cell to each practice | [`read_spax_example()`](https://songyosr.github.io/spax/reference/read_spax_example.md) |

Each object has a help page with every column, for example
[`?cornwall_gp`](https://songyosr.github.io/spax/reference/cornwall_gp.md).

## Practices

`cornwall_gp` is an `sf` point layer in longitude and latitude (WGS 84,
EPSG:4326), one row per practice; `easting` and `northing` give the
British National Grid coordinates in metres. `practice` is the NHS
practice code, which links the practices to the registrations and to the
travel times. The code drops the geometry and shows the first practices.

``` r

gp <- st_drop_geometry(cornwall_gp)
gp |>
  select(practice, name, gp_fte_extg, gp_fte, list_size) |>
  head()
```

``` spax-output
  practice                                name gp_fte_extg    gp_fte list_size
1   L82001             LANDER MEDICAL PRACTICE   10.298887 12.858887     19477
2   L82003                  PORT ISAAC SURGERY    4.693333  5.546667      8738
3   L82004 WADEBRIDGE & CAMEL ESTUARY PRACTICE    4.341333  5.194667      8045
4   L82006                      PENRYN SURGERY    7.973333  9.893333     20089
5   L82007            CAMELFORD MEDICAL CENTRE    2.888800  2.888800      6658
6   L82008             STRATTON MEDICAL CENTRE    4.670400  6.377067     10746
```

The guides use `gp_fte_extg` as supply, what each practice offers:
full-time equivalent GPs (37.5 hours a week) excluding GPs in training,
from the June 2026 workforce data. GPs in training are left out because
they cluster in a few training practices. `gp_fte` includes them.
`list_size` counts all patients registered at the practice. Each
practice sits at its main site, so branch surgeries do not appear.

## Small areas

`cornwall_lsoa` holds the 337 Lower layer Super Output Areas (LSOA 2021)
as polygons in longitude and latitude (WGS 84, like the practices); each
area’s population-weighted centroid is given in both systems, `cent_lon`
and `cent_lat` and the British National Grid `cent_x` and `cent_y`. The
areas are small, of 988 to 3,392 residents. Each has an ONS mid-2024
population estimate (`pop`, also by age band) and the number of its
residents registered at any GP practice in England (`registered`). That
number splits into residents registered at a practice in Cornwall and
the Isles of Scilly (`registered_in`) and those registered elsewhere
(`registered_out`). The code adds up these columns and counts the areas
in each broad rural-urban class.

``` r

areas <- st_drop_geometry(cornwall_lsoa)

c(
  population = sum(areas$pop),
  registered = sum(areas$registered),
  registered_in = sum(areas$registered_in),
  registered_out = sum(areas$registered_out)
)
```

``` spax-output
    population     registered  registered_in registered_out 
        585655         606796         601269           5527 
```

``` r

# Rural-urban classification (2021), in three broad groups
table(sub(":.*", "", areas$ruc21))
```

``` spax-output

 Larger rural Smaller rural         Urban 
           81           103           153 
```

`registered_in` and `registered_out` add up to `registered`. Registered
residents outnumber the population estimate by about 4%, which reflects
list inflation (patients who stay on a list after moving away) and the
different reference dates. The map shows the small areas by broad
rural-urban class, with the practices on top.

``` r

# Small areas with a broad rural-urban class
classed <- cornwall_lsoa |>
  mutate(class = sub(":.*", "", ruc21))

ggplot() +
  geom_sf(
    data = classed,
    aes(fill = class),
    color = "white",
    linewidth = 0.1
  ) +
  geom_sf(data = st_union(cornwall_lsoa), fill = NA, color = "grey20", linewidth = 0.3) +
  geom_sf(data = cornwall_gp, size = 1) +
  scale_fill_manual(
    name = "Rural-urban class",
    values = c(
      Urban = "#5b7fa6",
      `Larger rural` = "#a9c48f",
      `Smaller rural` = "#e3dca8"
    )
  ) +
  labs(title = "Small areas (LSOA 2021) and GP practices") +
  theme_void()
```

![Map of the 337 small areas of Cornwall and the Isles of Scilly colored
by broad rural-urban class (urban, larger rural, smaller rural), with
the 55 GP practices as black
points.](cornwall-data_files/figure-html/areas-map-1.png)

153 of the 337 areas are urban, but they cover little land: the towns
show as tight blue clusters, and most of the county’s land lies in
smaller rural areas. The Isles of Scilly are the small group of islands
off the western tip.

## Registrations

`cornwall_flows` counts the residents of each small area registered at
each of the 55 practices, one row per pair with at least one
registration. The code shows the first rows, checks that each area’s
rows add up to its `registered_in`, and counts the pairs.

``` r

head(cornwall_flows)
```

``` spax-output
       lsoa practice patients
1 E01018750   L82012     2185
2 E01018750   L82016       35
3 E01018750   L82025        1
4 E01018750   L82046        4
5 E01018750   L82066        1
6 E01018751   L82012     1691
```

``` r

# Each area's registrations add up to its registered_in
by_area <- tapply(cornwall_flows$patients, cornwall_flows$lsoa, sum)
all(by_area[areas$lsoa] == areas$registered_in)
```

``` spax-output
[1] TRUE
```

``` r

c(
  pairs = nrow(cornwall_flows),
  registered_residents = sum(cornwall_flows$patients)
)
```

``` spax-output
               pairs registered_residents 
                3188               601269 
```

Every area’s rows add up to its `registered_in`, so the flows hold all
601,269 residents registered inside Cornwall and the Isles of Scilly,
spread over 3,188 area-practice pairs.

A registration is a **stock**: who is registered with which practice on
one date (July 2026), not a count of visits. Residents counted in
`registered_out` are registered with a practice outside Cornwall and the
Isles of Scilly, for example in Devon, so they are in `cornwall_lsoa`
but not in these flows. The guides on the allocation model, a model of
where each area’s residents register, fit it to these counts. The guides
on floating catchment area (FCA) indices, which measure supply per
resident, do not use them.

## Travel times

`cornwall_travel` is a matrix of motorized travel minutes from each
small area’s population-weighted centroid (rows, `cornwall_lsoa$lsoa`)
to each practice’s main site (columns, `cornwall_gp$practice`). The code
prints a corner of the matrix and summarizes the minutes from each area
to its nearest practice.

``` r

cornwall_travel[1:3, 1:4]
```

``` spax-output
          L82001 L82003 L82004 L82006
E01018750     67     49     46     78
E01018751     67     49     46     78
E01018752     68     49     47     79
```

``` r

# Minutes from each small area to its nearest practice
nearest <- apply(cornwall_travel, 1, min, na.rm = TRUE)
summary(nearest)
```

``` spax-output
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   1.000   1.000   2.564   3.000  13.000 
```

The times are least-cost paths over a 2019 global map of the time needed
to cross each cell of about 1 km (a friction surface). They are rounded
to whole minutes, with a floor of one minute, so no time is 0. 197 of
the 337 small areas have a practice at 1 minute. Traffic is not modeled.
In towns several practices can share one cell, so they have the same
travel time from every area. The map shows the time to the nearest
practice.

``` r

# Join each area's nearest-practice time to its polygon
nearest_map <- cornwall_lsoa |>
  mutate(minutes = nearest[lsoa])

ggplot(nearest_map) +
  geom_sf(aes(fill = minutes), color = "white", linewidth = 0.05) +
  geom_sf(data = st_union(cornwall_lsoa), fill = NA, color = "grey20", linewidth = 0.3) +
  scale_fill_viridis_c(name = "Minutes", option = "magma", direction = -1) +
  labs(title = "Travel time to the nearest GP practice") +
  theme_void()
```

![Map of the small areas of Cornwall and the Isles of Scilly colored by
travel minutes to the nearest GP practice, from 1 to 13 minutes; most
areas are within a few minutes of a practice, and the longest times are
at the western tip, inland in the north-east and in one area on the
north coast near
Padstow.](cornwall-data_files/figure-html/nearest-map-1.png)

Most small areas are within a few minutes of a practice. The map gives
most of its area to the large rural areas, so the many small town areas,
mostly at 1 minute, are hard to see. The longest trips, up to 13
minutes, are at the western tip, inland in the north-east and in one
area on the north coast near Padstow.

## The grids

Two GeoTIFF files cover the same 30 arc-second grid (about 1 km, in
longitude and latitude). `cornwall_pop.tif` holds WorldPop 2020
estimated residents per cell: a **count**, not a density.
`cornwall_gp_travel.tif` holds one layer of travel minutes per practice,
named by practice code.
[`read_spax_example()`](https://songyosr.github.io/spax/reference/read_spax_example.md)
reads both, and the code counts their cells, layers and residents.

``` r

pop <- read_spax_example("cornwall_pop.tif")
travel_grid <- read_spax_example("cornwall_gp_travel.tif")

c(
  cells = ncell(pop),
  populated_cells = sum(!is.na(values(pop))),
  travel_layers = nlyr(travel_grid),
  grid_residents = round(global(pop, "sum", na.rm = TRUE)[[1]]),
  ons_residents = sum(areas$pop)
)
```

``` spax-output
          cells populated_cells   travel_layers  grid_residents   ons_residents 
          35772            6985              55          542666          585655 
```

The grid holds about 7% fewer residents than the ONS mid-2024 estimate
in `cornwall_lsoa`. The two are different sources and dates: choose one
for an analysis and say which. In the travel layers, `NA` marks cells
with no residents, pairs with no road connection and journeys over 180
minutes. The map shows residents per cell in six classes.

``` r

# The outline of Cornwall and the Isles of Scilly: the small areas share the
# grid's longitude and latitude (WGS 84)
outline <- cornwall_lsoa |>
  st_union() |>
  vect()

plot(
  pop,
  breaks = c(0, 10, 50, 100, 500, 1000, 5000),
  col = hcl.colors(6, "YlGnBu", rev = TRUE),
  axes = FALSE,
  main = "WorldPop 2020: residents per cell (about 1 km)",
  plg = list(title = "Residents")
)
lines(outline, lwd = 0.6)
```

![Raster map of the WorldPop 2020 population grid for Cornwall and the
Isles of Scilly in residents per cell, in six classes; most cells hold
fewer than 50 residents and the towns stand out with several hundred or
more.](cornwall-data_files/figure-html/pop-map-1.png)

Of the populated cells, 81% hold fewer than 50 residents, and the towns
stand out with several hundred or more. The FCA guides use the grid, and
the allocation guides use the registered residents of the small areas.

## The Isles of Scilly and the mainland

The Isles of Scilly have one small area and one practice, St Mary’s
Health Centre (`L82017`). Road travel times cannot cross the sea, so
every travel time between the islands and the mainland is `NA`. The code
counts the unknown times in the small-area matrix.

``` r

c(
  unknown = sum(is.na(cornwall_travel)),
  scilly_area = sum(is.na(cornwall_travel[areas$scilly, ])),
  scilly_practice = sum(is.na(cornwall_travel[, gp$scilly]))
)
```

``` spax-output
        unknown     scilly_area scilly_practice 
            390              54             336 
```

The unknown times are the Scilly area’s row (to the 54 mainland
practices) and the Scilly practice’s column (from the 336 mainland
areas). The islands do have access to the mainland, by boat and air,
which these travel times do not represent. Rather than treat the islands
as cut off, the guides model **mainland Cornwall only**: 336 small areas
and 54 practices, selected with the `scilly` column. Mainland residents
registered with the Scilly practice or outside Cornwall then count as
registered outside mainland Cornwall. [Bringing your own
data](https://songyosr.github.io/spax/articles/data-prep.md) shows what
the unknown times do to an FCA index when the islands are kept.

## The inputs used in the guides

The guides start from one of two short recipes, so that all of them
model the same mainland. Both prepare the data with
[`prep_data()`](https://songyosr.github.io/spax/reference/prep_data.md),
which lines the inputs up by ID for
[`spax()`](https://songyosr.github.io/spax/reference/spax.md), the
function that runs a model on them.

### A grid, for the FCA indices

An FCA index measures supply per resident, counting nearby practices
more than distant ones. The FCA guides compute it on the population grid
and the travel stack. The recipe keeps the mainland practices, then only
the cells with a travel time to every one of them, so that no populated
cell has an unknown travel time.

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
grid_data
```

``` spax-output
<spax data> raster 117 x 187 cells x 54 facilities; supply gp_fte_extg
  demand    1 layer; travel 54 layers
  note      holds terra rasters: prepare the data again after saveRDS()
```

`[[gp$practice]]` picks the travel layers of the mainland practices by
name, in the table’s order. `sum(travel_grid)` is `NA` wherever any
layer is `NA`, so the mask keeps exactly the cells with complete travel.
[`trim()`](https://rspatial.github.io/terra/reference/trim.html) drops
the empty margin the islands leave behind, and
[`crop()`](https://rspatial.github.io/terra/reference/crop.html) cuts
the travel stack to the trimmed grid.

The mainland grid holds 541,184 residents, against 542,666 in the full
grid. As the print says, `grid_data` points to the rasters rather than
copying them: run the recipe again in a new session instead of saving
it.

### Small areas, for the allocation model

The allocation model gives each small area’s residents a probability of
registering at each practice, or with the outside option: a practice
outside the data. It runs on numeric inputs named by ID: demand (the
people of each small area who may register), supply by practice and the
travel matrix between them. Demand is the registered residents of each
area, so it counts the same people as the registrations. The mainland
rows and columns of `cornwall_travel` have no unknown times. The labels
in `metadata` record units and dates with the data. spax keeps them with
every result and does not convert them.

``` r

areas <- cornwall_lsoa |>
  filter(!scilly) |>
  st_drop_geometry()
gp <- cornwall_gp |>
  filter(!scilly) |>
  st_drop_geometry()
travel <- cornwall_travel[areas$lsoa, gp$practice]
demand <- setNames(areas$registered, areas$lsoa)
supply <- setNames(gp$gp_fte_extg, gp$practice)
d <- prep_data(demand, supply, travel, metadata = list(
  demand_units = "registered residents", demand_period = "July 2026",
  supply_units = "GP FTE excluding GPs in training", supply_period = "June 2026",
  travel_units = "minutes"))
flows <- obs_flows(cornwall_flows, origin = "lsoa", facility = "practice", count = "patients")
d
```

``` spax-output
<spax data> 336 origins (336 active) x 54 facilities
  demand      604,471 registered residents (July 2026)
  supply      GP FTE excluding GPs in training (June 2026)
  travel      minutes; unknown for 0 active origins
  policies    missing demand: error; missing travel: error
```

``` r

flows
```

``` spax-output
<spax obs> flows: 3,188 origin-facility counts, total 601,269; loss poisson
```

`d` holds 336 small areas (origins) by 54 practices (facilities), all
active, that is, with demand above zero. Their demand is larger than
their registrations at mainland practices by 5,497 residents: those
registered elsewhere, whom the outside option stands for. `flows` names
its loss, Poisson, which is how a fit compares modeled with observed
counts.

[`obs_flows()`](https://songyosr.github.io/spax/reference/obs_flows.md)
declares the observed registrations from the long table, which still
includes the Isles of Scilly. When
[`spax()`](https://songyosr.github.io/spax/reference/spax.md) fits a
model, it matches the counts to the data and says in a message which
counts it sets aside. [Bringing your own
data](https://songyosr.github.io/spax/articles/data-prep.html#observed-counts-in-a-long-table)
explains that message.

## Sources, dates and licenses

| Data | Objects | Source and date | License |
|:---|:---|:---|:---|
| Registrations | `cornwall_gp`, `cornwall_lsoa`, `cornwall_flows` | NHS England, Patients Registered at a GP Practice, July 2026 | Open Government Licence v3.0 |
| GP workforce | `cornwall_gp` | NHS England, General Practice Workforce, 30 June 2026 | Open Government Licence v3.0 |
| Practice locations | `cornwall_gp` | ONS Postcode Directory, November 2025 | Open Government Licence v3.0; contains OS and Royal Mail data |
| Boundaries, centroids, population | `cornwall_lsoa` | ONS LSOA 2021 boundaries and rural-urban classification, population-weighted centroids, mid-2024 population estimates | Open Government Licence v3.0; contains OS data |
| Population grid | `cornwall_pop.tif` | WorldPop, 2020 UN-adjusted 1 km estimates | Creative Commons Attribution 4.0 |
| Travel times | `cornwall_travel`, `cornwall_gp_travel.tif` | Derived by the package author from the Malaria Atlas Project 2019 motorized friction surface (Weiss et al. 2020) | Creative Commons Attribution (version 3.0 on the Malaria Atlas Project website, 4.0 in the Google Earth Engine catalogue; both permit sharing adapted data with attribution) |

Each source keeps the license given in the table. The package’s MIT
license covers its code only. The registrations and the workforce are
from 2026, the population grid from 2020, the small-area population from
mid-2024 and the road network from about 2019, so keep the dates in mind
when reading results. The full attributions, with the required wording
for each source, are installed with the package:

``` r

file.show(system.file("COPYRIGHTS", package = "spax"))
```

## Next

- [Getting started](https://songyosr.github.io/spax/articles/spax.md):
  compute and map a first accessibility index from the grid recipe.
- [Bringing your own
  data](https://songyosr.github.io/spax/articles/data-prep.md): line up
  IDs, unknown travel times and area totals in your own data.
- [Where do residents
  register?](https://songyosr.github.io/spax/articles/allocation-workflow.md):
  fit the allocation model to these registrations.
- [Reading
  results](https://songyosr.github.io/spax/articles/reading-results.md):
  what the registrations, the outside option and the indices can and
  cannot tell you.
