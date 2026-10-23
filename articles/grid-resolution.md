# Grid resolution and computational cost

Does a finer grid give a better accessibility index, and what does it
cost? A raster gives every place explicit geometry and lines all inputs
up cell by cell. Its cost grows with the number of cells times the
number of facilities, while the result can be no better than the demand
and travel data behind it. This guide runs the enhanced two-step
floating catchment area (E2SFCA) index of [Getting
started](https://songyosr.github.io/spax/articles/spax.md), which gives
each place the GP supply per resident within reach, with weights that
fall with travel time. It runs the index at the native resolution of the
bundled grids, at two coarser resolutions and at one finer resolution,
and compares results, size and run time. By the end you can estimate the
memory a grid needs and check whether a coarser grid would change your
answer. The inputs are the mainland grids of [The Cornwall GP example
data](https://songyosr.github.io/spax/articles/cornwall-data.md). The
[`spax()`](https://songyosr.github.io/spax/reference/spax.md) interface
and the allocation model are experimental and may change in later
versions.

``` r

library(spax)
library(terra)
library(sf)
library(dplyr)
```

## The inputs

The setup code shared by every grid guide, explained in [The Cornwall GP
example
data](https://songyosr.github.io/spax/articles/cornwall-data.md), reads
the population grid (residents per cell), the mainland practices with
their GP full-time equivalents (FTE), and one layer of travel minutes
per practice.

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

`grid_data` is the native-resolution input. Below, `index_at()` prepares
each resolution the same way.

## What one cell-by-practice array costs

The travel grid holds one value per cell and practice, and the decay
weights hold another array of the same size. The numeric payload of one
such array is cells times practices times 8 bytes.

``` r

cells <- c(grid = ncell(pop), populated = sum(!is.na(values(pop))))

# 8 bytes per value, one value per cell and practice
data.frame(
  cells,
  practices = nrow(gp),
  MiB_per_array = round(cells * nrow(gp) * 8 / 1024^2, 1)
)
```

``` spax-output
          cells practices MiB_per_array
grid      21879        54           9.0
populated  6912        54           2.8
```

The rectangular grid has 3.2 times as many cells as its populated part,
because it also covers sea and land outside Cornwall. That is small
here. The same arithmetic at 100 meters, or with hundreds of facilities,
reaches gigabytes. spax reads the rasters in blocks of rows when it
computes an FCA index, so it needs less memory than holding these arrays
whole. Measure memory and run time on the whole workflow:
[`object.size()`](https://rdrr.io/r/utils/object.size.html) of a terra
raster reports only a pointer, not the cell values.

## Change the resolution

Demand counts can be added up exactly when cells are merged. Travel
cannot: the bundled grids do not contain the road network, so a coarse
cell’s travel time has to be approximated from its fine cells. Here we
use the resident-weighted mean of the fine-cell minutes. Averaging
travel before a nonlinear decay does not give averaged weights, so this
is one explicit approximation. For a real study, recompute travel at the
target resolution if you can.

A factor of 2 merges each 2 x 2 block of cells into one, 4 merges 4 x 4
blocks, and 0.5 splits each cell into four. The four subcells of a split
cell each get a quarter of its residents and keep its travel times. This
adds cells but no information.

``` r

# Demand and travel grids at `factor` times the native cell width
at_resolution <- function(factor) {
  # Native grid: unchanged
  if (factor == 1) return(list(demand = pop, travel = travel_grid))

  # Finer grid: split residents evenly, copy the parent's travel times
  if (factor < 1) {
    split <- round(1 / factor)
    return(list(
      demand = disagg(pop, split) / split^2,
      travel = disagg(travel_grid, split)
    ))
  }

  # Coarser grid: add up residents, take resident-weighted mean minutes
  demand <- aggregate(pop, factor, fun = "sum", na.rm = TRUE)
  minutes <- aggregate(travel_grid * pop, factor, fun = "sum", na.rm = TRUE) /
    demand
  list(demand = demand, travel = minutes)
}

factors <- c(0.5, 1, 2, 4)
grids <- lapply(factors, at_resolution)
```

Now time the E2SFCA at each resolution, with the Gaussian decay of the
other guides: a travel scale σ of 10 minutes, at which a practice 10
minutes away keeps 61% of its weight. The timings are elapsed seconds on
the machine that built this guide, the median of three runs. Read them
as relative costs.

``` r

e2sfca_spec <- spec_e2sfca(decay = decay_gaussian(sigma = 10))

# The E2SFCA index on one resolution's grids
index_at <- function(grid) {
  resolution_data <- prep_data(
    grid$demand,
    gp,
    grid$travel,
    id_col = "practice",
    supply_cols = "gp_fte_extg"
  )
  spax(resolution_data, e2sfca_spec)
}

# Median elapsed seconds of three runs at each resolution
seconds <- sapply(grids, function(grid) {
  median(replicate(3, system.time(index_at(grid))[["elapsed"]]))
})

results <- lapply(grids, index_at)
```

To compare the results, take each surface back to the native grid: every
native cell gets the value of the cell that contains it. Then compare
cell by cell, weighting by native residents.

``` r

# The native surface and residents, one value per native cell
native <- 1000 * values(results[[2]]$accessibility, mat = FALSE)
residents <- values(pop, mat = FALSE)

# One table row per resolution
summary_row <- function(level) {
  # This surface on the native grid
  on_native <- resample(results[[level]]$accessibility, pop, method = "near")
  on_native <- 1000 * values(on_native, mat = FALSE)

  # Residents and cell area on this resolution's own grid
  demand <- values(grids[[level]]$demand, mat = FALSE)
  area <- values(cellSize(grids[[level]]$demand, unit = "km"), mat = FALSE)

  # Index per 1,000 on its own grid, and the change from the native surface
  own <- 1000 * values(results[[level]]$accessibility, mat = FALSE)
  change <- abs(on_native / native - 1)

  data.frame(
    factor = factors[level],
    cell_km2 = round(mean(area[!is.na(demand)]), 2),
    cells = sum(!is.na(demand)),
    seconds = seconds[level],
    mean_per_1000 = round(weighted.mean(own, demand, na.rm = TRUE), 3),
    median_change_pct = round(
      100 * median(change[!is.na(residents)], na.rm = TRUE),
      1
    ),
    mean_change_pct = round(
      100 * weighted.mean(change, residents, na.rm = TRUE),
      1
    )
  )
}

comparison <- bind_rows(lapply(seq_along(factors), summary_row))
comparison
```

``` spax-output
  factor cell_km2 cells seconds mean_per_1000 median_change_pct mean_change_pct
1    0.5     0.14 27648   0.528         0.575               0.0             0.0
2    1.0     0.55  6912   0.069         0.575               0.0             0.0
3    2.0     2.20  1829   0.015         0.575               1.8             1.4
4    4.0     8.79   506   0.011         0.575               3.3             2.5
```

`cells` counts the populated cells, and `cell_km2` is their mean area in
square kilometers. The native cells are 30 arc-seconds, often called 1
km: about 0.93 km north to south but only about 0.59 km east to west at
this latitude. The two change columns give the median and the
resident-weighted mean of the absolute percentage change from the native
surface, over native cells.

Some things stay fixed. Every resolution keeps the same 541,184
residents, and the resident-weighted mean index (`mean_per_1000`) stays
at total GP FTE per 1,000 residents, because both FCA steps use the same
weights. That identity says nothing about local accuracy.

What changes:

- **Finer cells (factor 0.5)** give 4 times as many populated cells and
  take 7.7 times as long, yet reproduce the native surface exactly (a
  change of 0%): no information was added.
- **Coarser cells** are cheaper: the 4 x 4 grid runs 6.3 times as fast
  as the native one, though on a grid this small every run takes a
  fraction of a second. The local pattern changes by 1.4% to 2.5% for
  the average resident. A 10-minute decay is wide compared with cells of
  2.2 to 8.8 square kilometers, which likely keeps the change small.
  Repeat the comparison for a shorter travel scale or a binary
  catchment.

The maps put the native surface and the 4 x 4 surface on one color
scale.

``` r

# The native surface and the 4 x 4 surface, both on the native grid
pair <- c(
  1000 * results[[2]]$accessibility,
  resample(1000 * results[[4]]$accessibility, pop, method = "near")
)
names(pair) <- c("Native grid", "Aggregated 4 x 4")
# Mainland Cornwall's outline, in the grid's longitude and latitude
outline <- cornwall_lsoa |>
  filter(!scilly) |>
  st_union() |>
  vect()

plot(
  pair,
  axes = FALSE,
  col = hcl.colors(50, "viridis"),
  range = range(values(pair), na.rm = TRUE),
  mar = c(1, 1, 2, 6),
  plg = list(title = "GP FTE\nper 1,000"),
  fun = function() lines(outline, lwd = 0.5)
)
```

![Two raster maps of mainland Cornwall on one color scale in GP FTE per
1,000 residents: the native grid on the left and the grid aggregated 4 x
4 on the right; the coarse map keeps the broad pattern, highest in the
west between Redruth, Truro and Falmouth, but in larger blocks that blur
local detail such as the low values on Bodmin
Moor.](grid-resolution_files/figure-html/maps-1.png)

The coarse surface keeps the broad pattern but blurs the local detail,
including the low values on Bodmin Moor, the upland north-east of the
map’s center. Its blocks also cover cells that the native grid leaves
empty at the coast.

## The allocation model

The allocation model of [Where do residents
register?](https://songyosr.github.io/spax/articles/allocation-workflow.md)
estimates the share of each place’s residents who register with each
practice. On the small areas, its travel matrix of areas by practices is
small. On raster data every populated cell becomes an origin, a place
residents start from, so the matrix is the cell-by-practice array above.
In the large example of
[`?prep_data`](https://songyosr.github.io/spax/reference/prep_data.md),
a fit to practice totals from one start peaks at about 8 times that
matrix, and the help page gives its run time too. Read only the tables
you need: `as.data.frame(result, type = "facilities")` (one row per
practice) or `type = "origins"` (one row per origin) is far smaller than
`type = "flows"`, which has a row for every origin and practice with a
positive flow. The bootstrap of [How sure are the
estimates?](https://songyosr.github.io/spax/articles/facility-bootstrap.md)
refits the model for every draw, so its run time grows with the number
of draws.

## Next

- [Bringing your own
  data](https://songyosr.github.io/spax/articles/data-prep.md): spread
  small-area counts to a demand grid at the resolution you choose.
- [Inside the E2SFCA
  calculation](https://songyosr.github.io/spax/articles/e2sfca-steps.md):
  why the calculation has to cover the whole study area.
- [Extending the
  model](https://songyosr.github.io/spax/articles/extending.md): fitting
  the allocation model on raster data and mapping it on the grid.
- [An FCA index or an allocation
  model?](https://songyosr.github.io/spax/articles/model-choice.md):
  when to fit a model to where people actually go instead.
