# Travel times to facilities over a friction surface

Computes least-cost travel time from each facility to every non-missing
cell of an analysis grid, by accumulating friction (time per unit
distance) along the cheapest path between neighboring cells with
[`terra::costDist()`](https://rspatial.github.io/terra/reference/costDist.html).
The result is continuous, with no isochrone bands, and can be passed
directly to
[`prepare_allocation()`](https://songyosr.github.io/spax/reference/prepare_allocation.md)
as its travel matrix.

## Usage

``` r
calc_travel_time(
  friction,
  facilities,
  template,
  ids,
  mask = NULL,
  buffer = 50000,
  max_time = Inf,
  method = c("bilinear", "average"),
  type = c("matrix", "raster")
)
```

## Arguments

- friction:

  A one-layer `SpatRaster` of positive travel time per unit of map
  distance (for example minutes per metre). Missing cells are barriers;
  zero or negative values are refused.

- facilities:

  An `sf` or `SpatVector` of facility points.

- template:

  A one-layer `SpatRaster` with square cells in a projected coordinate
  reference system with map units of metres. Its non-missing cells are
  the origins.

- ids:

  Name of the facility ID column. The IDs must be unique and nonmissing;
  they name the columns (or layers) of the result.

- mask:

  Optional polygons (`sf` or `SpatVector`); friction outside them is a
  barrier.

- buffer:

  Distance in map units by which the working grid extends beyond the
  template (default 50 km).

- max_time:

  Travel times above this value become unreachable: `Inf` in the matrix
  (an absent edge for
  [`prepare_allocation()`](https://songyosr.github.io/spax/reference/prepare_allocation.md))
  and `NA` in the raster.

- method:

  How friction is transferred to the template lattice: `"bilinear"`
  (default) when the template is about as fine as the friction surface,
  or `"average"` when it is coarser, so that fast cells such as roads
  are averaged rather than sampled.

- type:

  `"matrix"` (default) for an origin-by-facility matrix whose row names
  are template cell numbers, or `"raster"` for one layer per facility.

## Value

For `type = "matrix"`, a numeric matrix with a `cell_index` attribute
giving the template cell of each row, so that
`spatial = list(template = template, cell_index = attr(x, "cell_index"))`
can be passed to
[`prepare_allocation()`](https://songyosr.github.io/spax/reference/prepare_allocation.md).
For `type = "raster"`, a `SpatRaster` on the template grid.

## Details

The friction surface is projected to the grid's lattice, extended by
`buffer` so that routes can leave the analysis area, and cells outside
`mask` are treated as barriers (for example, to keep routes within one
country). Global motorized friction surfaces, such as those of the
Malaria Atlas Project, give minutes per metre; the travel times are then
in minutes. The function downloads nothing.

Times are measured between cell centers; the facility's own cell has
time zero. Paths that leave the facility cell diagonally are
underestimated by at most about a fifth of one cell crossing. Because
paths move between neighboring cells in eight directions, travel over
uniform friction exceeds the straight-line time by up to about 8%.

## See also

Other experimental allocation workflow:
[`bootstrap_allocation()`](https://songyosr.github.io/spax/reference/bootstrap_allocation.md),
[`calc_demand()`](https://songyosr.github.io/spax/reference/calc_demand.md),
[`check_allocation()`](https://songyosr.github.io/spax/reference/check_allocation.md),
[`clm_allocation()`](https://songyosr.github.io/spax/reference/clm_allocation.md),
[`compare_allocations()`](https://songyosr.github.io/spax/reference/compare_allocations.md),
[`decay_kernel()`](https://songyosr.github.io/spax/reference/decay_kernel.md),
[`evaluate_allocation()`](https://songyosr.github.io/spax/reference/evaluate_allocation.md),
[`fit_allocation()`](https://songyosr.github.io/spax/reference/fit_allocation.md),
[`huff_allocation()`](https://songyosr.github.io/spax/reference/huff_allocation.md),
[`predict_allocation()`](https://songyosr.github.io/spax/reference/predict_allocation.md),
[`prepare_allocation()`](https://songyosr.github.io/spax/reference/prepare_allocation.md)

## Examples

``` r
library(sf)
#> Linking to GEOS 3.14.1, GDAL 3.12.2, PROJ 9.7.1; sf_use_s2() is TRUE
library(terra)

# Cornwall population grid in British National Grid (metres)
pop <- project(read_spax_example("cornwall_pop.tif"), "EPSG:27700")

# A uniform friction surface of 50 km/h, in minutes per metre
friction <- init(pop, 60 / 50000)

# Minutes from five practices to every populated cell
gp <- cornwall_gp[1:5, ]
travel <- calc_travel_time(friction, gp, pop, ids = "practice", buffer = 5000)
summary(travel[, 1])
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#>    0.00   25.54   45.03   47.18   67.17  134.35 
```
