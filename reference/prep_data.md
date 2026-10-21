# Prepare data for a spax model

Experimental. `prep_data()` checks and aligns the inputs of a model
once, so that model specifications can be bound to them with
[`spax()`](https://songyosr.github.io/spax/reference/spax.md). It has
two modes, chosen by `demand`.

## Usage

``` r
prep_data(
  demand,
  supply,
  travel,
  origin_ids = NULL,
  facility_ids = NULL,
  included = NULL,
  missing_demand = c("error", "exclude"),
  missing_travel = c("error", "legacy_zero"),
  spatial = NULL,
  metadata = list(),
  id_col = NULL,
  supply_cols = NULL
)

# S3 method for class 'ae_prepared_inputs'
format(x, ...)

# S3 method for class 'ae_prepared_inputs'
print(x, ...)

# S3 method for class 'spax_raster_data'
format(x, ...)

# S3 method for class 'spax_raster_data'
print(x, ...)
```

## Arguments

- demand:

  Numeric origin vector, or a named list of them for demand groups (see
  [`prepare_allocation()`](https://songyosr.github.io/spax/reference/prepare_allocation.md));
  or a `SpatRaster` of demand, or a named list of single-layer
  `SpatRaster`s, one per group. A list that mixes rasters and other
  elements is an error.

- supply:

  Numeric data: a finite nonnegative facility vector (see
  [`prepare_allocation()`](https://songyosr.github.io/spax/reference/prepare_allocation.md)).
  Raster data: a data frame with `id_col` and `supply_cols`, a matrix or
  a vector (see
  [`spax_e2sfca()`](https://songyosr.github.io/spax/reference/spax_e2sfca.md));
  several supply measures give independent layers in an FCA result.

- travel:

  Numeric origin-by-facility travel matrix. It is the `distance`
  argument of
  [`prepare_allocation()`](https://songyosr.github.io/spax/reference/prepare_allocation.md):
  the same alignment by IDs applies, and with demand groups it can be
  one matrix shared by the groups or a named list of matrices with one
  element per group. For raster data, a `SpatRaster` with one layer per
  facility (or a named list of them by demand group) on the grid of
  `demand`.

- origin_ids, facility_ids:

  Complete unique nonblank canonical IDs. Their order determines
  prepared order. The default (`NULL`) takes the names of the demand
  vector (of the first group for grouped demand) and the column names of
  the travel matrix; identities are never inferred from equal axis
  lengths.

- included:

  Nonmissing logical origin mask (default: all `TRUE`). Names align
  independently; an unnamed mask follows supplied demand order (that of
  the first group for grouped demand) and is reordered with demand.

- missing_demand:

  Default `"error"` rejects included missing demand. `"exclude"` omits
  it from loads while recording unknown demand, separately from known
  zero or explicit exclusion. Positive included demand remains active
  even when every inside edge is absent.

- missing_travel:

  Default `"error"` rejects NA/NaN on active fit rows. `"legacy_zero"`
  explicitly omits unknown edges and records that policy. Finite
  nonnegative travel is known, positive infinity is an absent edge, and
  negative travel is invalid. Omitted rows are validated when requested
  by prediction. Model-specific finite domains, such as positive power
  distances, are checked when binding parameters; no imputation bypasses
  them.

- spatial:

  Optional list with a one-layer
  [`terra::SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
  `template` and a complete unique in-range `cell_index` vector,
  optionally named by origin ID. Registry positions are not raster
  cells. Only plain geometry and the aligned mapping are saved. Numeric
  work needs no spatial metadata.

- metadata:

  Optional named list of scalar strings or NA for `demand_units`,
  `demand_period`, `supply_units`, `supply_period`, and `travel_units`.
  Omitted fields remain NA. These are caller declarations; no unit
  conversion or dimensional compatibility check is performed.

- id_col, supply_cols:

  Raster data with a data frame as `supply`: the column of facility IDs
  and the character vector of supply columns. They are refused for
  numeric data.

- x:

  Prepared inputs, an `ae_prepared_inputs` or `spax_raster_data`.

- ...:

  Unused.

## Value

Numeric data: the existing `ae_prepared_inputs`, as returned by
[`prepare_allocation()`](https://songyosr.github.io/spax/reference/prepare_allocation.md).
Raster data: a `spax_raster_data` (also a `spax_data`), a list with
`demand` and `travel` (as given), `supply` (the object as given),
`id_col`, `supply_cols`, `facility_ids` (the layer names of the travel
stack, or `facility_1`, ... by position), `groups` (the group IDs,
`NULL` without groups), `metadata` (the unit labels) and `grid` (a list
with `dim`, `res` and `crs` of the demand grid).
[`format()`](https://rdrr.io/r/base/format.html) returns the printed
lines as a character vector and
[`print()`](https://rdrr.io/r/base/print.html) returns its argument
invisibly.

## Details

**Numeric data** (`demand` is a numeric vector, or a named list of
numeric vectors for demand groups) are the inputs of an allocation model
([`spec_clm()`](https://songyosr.github.io/spax/reference/spec_clm.md),
[`spec_huff()`](https://songyosr.github.io/spax/reference/spec_clm.md)):
demand by origin, supply by facility and the origin-by-facility travel
matrix. In this mode `prep_data()` is
[`prepare_allocation()`](https://songyosr.github.io/spax/reference/prepare_allocation.md)
under another name, with `travel` for `distance`: it returns the
identical object, an `ae_prepared_inputs`, and every argument but
`id_col` and `supply_cols` is passed through. The step functions
([`clm_allocation()`](https://songyosr.github.io/spax/reference/clm_allocation.md)
and the others) accept the object too.

Printing the object lists its origins and facilities, the demand groups,
the unit labels when they were given, the active origins whose travel is
unknown and the policies for missing demand and travel.

**Raster data** (`demand` is a `SpatRaster`, or a named list of
single-layer `SpatRaster`s for demand groups) are the inputs of the
floating catchment area indices
([`spec_e2sfca()`](https://songyosr.github.io/spax/reference/spec_e2sfca.md)
and its relatives): `demand` is the demand grid, `travel` the travel
stack, one layer per facility named by the facility ID, and `supply` a
data frame with `id_col` and `supply_cols`, a matrix or a vector, as in
[`spax_e2sfca()`](https://songyosr.github.io/spax/reference/spax_e2sfca.md).
With demand groups, `travel` can be one stack or a named list of stacks,
one per group. `prep_data()` checks the structure (the demand groups,
one geometry, one layer per facility, the supply table) and reads no
value of a raster; the demand rule of the indices (finite and
nonnegative, `NA` for unknown) is applied when a model runs. The
arguments `origin_ids`, `facility_ids`, `included`, `missing_demand`,
`missing_travel` and `spatial` are for numeric data and are refused.
`id_col` and `supply_cols` are for raster data and are refused for
numeric data. The result holds the rasters as they are, so it does not
survive
[`saveRDS()`](https://rspatial.github.io/terra/reference/serialize.html):
its pointers to the rasters are then empty, and
[`spax()`](https://songyosr.github.io/spax/reference/spax.md) stops and
asks for the data to be prepared again.

**Allocation models on raster data.**
[`spax()`](https://songyosr.github.io/spax/reference/spax.md) also runs
[`spec_clm()`](https://songyosr.github.io/spax/reference/spec_clm.md)
and
[`spec_huff()`](https://songyosr.github.io/spax/reference/spec_clm.md)
on raster data. It converts the data to origins on each call, behind the
scenes, and keeps nothing: the origins are the cells with finite,
positive demand (with demand groups, the cells where every group's
demand is known and the total is positive), their IDs are the cell
numbers of the grid as text (`"843"`), and the origin registry holds
these cells only. The demand rule of the indices applies: negative or
infinite demand is an error and `NA` is unknown.
[`obs_totals()`](https://songyosr.github.io/spax/reference/obs_flows.md)
works directly.
[`obs_flows()`](https://songyosr.github.io/spax/reference/obs_flows.md)
is keyed by cell number, and counts at cells that are not origins (no
demand, or unknown) are dropped with a message. A missing or infinite
travel time is an absent pair, and a cell with demand that reaches no
facility stays an origin: with
[`spec_clm()`](https://songyosr.github.io/spax/reference/spec_clm.md)
its demand goes to the outside option, while
[`spec_huff()`](https://songyosr.github.io/spax/reference/spec_clm.md)
has no outside option and allocates none of it. The allocation model
uses one supply measure: with several `supply_cols`, choose one with
`prep_data(supply_cols = )`. The conversion is the same on every call,
so fits to the same data share their travel matrix and can be compared
with
[`compare_allocations()`](https://songyosr.github.io/spax/reference/compare_fits.md),
and a result does not depend on the rasters after the call.

The travel of the origins is held as a matrix of origins by facilities,
8 bytes per value. On a synthetic grid of 400 x 500 cells with demand in
half of them and 250 facilities (100,000 origins), the matrix is 200 MB;
converting takes 2 seconds and peaks at about 0.5 GB of R memory (0.85
GB of process memory above the data), 2.5 to 4 times the matrix. A fit
to facility totals from one start peaks at about 1.6 GB of R memory (2.6
GB of process memory), about 8 times the matrix, and takes about 2.5
minutes. That is the memory of the fit, the same as for numeric inputs
of that size, so a much larger grid is better cropped or coarsened
first.

**FCA indices on numeric data.** An FCA specification
([`spec_e2sfca()`](https://songyosr.github.io/spax/reference/spec_e2sfca.md)
and its relatives) also runs on numeric data with
[`spax()`](https://songyosr.github.io/spax/reference/spax.md). It gives
the index at the active origins only: an origin with zero or unknown
demand, or left out, has no row in the result. The result is a
`spax_access_values` (see
[spax_access_values-methods](https://songyosr.github.io/spax/reference/spax_access_values-methods.md)),
not rasters, and numeric data has one supply measure, `"supply"`. A
raster `sigma` (a variable catchment) needs raster data.

## See also

Other allocation interface:
[`decay_gaussian()`](https://songyosr.github.io/spax/reference/decay_gaussian.md),
[`obs_flows()`](https://songyosr.github.io/spax/reference/obs_flows.md),
[`spax()`](https://songyosr.github.io/spax/reference/spax.md),
[`spax_access_values-methods`](https://songyosr.github.io/spax/reference/spax_access_values-methods.md),
[`spax_allocation-methods`](https://songyosr.github.io/spax/reference/spax_allocation-methods.md),
[`spax_huff()`](https://songyosr.github.io/spax/reference/spax_huff.md),
[`spec_clm()`](https://songyosr.github.io/spax/reference/spec_clm.md)

## Examples

``` r
# Mainland Cornwall: registered residents, GP FTE and travel minutes
areas <- sf::st_drop_geometry(cornwall_lsoa[!cornwall_lsoa$scilly, ])
gp <- sf::st_drop_geometry(cornwall_gp[!cornwall_gp$scilly, ])
demand <- stats::setNames(areas$registered, areas$lsoa)
supply <- stats::setNames(gp$gp_fte_extg, gp$practice)
travel <- cornwall_travel[areas$lsoa, gp$practice]
prep_data(demand, supply, travel,
          metadata = list(demand_units = "registered residents",
                          supply_units = "GP FTE", travel_units = "minutes"))
#> <spax data> 336 origins (336 active) x 54 facilities
#>   demand      604,471 registered residents
#>   supply      GP FTE
#>   travel      minutes; unknown for 0 active origins
#>   policies    missing demand: error; missing travel: error

# Raster data for a floating catchment area index
pop <- read_spax_example("cornwall_pop.tif")
travel_grid <- read_spax_example("cornwall_gp_travel.tif")
grid_data <- prep_data(pop, sf::st_drop_geometry(cornwall_gp), travel_grid,
                       id_col = "practice", supply_cols = "gp_fte_extg")
grid_data
#> <spax data> raster 132 x 271 cells x 55 facilities; supply gp_fte_extg
#>   demand    1 layer; travel 55 layers
#>   note      holds terra rasters: prepare the data again after saveRDS()
spax(grid_data, spec_e2sfca(decay = decay_gaussian(sigma = 10)))
#> <spax access> E2SFCA, 1 layer on 132 x 271 cells
#> 
#> Spatial Properties:
#> - Resolution: 0.00833333 x 0.00833333
#> - Dimensions: 132 x 271
#> 
#> Measures: 1
#> - gp_fte_extg
#> 
#> Facilities: 55
#> 
#> Parameters:
#> - Decay function: gaussian (sigma = 10)
#> - Demand normalization: identity 

# \donttest{
# An allocation model on the mainland grid, fitted to practice totals. Demand
# must count the same people as the totals: residents registered with a GP
# practice. The grid estimates residents in 2020, fewer than the registered
# residents of the mainland small areas, so it is scaled to them and gives
# only where they live.
mainland <- terra::ext(-6, terra::xmax(pop), terra::ymin(pop), terra::ymax(pop))
pop <- terra::crop(pop, mainland)
registered <- pop * sum(areas$registered) /
  terra::global(pop, "sum", na.rm = TRUE)[[1]]
d_mainland <- prep_data(registered, gp,
                        terra::crop(travel_grid[[gp$practice]], mainland),
                        id_col = "practice", supply_cols = "gp_fte_extg")

# Registrations of mainland residents at the mainland practices
flows <- cornwall_flows[cornwall_flows$lsoa %in% areas$lsoa &
                          cornwall_flows$practice %in% gp$practice, ]
totals <- obs_totals(tapply(flows$patients, flows$practice, sum))

# The cells with residents become the origins. The Huff model (power decay,
# fitted attractiveness exponent) is well determined by these totals.
huff <- spax(d_mainland, spec_huff(), observed = totals)
huff
#> <spax allocation> Huff model (no outside option)
#>   decay       power (sigma = exponent): fit
#>   attraction  (kappa * supply)^beta: kappa = 1, beta = fit
#>   outside     none
#>   data        6,912 origins x 54 facilities
#> 
#>   parameter  value   status
#>   sigma      1.059   fitted
#>   beta       0.6123  fitted
#>   v0         0       held
#>   kappa      1       held
#> 
#>   selected    scan1 (start 1 of 3); 3 of 3 starts converged
#>   boundary    none
#>   starts      default search, 15 of 15 grid points, loss poisson

# What the totals say about travel: the fit against no decay
check_fit(huff)
#> <allocation check>
#>     comparator     loss loss_difference contact trip_median trip_p90
#>         fitted -5042045               0       1          11       53
#>       no_decay -5038257            3788       1          40       85
#>  capacity_only -5038282            3763      NA          40       85
#> Trip lengths are one-way, in travel units, weighted by expected flows.
#> 
#> The fitted allocation reproduces the counts better than both comparators.
# }
```
