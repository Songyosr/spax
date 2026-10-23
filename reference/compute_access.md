# Calculate spatial accessibility using weighted surfaces

General-purpose function to calculate spatial accessibility by combining
demand-side and supply-side weighted distributions. Supports multiple
supply measures and flexible weight specifications.

## Usage

``` r
compute_access(
  demand,
  supply,
  demand_weights,
  access_weights,
  id_col = NULL,
  supply_cols = NULL,
  indicator_names = NULL,
  snap = FALSE
)
```

## Arguments

- demand:

  SpatRaster representing spatial distribution of demand, or a named
  list of single-layer SpatRasters, one per demand group (see "Demand
  groups"). A multi-layer SpatRaster is refused. Demand must be finite
  and nonnegative (use NA for unknown) unless `snap = TRUE`, which
  ignores other cells.

- supply:

  vector, matrix, or data.frame containing supply capacity values. If
  using an sf object, please use st_drop_geometry() first.

- demand_weights:

  Multi-layer SpatRaster of demand-side weights; values may be NA but
  not infinite. With demand groups, either one stack shared by every
  group or a list of stacks named by the groups (one per group); the
  stacks of all groups must name the same facilities in the same order
  and have the same geometry.

- access_weights:

  Multi-layer SpatRaster of accessibility-side weights; values may be NA
  but not infinite. Given per group in the same way as `demand_weights`.

- id_col:

  Character; column name for facility IDs if supply is a data.frame

- supply_cols:

  Character vector; names of supply columns if supply is a data.frame

- indicator_names:

  Character vector; custom names for output accessibility layers

- snap:

  Logical; if TRUE enable fast computation mode (default = FALSE)

## Value

SpatRaster of accessibility scores. With demand groups, the layers are
named `<measure>.<group>` for every group and `<measure>.all` for the
overall layer, measure by measure.

## Demand groups

When `demand` is a named list of single-layer rasters, the groups
compete for the same supply: the demand that reaches a facility is the
sum over the groups, each with its own demand weights. Each group gets
its own accessibility layer, and `<measure>.all` is the overall layer,
the average of the group scores weighted by the demand of the cell. A
group with demand in a cell that none of its weights reach counts with
accessibility 0 there; the overall layer is NA where any group's demand
is missing or the total demand is 0. Group names must be unique,
non-empty, not `"all"` and without a `.`. `demand_weights` and
`access_weights` are shared by the groups, or a list with one element
per group, named by the groups. Shared weights should be passed as one
object, so that they are read once.

## Examples

``` r
library(terra)

# Cornwall population grid, and Gaussian weights of the travel time to each
# GP practice
pop <- read_spax_example("cornwall_pop.tif")
travel <- read_spax_example("cornwall_gp_travel.tif")
gp <- sf::st_drop_geometry(cornwall_gp)
w <- calc_decay(travel, method = "gaussian", sigma = 10)

# The same weights in both steps give E2SFCA (spax_e2sfca()); the squared
# weights in the second step give M2SFCA (spax_m2sfca())
e2sfca <- compute_access(pop, gp, demand_weights = w, access_weights = w,
                         id_col = "practice", supply_cols = "gp_fte_extg",
                         indicator_names = "e2sfca")
m2sfca <- compute_access(pop, gp, demand_weights = w, access_weights = w^2,
                         id_col = "practice", supply_cols = "gp_fte_extg",
                         indicator_names = "m2sfca")
plot(c(e2sfca, m2sfca))
```
