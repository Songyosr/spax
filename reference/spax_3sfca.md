# Calculate Three-Step Floating Catchment Area (3SFCA) accessibility scores

This experimental function implements the three-step floating catchment
area (3SFCA) method of Wan, Zou and Sternberg (2012). A selection step
precedes the two steps of
[`spax_e2sfca`](https://songyosr.github.io/spax/reference/spax_e2sfca.md):
it divides the demand at each origin among the facilities with selection
weights that sum to 1 at the origin. A facility then gathers only its
share of the demand at each origin, and its supply-to-demand ratio is
spread back to the origins with the same shares.

## Usage

``` r
spax_3sfca(
  demand,
  supply,
  distance,
  decay_params = list(method = "gaussian", sigma = 30),
  selection_params = decay_params,
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

  vector, matrix, or data.frame containing supply capacity values

- distance:

  SpatRaster stack of travel times/distances to facilities. With demand
  groups, either one stack shared by every group or a list of stacks
  named by the groups (for example one per travel mode); the stacks of
  all groups must name the same facilities in the same order and have
  the same geometry (reorder with `x[[names(first)]]`).

- decay_params:

  List of parameters for the decay weights used in both steps (with
  demand groups, one list shared by every group, or a list of such lists
  named by the groups):

  - method: "gaussian", "exponential", "power" or "binary", a custom
    decay function, or a kernel declared with
    [`decay_kernel`](https://songyosr.github.io/spax/reference/decay_kernel.md).
    Weights must be finite where the distance is finite, so power decay
    needs a positive minimum distance

  - sigma: decay parameter controlling the rate of distance decay

  - Additional parameters passed to custom decay functions

- selection_params:

  List of parameters, in the form of `decay_params`, for the decay
  weights that the selection weights are built from. The default is
  `decay_params`, so one decay both weights distance and selects
  facilities; give a different list to select with another decay. Its
  weights must also be finite where the distance is finite. With demand
  groups, shared or a list named by the groups; a group whose selection
  list is identical to its decay list reuses the decay weights.

- id_col:

  Character; column name for facility IDs if supply is a data.frame

- supply_cols:

  Character vector; names of supply columns if supply is a data.frame

- indicator_names:

  Character vector; custom names for output accessibility layers

- snap:

  Logical; if TRUE enable fast computation mode (default = FALSE)

## Value

A spax object (see
[`spax_e2sfca`](https://songyosr.github.io/spax/reference/spax_e2sfca.md))
whose `type` is "3SFCA". Its `parameters` are the named list
`decay_params`, `selection_params` (and `groups` and `layers` with
demand groups).

## Details

Write \\D_i\\ for the demand at origin \\i\\, \\S_j\\ for the supply of
facility \\j\\, \\W\_{ij}\\ for the decay weights from `decay_params`
and \\T\_{ij}\\ for the decay weights from `selection_params`. The two
are the same weights when `selection_params` is identical to
`decay_params`.

- Selection weights: \\G\_{ij} = T\_{ij} / \sum_k T\_{ik}\\.

- Ratio of facility \\j\\: \\R_j = S_j / \sum_i D_i W\_{ij} G\_{ij}\\.

- Accessibility of origin \\i\\: \\A_i = \sum_j R_j W\_{ij} G\_{ij}\\.

Both steps use the kernel \\K\_{ij} = W\_{ij} G\_{ij}\\. The result
equals
`compute_access(demand, supply, demand_weights = K, access_weights = K)`
with `K = W * calc_choice(T)`. There is no `demand_normalize` argument:
the selection weights already divide the demand of each origin among the
facilities.

The selection weights are normalized over every facility with a positive
weight at the origin, however far away it is. To restrict the selection
to a catchment, set the travel times beyond the limit to `NA` or `Inf`
before calling the function, or use a decay method that is zero beyond
it, for example `method = "binary"` in `selection_params`.

Because both steps use the same kernel, the scores conserve supply: the
sum of \\D_i A_i\\ over the origins with demand and a score equals the
total supply of the facilities that gather demand (\\\sum_i D_i K\_{ij}
\> 0\\). A facility that gathers no demand has a ratio of zero. Several
supply measures give independent accessibility layers.

## Demand groups

Give `demand` as a named list of single-layer rasters (for example age
bands) to let several groups compete for the same facilities. The names
are the group IDs: unique, non-empty, not `"all"` and without a `.`; all
rasters need the same geometry. A multi-layer raster is not taken as
groups.

Step 1 sums, over all groups, the demand that reaches each facility,
with the demand weights of each group; step 2 gives each group its own
accessibility from the shared ratios, with its own weights. The result
has one layer per group, and an overall layer `<measure>.all`: the
average of the group scores weighted by the demand of the cell. A group
with demand in a cell that its travel cannot reach counts with
accessibility 0 in the overall layer; the overall layer is NA where any
group's demand is missing or the total demand is 0. With one group the
group layer equals the ungrouped result.

`decay_params` and `distance` (and `selection_params` of `spax_3sfca()`
and
[`spax_huff2sfca()`](https://songyosr.github.io/spax/reference/spax_huff2sfca.md),
`threshold` of
[`spax_2sfca()`](https://songyosr.github.io/spax/reference/spax_2sfca.md))
are each either shared by every group or given per group. An argument is
per group when it is a list (for `threshold`, a named numeric vector)
whose names are the group IDs, in any order; names that match only some
of the groups are refused. Any other value is shared and checked as for
one group. Pass a shared `distance` as one object, so that it is read
once. A plain-function decay builds a whole in-memory weight raster for
each group. `demand_normalize`, `indicator_names`, `id_col`,
`supply_cols` and the attractiveness of
[`spax_huff2sfca()`](https://songyosr.github.io/spax/reference/spax_huff2sfca.md)
are shared.

## References

Wan, N., Zou, B., & Sternberg, T. (2012). A three-step floating
catchment area method for analyzing spatial access to health services.
*International Journal of Geographical Information Science*, *26*(6),
1073-1089.
[doi:10.1080/13658816.2011.624987](https://doi.org/10.1080/13658816.2011.624987)

## See also

[`spax_huff2sfca`](https://songyosr.github.io/spax/reference/spax_huff2sfca.md)
for selection weights that also depend on facility attractiveness,
[`spax_e2sfca`](https://songyosr.github.io/spax/reference/spax_e2sfca.md),
[`spax_m2sfca`](https://songyosr.github.io/spax/reference/spax_m2sfca.md),
[`compute_access`](https://songyosr.github.io/spax/reference/compute_access.md),
[`calc_decay`](https://songyosr.github.io/spax/reference/calc_decay.md),
[`calc_choice`](https://songyosr.github.io/spax/reference/calc_choice.md)
and
[`decay_kernel`](https://songyosr.github.io/spax/reference/decay_kernel.md).
[`vignette("fca-methods", package = "spax")`](https://songyosr.github.io/spax/articles/fca-methods.md)
compares the FCA methods on the Cornwall data.

## Examples

``` r
# Load example data
library(terra)
library(sf)

# Cornwall population grid and travel time to each GP practice
pop_rast <- read_spax_example("cornwall_pop.tif")
travel <- read_spax_example("cornwall_gp_travel.tif")

# Drop geometry for supply data
gp <- cornwall_gp |> st_drop_geometry()

# 3SFCA with Gaussian decay; the same decay gives the selection weights
result <- spax_3sfca(
  demand = pop_rast,
  supply = gp,
  distance = travel,
  decay_params = list(
    method = "gaussian",
    sigma = 10 # 10-minute characteristic travel time
  ),
  id_col = "practice",
  supply_cols = "gp_fte_extg"
)
plot(result$accessibility, main = "GP Accessibility (3SFCA)")


# Select among the practices within 15 minutes only
result_15 <- spax_3sfca(
  demand = pop_rast,
  supply = gp,
  distance = travel,
  decay_params = list(method = "gaussian", sigma = 10),
  selection_params = list(method = "binary", sigma = 15),
  id_col = "practice",
  supply_cols = "gp_fte_extg"
)

# Plot both for comparison
plot(c(result$accessibility, result_15$accessibility),
  main = c("Selection by decay", "Selection within 15 minutes")
)

```
