# Calculate Original Two-Step Floating Catchment Area (2SFCA) accessibility scores

Implements the original Two-Step Floating Catchment Area (2SFCA) method
using binary catchment areas, as proposed by Luo & Wang (2003). This
foundational method uses a single distance/time threshold to define
service areas and computes accessibility as a ratio of supply to demand
within these catchments.

## Usage

``` r
spax_2sfca(
  demand,
  supply,
  distance,
  threshold,
  id_col = NULL,
  supply_cols = NULL,
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
  groups, one stack shared by every group or a list of stacks named by
  the groups (see
  [`spax_e2sfca()`](https://songyosr.github.io/spax/reference/spax_e2sfca.md)).

- threshold:

  Numeric value defining the catchment area cutoff (same units as
  distance). With demand groups, one number shared by every group or a
  numeric vector named by the groups, one positive finite number per
  group.

- id_col:

  Character; column name for facility IDs if supply is a data.frame

- supply_cols:

  Character vector; names of supply columns if supply is a data.frame

- snap:

  Logical; if TRUE enable fast computation mode (default = FALSE)

## Value

A spax object containing:

- accessibility: SpatRaster of accessibility scores (with demand groups,
  named as in
  [`spax_e2sfca()`](https://songyosr.github.io/spax/reference/spax_e2sfca.md))

- type: Character string "2SFCA"

- parameters: List containing threshold and binary decay parameters (and
  `groups` and `layers` with demand groups)

- facilities: data.frame of facility-level information

- call: The original function call

## Details

The original Two-Step Floating Catchment Area (2SFCA) method operates in
two steps:

Step 1: For each facility j:

- Define a catchment area within threshold distance/time

- Sum the population of all demand locations i within the catchment

- Calculate supply-to-demand ratio Rj = Sj/sum(Pi)

Step 2: For each demand location i:

- Define a catchment area within threshold distance/time

- Sum all facility ratios Rj within the catchment

- Final accessibility score Ai = sum(Rj)

Key characteristics:

1.  Binary catchment areas (within threshold = 1, beyond = 0)

2.  Equal weights for all locations within catchment

3.  No normalization of demand weights

4.  Single threshold value for both steps

Limitations addressed by later methods:

- No distance decay within catchments

- Artificial barriers at catchment boundaries

- Potential demand overestimation in overlapping areas

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

`decay_params` and `distance` (and `selection_params` of
[`spax_3sfca()`](https://songyosr.github.io/spax/reference/spax_3sfca.md)
and
[`spax_huff2sfca()`](https://songyosr.github.io/spax/reference/spax_huff2sfca.md),
`threshold` of `spax_2sfca()`) are each either shared by every group or
given per group. An argument is per group when it is a list (for
`threshold`, a named numeric vector) whose names are the group IDs, in
any order; names that match only some of the groups are refused. Any
other value is shared and checked as for one group. Pass a shared
`distance` as one object, so that it is read once. A plain-function
decay builds a whole in-memory weight raster for each group.
`demand_normalize`, `indicator_names`, `id_col`, `supply_cols` and the
attractiveness of
[`spax_huff2sfca()`](https://songyosr.github.io/spax/reference/spax_huff2sfca.md)
are shared.

## References

Luo, W., & Wang, F. (2003). Measures of Spatial Accessibility to Health
Care in a GIS Environment: Synthesis and a Case Study in the Chicago
Region. *Environment and Planning B: Planning and Design*, *30*(6),
865-884. https://doi.org/10.1068/b29120

## See also

- [`spax_e2sfca()`](https://songyosr.github.io/spax/reference/spax_e2sfca.md)
  for the enhanced version with distance decay

- [`compute_access()`](https://songyosr.github.io/spax/reference/compute_access.md)
  for more flexible accessibility calculations

## Examples

``` r
# Load example data
library(terra)
library(sf)

# Load data
pop_rast <- read_spax_example("cornwall_pop.tif")
travel <- read_spax_example("cornwall_gp_travel.tif")

# Drop geometry for supply data
gp <- cornwall_gp |> st_drop_geometry()

# Calculate accessibility to GPs with a 15-minute catchment
result <- spax_2sfca(
  demand = pop_rast,
  supply = gp,
  distance = travel,
  threshold = 15, # 15-minute catchment
  id_col = "practice",
  supply_cols = "gp_fte_extg"
)

# Plot the results
plot(result$accessibility, main = "GP Accessibility (Original 2SFCA)")


# Calculate accessibility for two supply measures
result_multi <- spax_2sfca(
  demand = pop_rast,
  supply = gp,
  distance = travel,
  threshold = 15,
  id_col = "practice",
  supply_cols = c("gp_fte_extg", "gp_fte")
)

# Compare accessibility for different supply measures
plot(result_multi$accessibility,
  main = c("GPs excluding trainees", "All GPs")
)

```
