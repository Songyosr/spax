# Prepare checked numeric inputs for an experimental allocation workflow

Named axes align independently; unnamed axes are positional. Positive
included demand enters facility-load support. Zero, unknown and
explicitly excluded origins retain separate support statuses. This
experimental entry point delegates to the existing compact preparation
boundary.

## Usage

``` r
prepare_allocation(
  demand,
  supply,
  distance,
  origin_ids = NULL,
  facility_ids = NULL,
  included = NULL,
  missing_demand = c("error", "exclude"),
  missing_travel = c("error", "legacy_zero"),
  spatial = NULL,
  metadata = list()
)
```

## Arguments

- demand:

  Numeric origin vector, finite and nonnegative or explicitly permitted
  missing values. Negative or infinite demand is rejected even on
  excluded origins. Named demand matches canonical origin IDs. For
  demand groups, a named list by group of such named vectors with
  identical name sets (any order); the group names must be unique,
  non-empty, not `NA`, not `"all"` and without `"."`. A matrix is
  refused.

- supply:

  Finite nonnegative facility vector. Known zero supply is retained;
  unknown supply is rejected. Names match canonical facility IDs.

- distance:

  Numeric origin-by-facility travel matrix. Complete named rows and
  columns match IDs independently; unnamed axes are positional. With
  demand groups, one matrix shared by the groups or a named list of
  matrices with one element per group, each aligned by origin and
  facility IDs. Missing travel for a group with zero demand at an origin
  is ignored.

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

## Value

The existing `ae_prepared_inputs`, with a full origin registry and
distinct support statuses plus compact active load arrays. No duplicate
full travel matrix is retained. Pass it to
[`clm_allocation()`](https://songyosr.github.io/spax/reference/clm_allocation.md)
rather than editing its internal fields. Expected counts use demand
units/period; proportional capacity attribution uses supply per unit of
demand. With demand groups the total demand is `D_active`, and
`group_ids`, `D_groups` and `distance_groups` are added
(`distance_active` is `NULL` when travel is given per group).

## Details

Demand groups (age bands, insurance types) are given as a named list of
demand vectors, one per group, with the same origin IDs. An origin is
active when it is included, every group demand is known and the total is
positive; an origin with a group demand missing is unknown as a whole
under `missing_demand = "exclude"`. The prepared object then also
carries the group IDs and the demand and travel by group (`D_groups`,
`distance_groups`).

## See also

Other experimental allocation workflow:
[`bootstrap_allocation()`](https://songyosr.github.io/spax/reference/bootstrap_allocation.md),
[`calc_demand()`](https://songyosr.github.io/spax/reference/calc_demand.md),
[`calc_travel_time()`](https://songyosr.github.io/spax/reference/calc_travel_time.md),
[`check_allocation()`](https://songyosr.github.io/spax/reference/check_allocation.md),
[`clm_allocation()`](https://songyosr.github.io/spax/reference/clm_allocation.md),
[`compare_allocations()`](https://songyosr.github.io/spax/reference/compare_allocations.md),
[`decay_kernel()`](https://songyosr.github.io/spax/reference/decay_kernel.md),
[`evaluate_allocation()`](https://songyosr.github.io/spax/reference/evaluate_allocation.md),
[`fit_allocation()`](https://songyosr.github.io/spax/reference/fit_allocation.md),
[`huff_allocation()`](https://songyosr.github.io/spax/reference/huff_allocation.md),
[`predict_allocation()`](https://songyosr.github.io/spax/reference/predict_allocation.md)
