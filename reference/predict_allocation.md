# Predict requested origins against a saved fitted or evaluated CLM

Numeric output preserves the existing prediction result and its separate
probability, demand and capacity validity. Raster conversion is explicit
and permits one vector only; inspect numeric validity before
interpreting a map. Supply/theta overrides are deliberately absent. A
changed system requires reevaluation.

## Usage

``` r
predict_allocation(
  source,
  distance,
  origin_ids = rownames(distance),
  demand = NULL,
  outputs = c("rho", "A", "abar"),
  missing_travel = c("unknown", "legacy_zero"),
  spatial = NULL,
  type = c("numeric", "raster")
)
```

## Arguments

- source:

  A selected multistart result, NFXP fit or fixed evaluation with a
  valid CLM prediction snapshot, for a built-in decay kernel or one
  declared with
  [`decay_kernel()`](https://songyosr.github.io/spax/reference/decay_kernel.md).
  Older objects need regeneration. The snapshot of a declared kernel
  carries its code and predicting runs it, so use saved results only
  from sources you trust.

- distance:

  Numeric requested-origin-by-facility travel matrix with complete
  facility column IDs. Named axes align independently; requested origin
  IDs may define a new domain outside the fitted load support. Values
  must use the saved travel unit; no unit conversion is performed.

- origin_ids:

  Complete unique nonblank requested origin IDs, defaulting to matrix
  row names. Returned rows follow this declared order.

- demand:

  Optional requested numeric demand, independently aligned by names or
  positional in request order. Zero is known zero; missing or omitted
  values stay unknown. It multiplies requested flow only and never
  changes saved system utilization. Use the saved demand unit and
  reference period; returned metadata is inherited without checking or
  converting units.

- outputs:

  Exact requested vectors or matrices. Vectors include rho,
  outside_share, A, abar, unsupported_contact, A_supported and
  abar_supported; allocation and flow are optional matrices. rho/outside
  are probabilities; flow has demand count units/period. A and abar have
  supply-per-demand units.

- missing_travel:

  Default `"unknown"` makes a normalized row unknown when any
  alternative's travel is unknown. Explicit `"legacy_zero"` preserves
  its imputation flags; positive infinity remains known absence, while
  negative travel and invalid finite kernel domains are rejected.

- spatial:

  Optional one-layer raster template plus separately named `cell_index`
  mapping for requested IDs, as in preparation. Saved metadata contains
  plain geometry. Without a mapping, numeric prediction still works.

- type:

  `"numeric"` retains selected values, all separate validity flags,
  policies and unit declarations. `"raster"` explicitly returns one
  vector map, so inspect the numeric validity result before interpreting
  the map.

## Value

An `ae_clm_prediction`, or an explicitly requested one-layer raster.
Contact to positive-supply/zero-load facilities preserves valid
probabilities but makes unqualified A/abar unknown; explicitly named
supported-only outputs retain the existing zero-contribution convention.
Zero-denominator rows retain guarded zero mass and a false
probability-valid flag.

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
[`prepare_allocation()`](https://songyosr.github.io/spax/reference/prepare_allocation.md)
