# Evaluate a declared allocation model at supplied fixed parameters

Solves once and constructs compact coverage from the saved evaluated
outputs. The existing equilibrium result carries its prediction
snapshot; adding coverage does not retain the original model or create a
scenario type. The caller changes system supply/demand by preparing a
new model explicitly.

## Usage

``` r
evaluate_allocation(model, theta, coverage_args = list(), ...)
```

## Arguments

- model:

  A corrected CLM declared over checked prepared inputs.

- theta:

  A complete model parameter vector. Named parameters align to the model
  contract; unnamed values follow its declared order. This operation
  estimates no parameters and is not a causal intervention estimator.
  For a model with demand groups, a parameter held fixed by
  `clm_allocation(fixed = )` may be given only with its fixed value, and
  unknown names are an error.

- coverage_args:

  Named reporting controls for compact coverage, including optional
  `norm`, `units`, reporting floors and classification cuts. A norm must
  have the same declared supply-per-demand scale as capacity
  attribution.

- ...:

  Named controls passed to the existing fixed-parameter solver,
  including `requested_outputs`. For a model with demand groups, request
  the per-group outputs here, for example
  `requested_outputs = c("utilization_by_group", "access_by_group", "outside_share_by_group")`;
  the other outputs are pooled over the groups (demand-weighted). A
  group's own access and outside share are `NA` on rows where that group
  has no demand and its travel there is not usable.

  The expected counts that can be requested with `requested_outputs` are
  `flow` (origins by facilities), `flow_outside` (`flow` with one more
  column, `"(outside)"`, holding each origin's expected outside count
  `D_i * outside_share_i`), `utilization_outside` (`utilization` with
  one more element, `"(outside)"`, the expected outside count of all
  origins) and, with demand groups, `utilization_by_group` and
  `utilization_outside_by_group` (with one more row, `"(outside)"`, the
  expected outside count of each group). The outside counts are zero for
  a model without an outside option; they are the outputs that
  [`fit_allocation()`](https://songyosr.github.io/spax/reference/fit_allocation.md)
  fits to observed outside counts.

## Value

The existing `ae_equilibrium` with its portable prediction snapshot
(none for a model with demand groups, which cannot be predicted yet) and
compact `coverage`. Coverage uses raw supply units without kappa;
proportional attributed capacity is not a delivered-service measure or a
hard constraint on expected utilization.

## See also

Other allocation steps:
[`bootstrap_fit()`](https://songyosr.github.io/spax/reference/bootstrap_fit.md),
[`calc_demand()`](https://songyosr.github.io/spax/reference/calc_demand.md),
[`calc_travel_time()`](https://songyosr.github.io/spax/reference/calc_travel_time.md),
[`check_fit()`](https://songyosr.github.io/spax/reference/check_fit.md),
[`clm_allocation()`](https://songyosr.github.io/spax/reference/clm_allocation.md),
[`compare_fits()`](https://songyosr.github.io/spax/reference/compare_fits.md),
[`decay_kernel()`](https://songyosr.github.io/spax/reference/decay_kernel.md),
[`fit_allocation()`](https://songyosr.github.io/spax/reference/fit_allocation.md),
[`huff_allocation()`](https://songyosr.github.io/spax/reference/huff_allocation.md),
[`predict_allocation()`](https://songyosr.github.io/spax/reference/predict_allocation.md),
[`prepare_allocation()`](https://songyosr.github.io/spax/reference/prepare_allocation.md)
