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

- coverage_args:

  Named reporting controls for compact coverage, including optional
  `norm`, `units`, reporting floors and classification cuts. A norm must
  have the same declared supply-per-demand scale as capacity
  attribution.

- ...:

  Named controls passed to the existing fixed-parameter solver.

## Value

The existing `ae_equilibrium` with its portable prediction snapshot and
compact `coverage`. Coverage uses raw supply units without kappa;
proportional attributed capacity is not a delivered-service measure or a
hard constraint on expected utilization.

## See also

Other experimental allocation workflow:
[`bootstrap_allocation()`](https://songyosr.github.io/spax/reference/bootstrap_allocation.md),
[`calc_demand()`](https://songyosr.github.io/spax/reference/calc_demand.md),
[`calc_travel_time()`](https://songyosr.github.io/spax/reference/calc_travel_time.md),
[`check_allocation()`](https://songyosr.github.io/spax/reference/check_allocation.md),
[`clm_allocation()`](https://songyosr.github.io/spax/reference/clm_allocation.md),
[`compare_allocations()`](https://songyosr.github.io/spax/reference/compare_allocations.md),
[`decay_kernel()`](https://songyosr.github.io/spax/reference/decay_kernel.md),
[`fit_allocation()`](https://songyosr.github.io/spax/reference/fit_allocation.md),
[`predict_allocation()`](https://songyosr.github.io/spax/reference/predict_allocation.md),
[`prepare_allocation()`](https://songyosr.github.io/spax/reference/prepare_allocation.md)
