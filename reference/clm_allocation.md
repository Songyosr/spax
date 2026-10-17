# Declare corrected static CLM over checked allocation inputs

Declare corrected static CLM over checked allocation inputs.

## Usage

``` r
clm_allocation(
  inputs,
  family = c("gaussian", "exponential", "power"),
  kappa = 1,
  beta = 1,
  v0 = 0,
  fit_v0 = FALSE,
  fit_beta = FALSE
)
```

## Arguments

- inputs:

  An `ae_prepared_inputs` object from checked preparation.

- family:

  Travel kernel: `"gaussian"`, `"exponential"`, `"power"`, or a
  declaration from
  [`decay_kernel()`](https://songyosr.github.io/spax/reference/decay_kernel.md).
  Gaussian sigma is a travel scale, exponential sigma a rate, and power
  sigma an exponent. Power requires strictly positive finite travel
  distances. A built-in declaration, such as `decay_kernel("gaussian")`,
  is the same as its string. For a declaration of your own function,
  sigma has the meaning the declaration gives it, the declaration's
  `lower` and `upper` bounds apply to sigma (starts, bounds and
  parameters outside them are refused, and sigma must be positive), and
  `zero_distance = FALSE` refuses a finite zero travel time in the
  prepared inputs.

- kappa:

  Positive supply scale used in attractiveness, not in reported raw
  supply or capacity attribution.

- beta:

  Positive supply elasticity in `(kappa * supply)^beta`.

- v0:

  Nonnegative outside-option weight, not an outside probability. With
  zero outside weight and zero inside opportunity, the existing guard
  returns zero mass; that row is not a normalized choice distribution.

- fit_v0, fit_beta:

  `TRUE` or `FALSE`: whether to promote these constants to fitted
  parameters.

## Value

The existing private `ae_problem` for corrected static CLM. Parameter
order is sigma, then v0 and beta when their flags are true.

## See also

Other experimental allocation workflow:
[`bootstrap_allocation()`](https://songyosr.github.io/spax/reference/bootstrap_allocation.md),
[`calc_demand()`](https://songyosr.github.io/spax/reference/calc_demand.md),
[`calc_travel_time()`](https://songyosr.github.io/spax/reference/calc_travel_time.md),
[`check_allocation()`](https://songyosr.github.io/spax/reference/check_allocation.md),
[`compare_allocations()`](https://songyosr.github.io/spax/reference/compare_allocations.md),
[`decay_kernel()`](https://songyosr.github.io/spax/reference/decay_kernel.md),
[`evaluate_allocation()`](https://songyosr.github.io/spax/reference/evaluate_allocation.md),
[`fit_allocation()`](https://songyosr.github.io/spax/reference/fit_allocation.md),
[`predict_allocation()`](https://songyosr.github.io/spax/reference/predict_allocation.md),
[`prepare_allocation()`](https://songyosr.github.io/spax/reference/prepare_allocation.md)
