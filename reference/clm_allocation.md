# Declare a static conditional logit allocation model

Declare a static conditional logit allocation model (CLM) with an
outside option on prepared allocation inputs.

## Usage

``` r
clm_allocation(
  inputs,
  family = c("gaussian", "exponential", "power"),
  kappa = 1,
  beta = 1,
  v0 = 0,
  fit_v0 = FALSE,
  fit_beta = FALSE,
  by_group = character(),
  fixed = NULL
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
  zero outside weight and zero inside opportunity, the model returns
  zero mass; that row is not a normalized choice distribution.

- fit_v0, fit_beta:

  `TRUE` or `FALSE`: whether to promote these constants to fitted
  parameters.

- by_group:

  For inputs with demand groups: the parameters that differ between the
  groups, a subset of `"sigma"` and `"v0"` (default none: one sigma and
  one v0 for all groups). `"v0"` requires `fit_v0 = TRUE`. The
  parameters are then named `sigma.<group>` and `v0.<group>`.

- fixed:

  For inputs with demand groups: a named numeric vector of parameters
  held fixed at the given values, named like the parameters of the model
  (for example `sigma.a` or `v0.b = 0`). They are not fitted and not
  part of `theta`.

## Value

The model: an `ae_problem` for the static CLM. Parameter order is sigma,
then v0 and beta when their flags are true. With demand groups sigma and
v0 are `sigma.<group>` and `v0.<group>` for the parameters in
`by_group`.

## Details

With demand groups (see
[`prepare_allocation()`](https://songyosr.github.io/spax/reference/prepare_allocation.md)),
group `g` chooses among the facilities with its own kernel and outside
weight, from its own travel times:
`P_ijg = a_j W_g(d_ijg) / (v0_g + sum_k a_k W_g(d_ikg))`. The
allocation, contact (`access`) and outside share of the model are the
demand-weighted pooled values, so facility load, coverage and flows are
the ordinary computations on the total demand. Per-group outputs
(`utilization_by_group`, `access_by_group`, `outside_share_by_group`)
are materialized when requested. With every parameter and the travel
shared by the groups, the model equals the ungrouped model on the total
demand. `family` may be given by group, a named list of kernel names or
declarations with one element per group (sigma is then group specific);
`v0` may be a named numeric vector by group when `fit_v0 = FALSE`.

## See also

Other allocation steps:
[`bootstrap_fit()`](https://songyosr.github.io/spax/reference/bootstrap_fit.md),
[`calc_demand()`](https://songyosr.github.io/spax/reference/calc_demand.md),
[`calc_travel_time()`](https://songyosr.github.io/spax/reference/calc_travel_time.md),
[`check_fit()`](https://songyosr.github.io/spax/reference/check_fit.md),
[`compare_fits()`](https://songyosr.github.io/spax/reference/compare_fits.md),
[`decay_kernel()`](https://songyosr.github.io/spax/reference/decay_kernel.md),
[`evaluate_allocation()`](https://songyosr.github.io/spax/reference/evaluate_allocation.md),
[`fit_allocation()`](https://songyosr.github.io/spax/reference/fit_allocation.md),
[`huff_allocation()`](https://songyosr.github.io/spax/reference/huff_allocation.md),
[`predict_allocation()`](https://songyosr.github.io/spax/reference/predict_allocation.md),
[`prepare_allocation()`](https://songyosr.github.io/spax/reference/prepare_allocation.md)

## Examples

``` r
# West Cornwall: the small areas and practices west of easting 170 km
areas <- sf::st_drop_geometry(cornwall_lsoa)
areas <- areas[!areas$scilly & areas$cent_x < 170000, ]
gp <- sf::st_drop_geometry(cornwall_gp)
gp <- gp[!gp$scilly & gp$easting < 170000, ]
inputs <- prepare_allocation(stats::setNames(areas$registered, areas$lsoa),
                             stats::setNames(gp$gp_fte_extg, gp$practice),
                             cornwall_travel[areas$lsoa, gp$practice])

# Gaussian decay and a fitted outside weight: the parameters are sigma and v0
model <- clm_allocation(inputs, family = "gaussian", fit_v0 = TRUE)
model
#> <spax model> conditional logit with an outside option
#>   decay       gaussian (sigma = travel scale): fit
#>   attraction  (kappa * supply)^beta: kappa = 1, beta = 1
#>   outside     v0: fit
#>   data        85 origins x 15 facilities

# Evaluate it at a travel scale of 10 minutes and an outside weight of 1
evaluate_allocation(model, c(sigma = 10, v0 = 1))$coverage
#> <allocation coverage> gaussian (sigma=10, v0=1): A=0.0006053; rho=0.9707; abar=0.0006236 
#> conservation: PASS  relative difference: 0 
#> facilities: 15  excluded (zero load): 0 
#> support: fitted positive-demand origins 

# Or fit it to the registrations of these residents at these practices
flows <- cornwall_flows[cornwall_flows$lsoa %in% areas$lsoa &
                          cornwall_flows$practice %in% gp$practice, ]
totals <- sapply(split(flows$patients, flows$practice), sum)
coef(fit_allocation(model, totals, loss = "poisson"))
#>     sigma        v0 
#> 13.223212  1.001842 

# Power decay, a fitted supply elasticity beta and an outside weight held at 0.5
clm_allocation(inputs, family = "power", v0 = 0.5, fit_beta = TRUE)
#> <spax model> conditional logit with an outside option
#>   decay       power (sigma = exponent): fit
#>   attraction  (kappa * supply)^beta: kappa = 1, beta = fit
#>   outside     v0: 0.5
#>   data        85 origins x 15 facilities
```
