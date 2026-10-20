# Declare the Huff model over checked allocation inputs

Declare the Huff model, a conditional logit allocation without an
outside option, over checked allocation inputs.

## Usage

``` r
huff_allocation(
  inputs,
  family = "power",
  kappa = 1,
  beta = 1,
  fit_beta = TRUE,
  by_group = character(),
  fixed = NULL
)
```

## Arguments

- inputs:

  An `ae_prepared_inputs` object from checked preparation.

- family:

  Travel kernel, as in
  [`clm_allocation()`](https://songyosr.github.io/spax/reference/clm_allocation.md):
  `"power"` (the default, Huff's form), `"gaussian"`, `"exponential"`, a
  declaration from
  [`decay_kernel()`](https://songyosr.github.io/spax/reference/decay_kernel.md),
  or for inputs with demand groups a named list with one of these per
  group.

- kappa:

  Positive supply scale used in attractiveness, not in reported raw
  supply or capacity attribution.

- beta:

  Positive exponent on attractiveness in `(kappa * supply)^beta`: the
  value held fixed when `fit_beta = FALSE`.

- fit_beta:

  `TRUE` (the default) fits `beta` as a parameter; `FALSE` holds it at
  the given value.

- by_group:

  For inputs with demand groups: `"sigma"` lets the decay differ by
  group (default none). `"v0"` is refused, since the Huff model has no
  outside option.

- fixed:

  For inputs with demand groups: a named numeric vector of parameters
  held fixed, as in
  [`clm_allocation()`](https://songyosr.github.io/spax/reference/clm_allocation.md).
  Names of outside weights (`v0`) are refused.

## Value

The existing private `ae_problem` for corrected static CLM with no
outside option. Parameter order is sigma, then beta when it is fitted.
With demand groups sigma is `sigma.<group>` for the groups in
`by_group`.

## Details

The Huff model (Huff, 1963) gives each facility a share of each origin's
demand in proportion to its attractiveness times a decay in travel time:
`P_ij = a_j W(d_ij) / sum_k a_k W(d_ik)`, where
`a_j = (kappa * S_j)^beta` for facility supply `S_j`. Every origin's
demand is shared among the facilities, so the shares at an origin add up
to 1.

This is the conditional logit model of
[`clm_allocation()`](https://songyosr.github.io/spax/reference/clm_allocation.md)
without an outside option: `huff_allocation()` calls
[`clm_allocation()`](https://songyosr.github.io/spax/reference/clm_allocation.md)
with `v0 = 0` and returns the same kind of model, so
[`fit_allocation()`](https://songyosr.github.io/spax/reference/fit_allocation.md),
[`evaluate_allocation()`](https://songyosr.github.io/spax/reference/evaluate_allocation.md),
[`predict_allocation()`](https://songyosr.github.io/spax/reference/predict_allocation.md),
[`check_allocation()`](https://songyosr.github.io/spax/reference/check_allocation.md)
and
[`compare_allocations()`](https://songyosr.github.io/spax/reference/compare_allocations.md)
work on it unchanged. Use `clm_allocation(fit_v0 = TRUE)` when part of
the demand goes to facilities that are not in the data.

With the default power decay, `W(d) = d^(-sigma)`, so spax's `sigma` is
Huff's distance exponent and `beta` is his exponent on attractiveness.
Both are fitted by default (`fit_beta = TRUE`); with `beta = 1` and
`fit_beta = FALSE`, attractiveness is proportional to supply. Power
decay is undefined at a travel time of 0, so use a positive minimum
travel time or another kernel (`family`).

The facility bootstrap
[`bootstrap_allocation()`](https://songyosr.github.io/spax/reference/bootstrap_allocation.md)
needs a fitted outside weight and so does not apply to a Huff model.

## References

Huff DL (1963). A probabilistic analysis of shopping center trade areas.
*Land Economics* 39(1):81-90.
[doi:10.2307/3144521](https://doi.org/10.2307/3144521)

## See also

[`clm_allocation()`](https://songyosr.github.io/spax/reference/clm_allocation.md)
for the general model with an outside option.

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
[`predict_allocation()`](https://songyosr.github.io/spax/reference/predict_allocation.md),
[`prepare_allocation()`](https://songyosr.github.io/spax/reference/prepare_allocation.md)

## Examples

``` r
# Mainland Cornwall: small areas share their registered residents among the
# practices in proportion to attractiveness times travel-time decay.
areas <- sf::st_drop_geometry(cornwall_lsoa[!cornwall_lsoa$scilly, ])
gp <- sf::st_drop_geometry(cornwall_gp[!cornwall_gp$scilly, ])
travel <- cornwall_travel[areas$lsoa, gp$practice]

# Without an outside option, demand is the residents registered at the
# mainland practices
demand <- stats::setNames(areas$registered_in, areas$lsoa)
supply <- stats::setNames(gp$gp_fte_extg, gp$practice)
inputs <- prepare_allocation(demand, supply, travel)

# Power decay with a fitted attractiveness exponent
model <- huff_allocation(inputs)

# Evaluate at given parameters: distance exponent 2, attractiveness
# exponent 1
evaluate_allocation(model, c(sigma = 2, beta = 1))
#> <ae_equilibrium>
#> converged: TRUE
#> iterations: 1
#> error: 0
#> residual_norm: 0

# Fit practice totals of registrations from two starts
totals <- tapply(cornwall_flows$patients, cornwall_flows$practice, sum)
observed <- stats::setNames(numeric(ncol(travel)), colnames(travel))
observed[intersect(names(totals), names(observed))] <-
  totals[intersect(names(totals), names(observed))]
fit <- fit_allocation(
  model, observed,
  starts = rbind(a = c(sigma = 1, beta = 0.5), b = c(sigma = 3, beta = 1.5)),
  lower = c(sigma = 0.1, beta = 0.05), upper = c(sigma = 10, beta = 5),
  loss = "poisson"
)
fit$best$theta
#>     sigma      beta 
#> 1.0415962 0.6062301 
```
