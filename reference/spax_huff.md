# Run the Huff or conditional logit model from raw inputs

Experimental. `spax_huff()` and `spax_clm()` prepare the data with
[`prep_data()`](https://songyosr.github.io/spax/reference/prep_data.md),
specify the model with
[`spec_huff()`](https://songyosr.github.io/spax/reference/spec_clm.md)
or [`spec_clm()`](https://songyosr.github.io/spax/reference/spec_clm.md)
and run it with
[`spax()`](https://songyosr.github.io/spax/reference/spax.md), in one
call: `spax_huff(demand, supply, travel, observed = ...)` is
`spax(prep_data(demand, supply, travel), spec_huff(), observed = ...)`.
See [`spax()`](https://songyosr.github.io/spax/reference/spax.md) for
fitting, evaluation and the result.

## Usage

``` r
spax_huff(
  demand,
  supply,
  travel,
  observed = NULL,
  decay = decay_power(),
  beta = "fit",
  kappa = 1,
  by_group = character(),
  data_args = list(),
  ...
)

spax_clm(
  demand,
  supply,
  travel,
  observed = NULL,
  decay = decay_gaussian(),
  outside = "fit",
  beta = 1,
  kappa = 1,
  by_group = character(),
  data_args = list(),
  ...
)
```

## Arguments

- demand, supply, travel:

  The inputs of
  [`prep_data()`](https://songyosr.github.io/spax/reference/prep_data.md):
  demand by origin (a named list by group for demand groups), supply by
  facility and the origin-by-facility travel matrix.

- observed:

  Observed counts, as in
  [`spax()`](https://songyosr.github.io/spax/reference/spax.md): an
  [`obs_flows()`](https://songyosr.github.io/spax/reference/obs_flows.md)
  or
  [`obs_totals()`](https://songyosr.github.io/spax/reference/obs_flows.md)
  object, or raw counts. `NULL` (the default) evaluates a model that has
  a value for every parameter.

- decay:

  The decay of the model: a decay object
  ([`decay_gaussian()`](https://songyosr.github.io/spax/reference/decay_gaussian.md)
  and its relatives), an exact kernel name, a
  [`decay_kernel()`](https://songyosr.github.io/spax/reference/decay_kernel.md),
  or a named list of these by demand group. The default is the power
  decay for `spax_huff()` and the Gaussian for `spax_clm()`.

- beta:

  The attractiveness exponent: `"fit"` (the default for `spax_huff()`)
  or a positive number (the default `1` for `spax_clm()`).

- kappa:

  A single finite number greater than 0, the scale of the attractiveness
  `(kappa * supply)^beta`. It is held.

- by_group:

  For data with demand groups, the parameters that differ by group, as
  in
  [`spec_clm()`](https://songyosr.github.io/spax/reference/spec_clm.md):
  `"sigma"` and, for `spax_clm()`, `"v0"`.

- data_args:

  A named list of further arguments of
  [`prep_data()`](https://songyosr.github.io/spax/reference/prep_data.md),
  for example
  `list(missing_demand = "exclude", metadata = list(travel_units = "minutes"))`.

- ...:

  Further arguments of
  [`spax()`](https://songyosr.github.io/spax/reference/spax.md):
  `starts`, `lower`, `upper` and the arguments of
  [`fit_allocation()`](https://songyosr.github.io/spax/reference/fit_allocation.md)
  or
  [`evaluate_allocation()`](https://songyosr.github.io/spax/reference/evaluate_allocation.md).

- outside:

  The outside weight `v0` of `spax_clm()`: `"fit"` (the default), `0`
  for no outside option, a positive number, or a value by group (see
  [`spec_clm()`](https://songyosr.github.io/spax/reference/spec_clm.md)).
  The Huff model has no outside option.

## Value

A `spax_allocation` as returned by
[`spax()`](https://songyosr.github.io/spax/reference/spax.md), with the
one-shot call as its `call`.

## See also

Other allocation interface:
[`decay_gaussian()`](https://songyosr.github.io/spax/reference/decay_gaussian.md),
[`obs_flows()`](https://songyosr.github.io/spax/reference/obs_flows.md),
[`prep_data()`](https://songyosr.github.io/spax/reference/prep_data.md),
[`spax()`](https://songyosr.github.io/spax/reference/spax.md),
[`spax_allocation-methods`](https://songyosr.github.io/spax/reference/spax_allocation-methods.md),
[`spec_clm()`](https://songyosr.github.io/spax/reference/spec_clm.md)

## Examples

``` r
areas <- sf::st_drop_geometry(cornwall_lsoa[!cornwall_lsoa$scilly, ])
gp <- sf::st_drop_geometry(cornwall_gp[!cornwall_gp$scilly, ])
flows <- obs_flows(cornwall_flows, origin = "lsoa", facility = "practice",
                   count = "patients")

# Fit Huff from raw inputs in one call
huff <- spax_huff(
  demand = stats::setNames(areas$registered_in, areas$lsoa),
  supply = stats::setNames(gp$gp_fte_extg, gp$practice),
  travel = cornwall_travel[areas$lsoa, gp$practice],
  observed = flows
)
#> observed: 2,290 counts from 1 origin outside the data or left out by it are dropped; 5 counts at 1 facility outside the data are dropped (the Huff model has no outside option)
coef(huff)
#>     sigma      beta 
#> 2.3928431 0.4862684 
```
