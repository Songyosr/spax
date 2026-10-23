# Conditional facility-case bootstrap for a fitted allocation

This experimental operation runs separately from fitting. It resamples
facility observation multiplicities, retaining every physical facility
and its supply in the allocation network. Need, travel, capacity and
included-network inputs remain fixed. Facility independence is a
caller-declared working assumption.

## Usage

``` r
bootstrap_fit(
  model,
  fit,
  observation,
  scenario = NULL,
  seed,
  checkpoint = NULL,
  attempts = 399
)

bootstrap_allocation(
  model,
  fit,
  observation,
  scenario = NULL,
  seed,
  checkpoint = NULL,
  attempts = 399
)
```

## Arguments

- model:

  A checked static CLM from
  [`clm_allocation()`](https://songyosr.github.io/spax/reference/clm_allocation.md)
  with a fitted outside mass (`fit_v0 = TRUE`), a decay kernel that is
  smooth in sigma (built in, or declared with
  [`decay_kernel()`](https://songyosr.github.io/spax/reference/decay_kernel.md)
  and `smooth = TRUE`), and known travel on its retained demand support.
  The supply elasticity beta may be fixed or fitted; `kappa` and a fixed
  `beta` may be any positive values. It can instead be a result of
  [`spax()`](https://songyosr.github.io/spax/reference/spax.md) that
  fitted such a model to facility totals: the result supplies its bound
  model and its fit, so leave `fit` out and give `observation` by name.
  A Huff model, a fit to flows and an evaluated result are refused as
  for the model and fit given separately, and so is an FCA index (a
  result of
  [`spax()`](https://songyosr.github.io/spax/reference/spax.md) with an
  FCA specification, or of
  [`spax_e2sfca()`](https://songyosr.github.io/spax/reference/spax_e2sfca.md)
  and the other one-shots): an index is computed, not fitted.

- fit:

  A matching multistart fit of complete whole facility counts using the
  unweighted built-in Poisson objective. Fractional or partial
  observations, flow targets, fits with observed outside counts
  (`flow_outside`, `utilization_outside`: the facility-case bootstrap is
  defined for facility totals without outside counts) and custom
  objectives are outside this method. Not given when `model` is a result
  of [`spax()`](https://songyosr.github.io/spax/reference/spax.md).

- observation:

  Named list declaring nonblank `event`, `period` (including an explicit
  unresolved reference date), and `sampling="independent_facilities"`.
  Non-overlapping registration alone does not establish independence.

- scenario:

  Optional checked CLM whose prepared system changes supply only. Every
  draw evaluates baseline and scenario together at the same parameters.

- seed:

  Required nonnegative integer; attempt b uses seed+b, so seed plus
  `attempts` must stay within the integer range. The caller's RNG state
  and RNG kind are restored, including when interrupted.

- checkpoint:

  Optional RDS file in an existing directory. Completed attempts are
  saved atomically and resumed after exact input/settings/version
  checks. Completed failures are never retried; incomplete runs retain
  actual counts. A checkpoint belongs to its number of attempts: a run
  cannot be extended to more attempts once it has been made.

- attempts:

  The number of bootstrap attempts, a whole number from 199 to
  1,000,000; 399 by default. More attempts make the interval endpoints
  less dependent on the random draws and take proportionally longer;
  fewer give noisier intervals that tend to be narrower, with lower
  coverage. Choose the number of attempts and the seed before the run:
  runs with the same seed share their first attempts (a run of 999
  repeats the run of 399 and adds 600 more), and changing either after
  seeing a result is outside the method.

## Value

An `ae_allocation_bootstrap` with compact draws, multiplicities, all
start diagnostics, assumptions, failure reasons and conditional
percentile intervals for each fitted parameter (sigma, v0 and, when
fitted, beta), regional contact and optional paired change in modeled
contacts. Regional attributed capacity per need is a fixed-input
identity without an interval: it includes supply with at least one known
finite edge on retained demand support. Unsupported positive supply is
disclosed in `capacity_support`; total supplied capacity can exceed
attributed capacity. For any model other than built-in Gaussian decay
with fitted sigma and v0 and `kappa` and `beta` equal to 1, or a number
of attempts other than 399, the result also has a `coverage_note` saying
that coverage was evaluated for that case and not studied separately; it
is printed with the result. Scientific refusals are inspectable results;
invalid inputs error.

## Details

`bootstrap_fit()` is the general name of this step and
`bootstrap_allocation()` its earlier name: the two are the same
function, and they differ only in the name that a message gives for the
step.

The model is a static CLM with a fitted outside mass (`outside = "fit"`
in
[`spec_clm()`](https://songyosr.github.io/spax/reference/spec_clm.md),
or `fit_v0 = TRUE` in
[`clm_allocation()`](https://songyosr.github.io/spax/reference/clm_allocation.md))
and a decay kernel that is smooth in sigma: the Gaussian, exponential or
power kernel, or a kernel declared with
[`decay_kernel()`](https://songyosr.github.io/spax/reference/decay_kernel.md)
(not with `smooth = FALSE`). The supply elasticity beta may be fixed or
fitted. Every fitted parameter gets its own interval.

A run makes `attempts` attempts, 399 by default, each from the starts
and positive bounds of the original fit (given, or found by the default
search of
[`fit_allocation()`](https://songyosr.github.io/spax/reference/fit_allocation.md)),
with fixed numerical controls. Failed attempts are retained without
replacement. Successful inner boundary estimates remain in the
distribution. Original mean boundaries, local rank deficiency (fewer
identified directions than fitted parameters) or insufficient fit
precision, including a score that cannot be computed, refuse intervals;
percentile intervals require a complete run of all the attempts and
finite successful values from at least 95 percent of them, rounded up
(380 of the default 399). Incomplete results retain their point
estimates and diagnostics but withhold intervals. Coverage was evaluated
for Gaussian decay with fitted sigma and v0 and 399 attempts, in
generated 80-origin/40-facility designs under tested fixed-input laws;
other kernels, a fitted beta and other numbers of attempts use the same
procedure without a separate coverage study. Coverage validation is not
a universal guarantee of exact 95 percent coverage or validation of an
application's observation law.

A facility with positive supply, a known finite travel time on retained
demand support and a modeled mean of exactly zero is treated as
nonregular, so the original fit is refused and no intervals are
available. This happens when the kernel is exactly zero at every finite
travel time of such a facility, as a cutoff or bisquare kernel can be
beyond its range, and also when Gaussian weights underflow to zero.

## See also

Other allocation steps:
[`calc_demand()`](https://songyosr.github.io/spax/reference/calc_demand.md),
[`calc_travel_time()`](https://songyosr.github.io/spax/reference/calc_travel_time.md),
[`check_fit()`](https://songyosr.github.io/spax/reference/check_fit.md),
[`clm_allocation()`](https://songyosr.github.io/spax/reference/clm_allocation.md),
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
d <- prep_data(stats::setNames(areas$registered, areas$lsoa),
               stats::setNames(gp$gp_fte_extg, gp$practice),
               cornwall_travel[areas$lsoa, gp$practice])

# The bootstrap assumes practice totals that are independent counts over one
# period. The registrations of cornwall_flows are a stock on one date, so the
# totals here are drawn from the model instead: Poisson counts at a travel
# scale of 10 minutes and an outside weight of 1
expected <- fitted(spax(d, spec_clm(decay_gaussian(sigma = 10), outside = 1)))
set.seed(1)
totals <- stats::setNames(stats::rpois(length(expected), expected), names(expected))

# Fit the conditional logit model with a fitted outside weight to the totals
fit <- spax(d, spec_clm(decay_gaussian(), outside = "fit"),
            observed = obs_totals(totals))
coef(fit)
#>     sigma        v0 
#> 10.005894  0.963842 

# \donttest{
# 199 attempts, the fewest allowed (399 by default): each draws the practices
# with replacement and refits the model
boot <- bootstrap_fit(fit, observation = list(event = "simulated registrations",
                                              period = "one reference period",
                                              sampling = "independent_facilities"),
                      seed = 2026, attempts = 199)
boot
#> <allocation bootstrap>
#> status: complete
#>  planned attempted successful
#>      199       199        199
#> failed attempts: 0; retained inner-boundary attempts: 0
#> sampling: independent_facilities; event: simulated registrations; period: one reference period
#> supply (raw supply units): total: 90.38; attributed: 90.38; unsupported: 0
#> facilities: total: 15; with retained-demand support: 15; positive supply without support: 0
#> fixed inputs: demand, supply, travel, included network and support policy
#> regional_A is supported raw supply per retained need; positive supply
#> with no finite edge on retained demand support is excluded from
#> attributed capacity and disclosed separately 
#> Conditional percentile intervals; coverage evidence is limited to the
#> package's coverage study, a fixed 80-origin/40-facility generated
#> design. Facility independence and the empirical observation law are
#> assumptions. No simultaneous-map or causal coverage claim. 
#> Coverage was evaluated with 399 attempts; this run used 199. Fewer
#> attempts give noisier intervals that tend to be narrower. 
#> Available bounds: conditional bootstrap intervals.
#>      quantity  estimate  lower  upper                          status
#>         sigma 1.001e+01 9.8030 10.230 conditional_pointwise_bootstrap
#>            v0 9.638e-01 0.8256  1.147 conditional_pointwise_bootstrap
#>  regional_rho 9.717e-01 0.9667  0.976 conditional_pointwise_bootstrap
#>    regional_A 6.053e-04     NA     NA            fixed_input_identity
#>  successful attempted planned
#>         199       199     199
#>         199       199     199
#>         199       199     199
#>         199       199     199
# }
```
