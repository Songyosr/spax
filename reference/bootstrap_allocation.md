# Conditional facility-case bootstrap for a fitted allocation

This experimental operation runs separately from fitting. It resamples
facility observation multiplicities, retaining every physical facility
and its supply in the allocation network. Need, travel, capacity and
included-network inputs remain fixed. Facility independence is a
caller-declared working assumption.

## Usage

``` r
bootstrap_allocation(
  model,
  fit,
  observation,
  scenario = NULL,
  seed,
  checkpoint = NULL
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
  for the model and fit given separately.

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

  Required nonnegative integer; attempt b uses seed+b. The caller's RNG
  state and RNG kind are restored, including when interrupted.

- checkpoint:

  Optional RDS file in an existing directory. Completed attempts are
  saved atomically and resumed after exact input/settings/version
  checks. Completed failures are never retried; incomplete runs retain
  actual counts.

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
with fitted sigma and v0 and `kappa` and `beta` equal to 1, the result
also has a `coverage_note` saying that coverage was evaluated for that
case and not studied separately for the model; it is printed with the
result. Scientific refusals are inspectable results; invalid inputs
error.

## Details

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

Exactly 399 attempts use the starts and positive bounds of the original
fit (given, or found by the default search of
[`fit_allocation()`](https://songyosr.github.io/spax/reference/fit_allocation.md)),
with fixed numerical controls. Failed attempts are retained without
replacement. Successful inner boundary estimates remain in the
distribution. Original mean boundaries, local rank deficiency (fewer
identified directions than fitted parameters) or insufficient fit
precision, including a score that cannot be computed, refuse intervals;
percentile intervals require a complete run of 399 attempts and at least
380 finite successful values per quantity. Incomplete results retain
their point estimates and diagnostics but withhold intervals. Coverage
was evaluated for Gaussian decay with fitted sigma and v0, in generated
80-origin/40-facility designs under tested fixed-input laws; other
kernels and a fitted beta use the same procedure without a separate
coverage study. Coverage validation is not a universal guarantee of
exact 95 percent coverage or validation of an application's observation
law.

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
[`check_allocation()`](https://songyosr.github.io/spax/reference/check_allocation.md),
[`clm_allocation()`](https://songyosr.github.io/spax/reference/clm_allocation.md),
[`compare_allocations()`](https://songyosr.github.io/spax/reference/compare_allocations.md),
[`decay_kernel()`](https://songyosr.github.io/spax/reference/decay_kernel.md),
[`evaluate_allocation()`](https://songyosr.github.io/spax/reference/evaluate_allocation.md),
[`fit_allocation()`](https://songyosr.github.io/spax/reference/fit_allocation.md),
[`huff_allocation()`](https://songyosr.github.io/spax/reference/huff_allocation.md),
[`predict_allocation()`](https://songyosr.github.io/spax/reference/predict_allocation.md),
[`prepare_allocation()`](https://songyosr.github.io/spax/reference/prepare_allocation.md)
