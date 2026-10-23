# Compare fitted allocations across model and input specifications

Summarizes several fitted allocations side by side: regional contact,
attributed capacity and its per-contact value, the spread of contact
across origins, agreement between predicted and observed counts, and the
fitted loss. Each fit's coverage is computed from its saved outputs;
nothing is refitted.

## Usage

``` r
compare_fits(
  fits,
  models = NULL,
  reference = 1L,
  zones = NULL,
  coverage_args = list()
)

compare_allocations(
  fits,
  models = NULL,
  reference = 1L,
  zones = NULL,
  coverage_args = list()
)

# S3 method for class 'ae_allocation_comparison'
print(x, digits = 4, ...)

# S3 method for class 'ae_allocation_comparison'
as.data.frame(
  x,
  row.names = NULL,
  optional = FALSE,
  type = c("fits", "zones"),
  ...
)
```

## Arguments

- fits:

  A named list of `ae_multistart_fit` results from
  [`fit_allocation()`](https://songyosr.github.io/spax/reference/fit_allocation.md),
  each with a selected fit. An element can instead be a result of
  [`spax()`](https://songyosr.github.io/spax/reference/spax.md) that
  fitted a model; it supplies its fit and its model. A result that was
  evaluated at given values has no fit and is refused, and so is an FCA
  index (a result of
  [`spax()`](https://songyosr.github.io/spax/reference/spax.md) with an
  FCA specification, or of
  [`spax_e2sfca()`](https://songyosr.github.io/spax/reference/spax_e2sfca.md)
  and the other one-shots): the message names the element.

- models:

  Optional named list of the corresponding models from
  [`clm_allocation()`](https://songyosr.github.io/spax/reference/clm_allocation.md),
  with the same names, used to verify that travel times are identical
  before reporting loss differences. The elements of `fits` that are
  results of
  [`spax()`](https://songyosr.github.io/spax/reference/spax.md) bring
  their own models and must not be named here. When `fits` mixes results
  with plain fits, give `models` for the plain ones.

- reference:

  Name or position of the reference fit.

- zones:

  Optional data frame with `origin_id` and `zone` columns covering every
  fitted origin.

- coverage_args:

  Named reporting controls passed to the coverage calculation for every
  fit, such as `norm`, `rho_floor`, `contact_need_floor`, `contact_cut`
  and `adequacy_cut`.

- x:

  An `ae_allocation_comparison`.

- digits:

  Significant digits for printed values.

- ...:

  Unused.

- row.names, optional:

  Standard
  [`as.data.frame()`](https://rspatial.github.io/terra/reference/as.data.frame.html)
  arguments; unused.

- type:

  `"fits"` for one row per fit or `"zones"` for zone summaries.

## Value

An `ae_allocation_comparison` with a `table` (one row per fit), a long
`zones` table when zones are supplied, and the reference name. Use
[`print()`](https://rdrr.io/r/base/print.html) or
`as.data.frame(type = "fits")` / `as.data.frame(type = "zones")`.

## Details

`compare_fits()` is the general name of this step and
`compare_allocations()` its earlier name: the two are the same function,
and they differ only in the name that a message gives for the step.

Loss differences from the reference are reported only when the
comparison is like for like: the same observed targets, loss function
and arguments (Poisson fits: the same floor, a fit that stored none used
1e-9, or floors that change neither loss), demand, supply, origin and
facility IDs and prepared travel times. Travel times are checked only
when `models` are supplied, and each model must reproduce its own fit;
otherwise loss differences are left missing. Fits of models with demand
groups
([`prepare_allocation()`](https://songyosr.github.io/spax/reference/prepare_allocation.md))
are comparable only with each other, with the same group IDs and, once
`models` are supplied, the same demand and the same travel times for
every group; a shared-parameter fit and a `by_group` fit on the same
group-labelled target answer whether the group decays differ. The table
then shows the free parameters, such as `sigma.<group>`, and coverage
comes from the pooled outputs. Contact and capacity values remain
comparable across specifications, but they are conditional on each
specification's inputs.

For fits to the outside-count outputs (`flow_outside`,
`utilization_outside`, `utilization_outside_by_group`), the
`count_correlation` is over the whole target, the `"(outside)"` cells
included, so it is not comparable with the correlation of a fit to
facility counts without outside counts. The capacity-only comparator of
[`check_allocation()`](https://songyosr.github.io/spax/reference/check_fit.md)
applies to facility totals without outside counts only.

With `zones`, each fit is also summarized by zone using contact-weighted
aggregation, and the zones whose reporting class differs from the
reference are listed. Classes require an adequacy cut in
`coverage_args`; each fit uses its own regional contact as the contact
cut unless `contact_cut` is given.

## See also

Other allocation steps:
[`bootstrap_fit()`](https://songyosr.github.io/spax/reference/bootstrap_fit.md),
[`calc_demand()`](https://songyosr.github.io/spax/reference/calc_demand.md),
[`calc_travel_time()`](https://songyosr.github.io/spax/reference/calc_travel_time.md),
[`check_fit()`](https://songyosr.github.io/spax/reference/check_fit.md),
[`clm_allocation()`](https://songyosr.github.io/spax/reference/clm_allocation.md),
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

# Registrations of these residents at these practices
flows <- cornwall_flows[cornwall_flows$lsoa %in% areas$lsoa &
                          cornwall_flows$practice %in% gp$practice, ]
totals <- obs_totals(tapply(flows$patients, flows$practice, sum))

# The same counts fitted with two decay kernels
gaussian <- spax(d, spec_clm(decay_gaussian()), observed = totals)
exponential <- spax(d, spec_clm(decay_exponential()), observed = totals)

# Loss differences are measured from the first fit
comparison <- compare_fits(list(gaussian = gaussian, exponential = exponential))
comparison
#> <allocation comparison> reference: gaussian
#>  specification contact capacity_per_contact cv_contact count_correlation
#>       gaussian  0.9784            0.0006187   0.007782            0.8850
#>    exponential  0.9781            0.0006189   0.007585            0.9016
#>  loss_difference
#>              0.0
#>           -124.4
as.data.frame(comparison)[, c("specification", "parameters", "loss_difference")]
#>   specification             parameters loss_difference
#> 1      gaussian  sigma=13.22, v0=1.002          0.0000
#> 2   exponential sigma=0.081, v0=0.7139       -124.4451
```
