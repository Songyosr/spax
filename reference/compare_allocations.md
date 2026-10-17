# Compare fitted allocations across model and input specifications

Summarizes several fitted allocations side by side: regional contact,
attributed capacity and its per-contact value, the spread of contact
across origins, agreement between predicted and observed counts, and the
fitted loss. Each fit's coverage is computed from its saved outputs;
nothing is refitted.

## Usage

``` r
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
  each with a selected fit.

- models:

  Optional named list of the corresponding models from
  [`clm_allocation()`](https://songyosr.github.io/spax/reference/clm_allocation.md),
  with the same names, used to verify that travel times are identical
  before reporting loss differences.

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

Loss differences from the reference are reported only when the
comparison is like for like: the same observed targets, loss function
and arguments, demand, supply, origin and facility IDs and prepared
travel times. Travel times are checked only when `models` are supplied,
and each model must reproduce its own fit; otherwise loss differences
are left missing. Contact and capacity values remain comparable across
specifications, but they are conditional on each specification's inputs.

With `zones`, each fit is also summarized by zone using contact-weighted
aggregation, and the zones whose reporting class differs from the
reference are listed. Classes require an adequacy cut in
`coverage_args`; each fit uses its own regional contact as the contact
cut unless `contact_cut` is given.

## See also

Other experimental allocation workflow:
[`bootstrap_allocation()`](https://songyosr.github.io/spax/reference/bootstrap_allocation.md),
[`calc_demand()`](https://songyosr.github.io/spax/reference/calc_demand.md),
[`calc_travel_time()`](https://songyosr.github.io/spax/reference/calc_travel_time.md),
[`check_allocation()`](https://songyosr.github.io/spax/reference/check_allocation.md),
[`clm_allocation()`](https://songyosr.github.io/spax/reference/clm_allocation.md),
[`decay_kernel()`](https://songyosr.github.io/spax/reference/decay_kernel.md),
[`evaluate_allocation()`](https://songyosr.github.io/spax/reference/evaluate_allocation.md),
[`fit_allocation()`](https://songyosr.github.io/spax/reference/fit_allocation.md),
[`predict_allocation()`](https://songyosr.github.io/spax/reference/predict_allocation.md),
[`prepare_allocation()`](https://songyosr.github.io/spax/reference/prepare_allocation.md)
