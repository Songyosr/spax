# Calibrate a declared allocation model, by default from starts found in the data

Count targets and loss choice are separate. When `starts` are given, the
loss defaults to weighted SSE; name Poisson explicitly. Custom
loss/gradient functions pass through. Observation-indexed loss arguments
must be in canonical order. Starts, boundaries, failed runs and
disagreement retain the existing multistart result; a selected fit is
not a claim of identification or global optimality.

## Usage

``` r
fit_allocation(
  model,
  observed,
  starts = NULL,
  lower = NULL,
  upper = NULL,
  output = "utilization",
  loss = "weighted_sse",
  loss_args = list(),
  loss_grad = NULL,
  gradient = FALSE,
  ...
)
```

## Arguments

- model:

  A static CLM from
  [`clm_allocation()`](https://songyosr.github.io/spax/reference/clm_allocation.md)
  or
  [`huff_allocation()`](https://songyosr.github.io/spax/reference/huff_allocation.md).
  A kernel declared `smooth = FALSE` cannot be fitted.

- observed:

  Numeric vector or origin-by-facility matrix. A one-dimensional array
  or table, such as the result of
  [`tapply()`](https://rdrr.io/r/base/tapply.html) or
  [`table()`](https://rdrr.io/r/base/table.html), is read as the vector
  named by its names. For a factor level without records
  [`tapply()`](https://rdrr.io/r/base/tapply.html) gives `NA`, which the
  fit reads as unobserved: give it `default = 0` (or use
  [`table()`](https://rdrr.io/r/base/table.html)) when no record means a
  count of zero. Named axes match declared IDs independently; unnamed
  axes are positional. Keep all IDs and use NA for unobserved entries.
  The count-valued `utilization`, `flow`, `utilization_by_group` and
  outside-count targets reject negative nonmissing observations;
  fractional counts are allowed. Other output domains pass to the fitter
  as given. For `utilization_by_group` (a model with demand groups) a
  facility-by-group matrix of observed counts with both axes named
  (group columns by group ID, facility rows by facility ID, any order).
  For the outside-count outputs the same shapes with the outside count
  added under the name `"(outside)"`: for `flow_outside` an extra column
  of the origin-by-facility matrix, for `utilization_outside` an extra
  element of the named vector, for `utilization_outside_by_group` an
  extra row of the facility-by-group matrix. The names are matched to
  the IDs in any order, the outside entry included, and `NA` leaves an
  outside count unobserved.

- starts:

  `NULL` (default) for the default search, or a finite positive matrix,
  one explicit independent start per row, with exactly the named model
  parameter columns. No random starts are added. Each start must lie
  inside the bounds. Given starts with `lower` or `upper` missing or
  partial use default bounds for the missing entries, widened to include
  every start.

- lower, upper:

  `NULL` (default), or named positive parameter bounds with lower \<
  upper: complete, or for some of the parameters only (the default
  bounds replace the missing entries). For a declared kernel, the starts
  and bounds of sigma must lie within the bounds of the declaration.

- output:

  Exact model output to calibrate; default facility utilization. `flow`
  is expected origin-facility counts, while `allocation` is an inside
  probability operator. Requesting counts does not select a likelihood.
  `utilization_by_group` is the expected count of each group at each
  facility (a model with demand groups, see
  [`clm_allocation()`](https://songyosr.github.io/spax/reference/clm_allocation.md)).
  The group-specific parameters of such a model (`sigma.<group>`,
  `v0.<group>`, from `by_group`) can be fitted only to this output, with
  observed counts for every group that has one and positive demand in
  that group; aggregated targets leave them multimodal and weakly
  identified, so the fit is refused (before any start) with a message
  naming the parameters. Hold them (`clm_allocation(fixed = )`, or
  numbers in the decay and outside slots of
  [`spec_clm()`](https://songyosr.github.io/spax/reference/spec_clm.md)),
  or fit shared parameters to any target. `flow_outside`,
  `utilization_outside` and `utilization_outside_by_group` are the three
  count outputs above with the modelled outside count added as one more
  column, element or row named `"(outside)"` (see the section on outside
  counts); they need a model with an outside option. The group-specific
  parameters can also be fitted to `utilization_outside_by_group`.

- loss:

  `"weighted_sse"`, `"poisson"`, or a function accepting predicted and
  observed values. Weighted SSE divides squared residuals by
  observed+eta, with eta=1 by default. Poisson omits the data-only
  log-factorial constant. It floors modelled counts at `eps`
  (`loss_args`), 1e-250 by default, only to avoid the log of zero; cells
  with positive observed counts modelled below the floor are reported
  with a warning, because the loss is capped there. Fits made before
  this default used 1e-9; pass `loss_args = list(eps = 1e-9)` to
  reproduce them. Joint fitting does not force total matching. The
  default is `"weighted_sse"` when `starts` are given; without `starts`
  the loss must be named. The outside-count outputs accept `"poisson"`
  only (not the default weighted SSE and not a custom function).

- loss_args:

  Named auxiliary arguments passed to loss callbacks. For
  `loss = "poisson"` an `eps` of 1e-250 is added when none is given;
  other arguments pass unchanged. Any observation-indexed values must
  already follow canonical observed order.

- loss_grad:

  Optional matching parameter-gradient callback. Built-in choices supply
  their analytic loss gradients. A custom loss requires its own
  loss_grad when gradient=TRUE; none is invented.

- gradient:

  Logical; use the implicit sensitivity route when TRUE. FALSE retains
  the underlying fitter's default behavior. When `starts` is `NULL` and
  `gradient` is not given, it is `TRUE` if the loss has a gradient (the
  built-in losses, or a custom loss with `loss_grad`).

- ...:

  Named multistart or fitting controls, including solver tolerances,
  optimizer `control`, `keep_fits` and descriptive disagreement
  tolerances. When `starts` is `NULL` and `control` is not given it is
  `list(maxit = 300, factr = 1e3, pgtol = 1e-2)`.

## Value

An `ae_multistart_fit`, retaining selected fit, all per-start
diagnostics, boundaries, warnings/errors and descriptive disagreement. A
fit whose starts came from the default search also has a
`default_search` element: the rule, the sigma range, the number of grid
points and of finite ones, the grid of sigma, v0 and loss, the starts,
the bounds, the controls and the loss.

## Details

A model whose decay kernel was declared with `smooth = FALSE` (see
[`decay_kernel()`](https://songyosr.github.io/spax/reference/decay_kernel.md))
is refused, because fitting needs a kernel that is smooth in sigma.
Evaluate such a kernel at fixed parameters with
[`evaluate_allocation()`](https://songyosr.github.io/spax/reference/evaluate_allocation.md),
for example over a grid of sigma and v0.

**Outside counts.** The outputs `flow_outside`, `utilization_outside`
and `utilization_outside_by_group` are `flow`, `utilization` and
`utilization_by_group` with the modelled outside count added: the demand
of the active origins that goes to the outside option, that is, to
facilities that are not in the data or to none. For `flow_outside` the
outside count of origin `i` is `D_i * outside_share_i`, one more column
(each row then adds up to the demand of the origin);
`utilization_outside` adds the sum of these over the origins as one more
element, and `utilization_outside_by_group` adds, for each group, the
sum of its demand times its outside share as one more row. They are
available for a model with an outside option (a fitted `v0`, or a held
`v0` above 0) and are refused as targets for any other model, such as
the Huff model: "outside counts need a model with an outside option".
They are fitted like the other count outputs, with the built-in Poisson
loss (`loss = "poisson"`) and no further argument; any other loss, the
default weighted SSE and custom loss functions included, is refused:
"outside counts are fitted with loss = \\poisson\\". The observed counts
carry the outside counts under the name `"(outside)"`: a column of the
flow matrix, an element of the totals vector, a row of the
facility-by-group matrix, matched by name like the facility IDs and in
any order. `NA` is allowed: an origin or group whose outside count is
not known carries no outside term, and its facility counts are used as
before. A facility cannot have the ID `"(outside)"`.

What an outside count means depends on how the counts were recorded. It
is the demand of the data's active origins that went to facilities
outside the data or to none, so for flows it is known by subtraction,
`y0_i = D_i - sum_j y_ij`, only when demand counts the same people as
the observations (for example registrations by residence from the same
extract and date). It is not a population estimate: list sizes and
population differ by several percent, and the gap goes straight into the
outside count. For an origin whose outside count is known and
`y0_i + sum_j y_ij = D_i`, the Poisson loss on its cells (facilities and
outside) is the multinomial likelihood of its choices up to constants.
When this holds for every origin of a flow fit and `v0` is free and
inside its bounds, the fitted model's total outside count equals the
observed one (the score equation of `log v0`). This is not claimed for
totals or for models with demand groups: there the total outside count
is not guaranteed to match. Fits to different outputs, or with and
without outside counts, are fits to different data, so
[`compare_allocations()`](https://songyosr.github.io/spax/reference/compare_fits.md)
gives no loss difference between them; compare models on the same
observation, and a model without an outside option (Huff) on the
observation without outside counts.
[`bootstrap_allocation()`](https://songyosr.github.io/spax/reference/bootstrap_fit.md)
refuses these fits: the facility-case bootstrap is defined for facility
totals without outside counts.

**Default search.** For the count outputs `starts`, `lower` and `upper`
are optional. When `starts` is `NULL` they come from the data (`loss`
must then be named, because the estimator is not chosen silently):

1.  the decay kernel is probed at the nearest, a far and the largest
    travel time of the data for the range of sigma over which it still
    decays over the travel times, whatever the unit of the travel times;

2.  sigma takes 15 log-spaced values over that range, from the steepest
    to the flattest kernel (group-specific sigmas walk together);

3.  a fitted outside weight `v0` is seeded from the observed outside
    share, times 0.1, 1 and 10, and a fitted `beta` starts at 1 (for the
    outside-count outputs the seed reads the facility counts only, the
    `"(outside)"` entries dropped, as for `flow` and `utilization`);

4.  the loss is evaluated at every point, by the code the fit itself
    uses;

5.  the fit starts from the best point of up to three sigma values
    (local minima along the grid first), named `scan1`, `scan2`, ...

The default bounds are the probed range for sigma; for `v0`, from a
thousandth of the smallest to a thousand times the largest of the best
values on the grid; and 0.05 to 5 for `beta`. Bounds you give replace
the default ones one by one, and starts you give widen the default
bounds to include them (the scan then runs only to find a default bound
of `v0`). The search is recorded in the `default_search` element of the
result.

When the starts come from the search, `gradient` and `control` default
(each unless you give it) to analytic gradients, when the loss has one,
and strict optimizer settings,
`control = list(maxit = 300, factr = 1e3, pgtol = 1e-2)`, because the
optimizer defaults can stop early at a different outside weight. Given
`starts` keep the earlier defaults.

A model in which every sigma is held (demand groups with `fixed`) has no
sigma to probe or scan: the starts are the three `v0` seeds at the held
values, or, with only `beta` free, `beta` = 0.5, 1 and 2.

A fully observed target whose counts reach the total demand has no
outside share for `v0` to explain: the search stops with an error (use
no outside option: `fit_v0 = FALSE`), and warns when the counts are
within 0.1% of the total demand (for the outside-count outputs these are
the facility counts, without the `"(outside)"` entries). Starts and
bounds remain required for any output other than `utilization`, `flow`,
`utilization_by_group` and the three outside-count outputs, and calls
that give `starts`, `lower` and `upper` run exactly as before.

The cost is about one solve of the model per grid point, 15 to 45
solves: about a second on the 336 by 54 Cornwall example, about 24
seconds for 20,000 origins and 400 facilities, which is roughly the cost
of one strict start. Give your own starts to skip it.

## See also

[allocation-fit-methods](https://songyosr.github.io/spax/reference/allocation-fit-methods.md)
for methods on saved fits.

Other allocation steps:
[`bootstrap_fit()`](https://songyosr.github.io/spax/reference/bootstrap_fit.md),
[`calc_demand()`](https://songyosr.github.io/spax/reference/calc_demand.md),
[`calc_travel_time()`](https://songyosr.github.io/spax/reference/calc_travel_time.md),
[`check_fit()`](https://songyosr.github.io/spax/reference/check_fit.md),
[`clm_allocation()`](https://songyosr.github.io/spax/reference/clm_allocation.md),
[`compare_fits()`](https://songyosr.github.io/spax/reference/compare_fits.md),
[`decay_kernel()`](https://songyosr.github.io/spax/reference/decay_kernel.md),
[`evaluate_allocation()`](https://songyosr.github.io/spax/reference/evaluate_allocation.md),
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
travel <- cornwall_travel[areas$lsoa, gp$practice]
inputs <- prepare_allocation(stats::setNames(areas$registered, areas$lsoa),
                             stats::setNames(gp$gp_fte_extg, gp$practice),
                             travel)
model <- clm_allocation(inputs, fit_v0 = TRUE)

# Registrations of these residents at these practices
flows <- cornwall_flows[cornwall_flows$lsoa %in% areas$lsoa &
                          cornwall_flows$practice %in% gp$practice, ]

# Fit to the practice totals. Without starts, the search finds starts and
# bounds from the data, and the loss must be named
totals <- sapply(split(flows$patients, flows$practice), sum)
fit <- fit_allocation(model, totals, loss = "poisson")
fit
#> <allocation fit> 3 starts, selected scan1
#> eligible starts: 3 / 3
#> selected start: scan1
#> starts: default search, 45 of 45 grid points, loss poisson
#> loss disagreement: FALSE; parameter disagreement: FALSE
#>  start     loss eligible optim_convergence solver_converged residual seconds
#>  scan1 -1200843     TRUE                 0             TRUE        0   0.017
#>  scan2 -1200843     TRUE                 0             TRUE        0   0.029
#>  scan3 -1200843     TRUE                 0             TRUE        0   0.025
#>  boundary   status                                    optim_message
#>     FALSE eligible CONVERGENCE: NORM OF PROJECTED GRADIENT <= PGTOL
#>     FALSE eligible CONVERGENCE: NORM OF PROJECTED GRADIENT <= PGTOL
#>     FALSE eligible CONVERGENCE: NORM OF PROJECTED GRADIENT <= PGTOL
#>  solver_message log_parameter_gap     loss_gap
#>       converged      0.000000e+00 0.000000e+00
#>       converged      8.566946e-07 0.000000e+00
#>       converged      6.215664e-06 1.164153e-09
coef(fit)
#>     sigma        v0 
#> 13.223212  1.001842 

# Fit to the flows: a matrix of small areas by practices, 0 where none. Demand
# counts every registered resident, so the residents registered elsewhere are
# known: they are one more column, "(outside)", fitted by the outside option
flow_matrix <- matrix(0, nrow(travel), ncol(travel), dimnames = dimnames(travel))
flow_matrix[cbind(flows$lsoa, flows$practice)] <- flows$patients
flow_matrix <- cbind(flow_matrix, "(outside)" = areas$registered - rowSums(flow_matrix))
coef(fit_allocation(model, flow_matrix, output = "flow_outside", loss = "poisson"))
#>      sigma         v0 
#> 3.97012750 0.09627051 

# Starts and bounds of your own: one start per row
starts <- rbind(near = c(sigma = 5, v0 = 0.5), far = c(sigma = 30, v0 = 5))
fit_own <- fit_allocation(model, totals, starts = starts,
                          lower = c(sigma = 1, v0 = 0.001),
                          upper = c(sigma = 100, v0 = 100), loss = "poisson")
coef(fit_own)
#>     sigma        v0 
#> 13.223190  1.001846 
```
