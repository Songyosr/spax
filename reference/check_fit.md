# Check what fitted facility counts identify in an allocation model

Compares a fitted allocation with two simpler allocations on the same
loss and observed targets, and summarizes the travel times the
allocations imply. The check reports only: it never changes or replaces
the fit.

## Usage

``` r
check_fit(model, fit, trip_breaks = c(30, 60, 120))

check_allocation(model, fit, trip_breaks = c(30, 60, 120))

# S3 method for class 'ae_allocation_check'
print(x, digits = 4, ...)

# S3 method for class 'ae_allocation_check'
as.data.frame(x, row.names = NULL, optional = FALSE, ...)
```

## Arguments

- model:

  The CLM from
  [`clm_allocation()`](https://songyosr.github.io/spax/reference/clm_allocation.md)
  that was fitted, or a result of
  [`spax()`](https://songyosr.github.io/spax/reference/spax.md) that
  fitted a model; the result supplies its bound model and its fit, so
  leave `fit` out. A result that was evaluated at given values has no
  fit and is refused, and so is an FCA index (a result of
  [`spax()`](https://songyosr.github.io/spax/reference/spax.md) with an
  FCA specification, or of
  [`spax_e2sfca()`](https://songyosr.github.io/spax/reference/spax_e2sfca.md)
  and the other one-shots): an index is computed, not fitted.

- fit:

  The `ae_multistart_fit` returned by
  [`fit_allocation()`](https://songyosr.github.io/spax/reference/fit_allocation.md)
  for `model`. A search with no selected fit cannot be checked. Not
  given when `model` is a result of
  [`spax()`](https://songyosr.github.io/spax/reference/spax.md).

- trip_breaks:

  Positive travel times (in the prepared travel units) for which the
  share of expected trips longer than each value is reported.

- x:

  An `ae_allocation_check`.

- digits:

  Significant digits for printed values.

- ...:

  Unused.

- row.names, optional:

  Standard
  [`as.data.frame()`](https://rspatial.github.io/terra/reference/as.data.frame.html)
  arguments; unused.

## Value

An `ae_allocation_check` with a `comparators` table (loss, loss
difference from the fit, parameters, need-weighted contact and implied
trips), the exact comparator `parameters` (those of `no_decay` are the
re-optimized parameters other than `sigma`, an empty vector when the
model fits only `sigma`), the selected fit's parameter `boundary` flags,
`search_at_edge`, logical `flags`, and `poisson_floor` (for a Poisson
fit: its floor `eps` and the number of cells with positive observed
counts modelled below it, which cap the loss; `NULL` otherwise). Use
[`print()`](https://rdrr.io/r/base/print.html) or
[`as.data.frame()`](https://rspatial.github.io/terra/reference/as.data.frame.html).

## Details

`check_fit()` is the general name of this step and `check_allocation()`
its earlier name: the two are the same function, and they differ only in
the name that a message gives for the step.

Facility counts constrain where predicted loads fall but not where users
live. When facility capacity closely tracks the observed counts, an
allocation that ignores travel time can reproduce the counts as well as,
or better than, a distance-decay allocation. The fitted decay then
depends on the travel support, such as a catchment limit on the prepared
travel times, rather than on the counts. The comparators are:

- `fitted`: the selected fit, recomputed at its parameters.

- `no_decay`: distance decay switched off within the prepared travel
  support: every finite travel time gets the same weight, whatever decay
  kernel the model uses, and absent edges stay absent. Origins choose
  among reachable facilities in proportion to attractiveness, and the
  outside mass is re-optimized when the model fits it. Its reported
  parameters leave out the decay parameter `sigma`, which this
  allocation does not use.

- `capacity_only`: facility loads proportional to attractiveness,
  ignoring travel entirely, with one fitted scale factor. Available for
  facility `utilization` targets only: it applies to facility totals
  without outside counts, and is not reported (`NA`) for any other
  output.

For the outside-count outputs (`flow_outside`, `utilization_outside`),
the loss of every comparator is scored on the whole target, the
`"(outside)"` cells included, as is the count correlation that
[`compare_allocations()`](https://songyosr.github.io/spax/reference/compare_fits.md)
reports for such fits.

When the model fits the attractiveness exponent `beta`, it is
re-optimized in both comparators. Comparator searches start from the
fit's own scale and widen while the optimum lies at the search edge;
`flags` records any search that still ends at an edge.

Implied trips are one-way travel times weighted by expected flows, using
the prepared travel times. Flows on absent edges (possible only for
`capacity_only`) have no travel time: the quantiles and the shares of
trips longer than each break are among flows on reachable edges, and the
absent share is reported separately. To see how the fit depends on a
catchment limit, prepare the inputs again with travel beyond the limit
set to `Inf`, refit, and check each fit.

## See also

Other allocation steps:
[`bootstrap_fit()`](https://songyosr.github.io/spax/reference/bootstrap_fit.md),
[`calc_demand()`](https://songyosr.github.io/spax/reference/calc_demand.md),
[`calc_travel_time()`](https://songyosr.github.io/spax/reference/calc_travel_time.md),
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
               cornwall_travel[areas$lsoa, gp$practice],
               metadata = list(travel_units = "minutes"))

# Fit to the registrations of these residents at these practices
flows <- cornwall_flows[cornwall_flows$lsoa %in% areas$lsoa &
                          cornwall_flows$practice %in% gp$practice, ]
totals <- obs_totals(tapply(flows$patients, flows$practice, sum))
clm <- spax(d, spec_clm(), observed = totals)

# The fit against no decay and against capacity shares alone, with the
# shares of trips longer than 10 and 20 minutes
check <- check_fit(clm, trip_breaks = c(10, 20))
check
#> <allocation check>
#>     comparator     loss loss_difference contact trip_median trip_p90
#>         fitted -1200843               0  0.9784          10       21
#>       no_decay -1199419            1424  0.9742          16       26
#>  capacity_only -1199419            1424      NA          16       26
#> Trip lengths are one-way, in minutes, weighted by expected flows.
#> 
#> The fitted allocation reproduces the counts better than both comparators.
as.data.frame(check)[, c("comparator", "parameters", "trips_over_10",
                         "trips_over_20")]
#>      comparator            parameters trips_over_10 trips_over_20
#> 1        fitted sigma=13.22, v0=1.002     0.4770478     0.1093308
#> 2      no_decay    no decay; v0=2.396     0.6817231     0.2935631
#> 3 capacity_only            scale=1609     0.6817231     0.2935631

# The same check from a model and its fit
check_allocation(clm$model, clm$fit)
#> <allocation check>
#>     comparator     loss loss_difference contact trip_median trip_p90
#>         fitted -1200843               0  0.9784          10       21
#>       no_decay -1199419            1424  0.9742          16       26
#>  capacity_only -1199419            1424      NA          16       26
#> Trip lengths are one-way, in minutes, weighted by expected flows.
#> 
#> The fitted allocation reproduces the counts better than both comparators.
```
