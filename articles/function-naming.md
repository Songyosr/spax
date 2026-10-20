# Finding functions and inspecting results

The guides use the bundled Cornwall GP data, described in [The Cornwall
GP example
data](https://songyosr.github.io/spax/articles/cornwall-data.md); this
page needs no data.

``` r

library(spax)
```

spax retains its public FCA functions alongside the experimental static
allocation workflow. Names describe the interface they belong to; there
is no rule that every complete workflow must start with `spax_`.
Existing FCA calls and result types remain supported. Within the
allocation workflow, model declarations are named `<model>_allocation()`
([`clm_allocation()`](https://songyosr.github.io/spax/reference/clm_allocation.md),
[`huff_allocation()`](https://songyosr.github.io/spax/reference/huff_allocation.md))
and workflow steps `<verb>_allocation()`.

## Choose the level of operation

| Task | Public interface | What it returns |
|:---|:---|:---|
| Binary catchment FCA | [`spax_2sfca()`](https://songyosr.github.io/spax/reference/spax_2sfca.md) | `spax` result with accessibility raster |
| Distance-weighted FCA | [`spax_e2sfca()`](https://songyosr.github.io/spax/reference/spax_e2sfca.md) | `spax` result with accessibility raster |
| Squared access-side decay FCA | [`spax_m2sfca()`](https://songyosr.github.io/spax/reference/spax_m2sfca.md) | `spax` result with accessibility raster |
| Selection-weighted FCA | [`spax_3sfca()`](https://songyosr.github.io/spax/reference/spax_3sfca.md), [`spax_huff2sfca()`](https://songyosr.github.io/spax/reference/spax_huff2sfca.md) | `spax` result with accessibility raster |
| Supply/demand calculation using supplied kernels | [`compute_access()`](https://songyosr.github.io/spax/reference/compute_access.md) | Accessibility `SpatRaster` |
| Decay, normalization or choice weights | [`calc_decay()`](https://songyosr.github.io/spax/reference/calc_decay.md), [`decay_kernel()`](https://songyosr.github.io/spax/reference/decay_kernel.md), [`calc_normalize()`](https://songyosr.github.io/spax/reference/calc_normalize.md), [`calc_choice()`](https://songyosr.github.io/spax/reference/calc_choice.md) | See the individual input/output contracts |
| Weighted reduction to facilities | [`gather_weighted()`](https://songyosr.github.io/spax/reference/gather_weighted.md), [`gather_demand()`](https://songyosr.github.io/spax/reference/gather_demand.md) | Facility vector/table |
| Weighted distribution to origins | [`spread_weighted()`](https://songyosr.github.io/spax/reference/spread_weighted.md), [`spread_access()`](https://songyosr.github.io/spax/reference/spread_access.md) | Raster result |
| Checked allocation preparation and declaration | [`prepare_allocation()`](https://songyosr.github.io/spax/reference/prepare_allocation.md), [`clm_allocation()`](https://songyosr.github.io/spax/reference/clm_allocation.md), [`huff_allocation()`](https://songyosr.github.io/spax/reference/huff_allocation.md) | Prepared inputs and experimental model |
| Fit, evaluate, predict and bootstrap | [`fit_allocation()`](https://songyosr.github.io/spax/reference/fit_allocation.md), [`evaluate_allocation()`](https://songyosr.github.io/spax/reference/evaluate_allocation.md), [`predict_allocation()`](https://songyosr.github.io/spax/reference/predict_allocation.md), [`bootstrap_allocation()`](https://songyosr.github.io/spax/reference/bootstrap_allocation.md) | Model-specific saved results |
| Check and compare fitted allocations | [`check_allocation()`](https://songyosr.github.io/spax/reference/check_allocation.md), [`compare_allocations()`](https://songyosr.github.io/spax/reference/compare_allocations.md) | Comparison tables; the fits are not changed |
| Travel times and demand inputs | [`calc_travel_time()`](https://songyosr.github.io/spax/reference/calc_travel_time.md), [`calc_demand()`](https://songyosr.github.io/spax/reference/calc_demand.md) | Travel matrix or raster; demand raster or vector |

These function families share concepts such as supply, demand and
facility IDs. Their arguments and result meanings are not
interchangeable. In particular,
[`calc_normalize()`](https://songyosr.github.io/spax/reference/calc_normalize.md)
applied to a numeric matrix normalizes the whole matrix, whereas a
multilayer raster is normalized across layers at each cell. Select the
operation appropriate to the declared axes; do not infer behavior from a
name.

``` r

functions <- c(
  # FCA accessibility
  "spax_2sfca", "spax_e2sfca", "spax_m2sfca", "spax_3sfca", "spax_huff2sfca",
  "compute_access",
  # Experimental allocation workflow
  "prepare_allocation", "clm_allocation", "huff_allocation", "fit_allocation",
  "evaluate_allocation", "predict_allocation", "bootstrap_allocation",
  "check_allocation", "compare_allocations",
  # Input helpers
  "calc_travel_time", "calc_demand", "decay_kernel"
)

# Every name above is exported by the installed package
stopifnot(all(functions %in% getNamespaceExports("spax")))
data.frame(function_name = functions)
```

``` spax-output
          function_name
1            spax_2sfca
2           spax_e2sfca
3           spax_m2sfca
4            spax_3sfca
5        spax_huff2sfca
6        compute_access
7    prepare_allocation
8        clm_allocation
9       huff_allocation
10       fit_allocation
11  evaluate_allocation
12   predict_allocation
13 bootstrap_allocation
14     check_allocation
15  compare_allocations
16     calc_travel_time
17          calc_demand
18         decay_kernel
```

The former iterative-FCA interface is no longer available. It is not an
alias for static CLM or a supported fixed-point workflow. Private
functions beginning with `.` and other unexported implementation
functions are not public contracts; examples for users should not call
them with `:::`.

## Inspect saved results using standard R generics

| Result | Inspection | Interpretation |
|:---|:---|:---|
| FCA `spax` object | [`print()`](https://rdrr.io/r/base/print.html), [`summary()`](https://rspatial.github.io/terra/reference/summary.html), [`plot()`](https://rspatial.github.io/terra/reference/plot.html), `$accessibility` | Saved supply-per-demand index; raster-cell summary |
| Allocation multistart fit | [`coef()`](https://rdrr.io/r/stats/coef.html), [`fitted()`](https://rdrr.io/r/stats/fitted.values.html), [`summary()`](https://rspatial.github.io/terra/reference/summary.html), `as.data.frame(type = "parameters")`, [`plot()`](https://rspatial.github.io/terra/reference/plot.html) | Selected fit plus retained per-start diagnostics |
| Allocation coverage | [`summary()`](https://rspatial.github.io/terra/reference/summary.html), `as.data.frame(type = "regional")` or `type = "cells"` | Saved coverage on retained positive-demand support |
| Allocation bootstrap | [`summary()`](https://rspatial.github.io/terra/reference/summary.html), `as.data.frame(type = "estimates")`, `"draws"`, `"starts"`, [`plot()`](https://rspatial.github.io/terra/reference/plot.html) | Saved point/interval status and every attempted result |

Inspection does not refit the allocation model or resample the
bootstrap. A failed search can still be summarized, but parameter
extraction requires an eligible selected fit. Bootstrap methods preserve
incomplete and refused statuses; displaying a point estimate does not
make an interval available.

FCA rasters and allocation numeric tables have different storage
contracts. Use
[`terra::as.data.frame()`](https://rspatial.github.io/terra/reference/as.data.frame.html)
to extract raster cell values, and preserve geometry separately.
Allocation tables carry IDs; coverage tables also carry unit, support
and reporting attributes. CSV loses those attributes, so retain the
documented RDS bundle when exporting allocation results.

## Keep input validation enabled

Some legacy functions expose `snap`. Leave its default `FALSE` in
ordinary user code. It is an internal optimization switch, not a
different model or a general speed recommendation. New prepared
allocation inputs validate IDs, geometry and support at their own
boundary and do not use a universal `snap` convention.

See [Choosing an FCA index or an allocation
model](https://songyosr.github.io/spax/articles/model-choice.md) for a
runnable comparison, [The E2SFCA calculation step by
step](https://songyosr.github.io/spax/articles/e2sfca-steps.md) for
public building blocks, the [allocation
workflow](https://songyosr.github.io/spax/articles/allocation-workflow.md)
for fitting, prediction, scenarios and persistence, and [Bootstrap
intervals for a fitted allocation
model](https://songyosr.github.io/spax/articles/facility-bootstrap.md)
for intervals.
