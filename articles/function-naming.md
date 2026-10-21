# Finding functions and inspecting results

The guides use the bundled Cornwall GP data, described in [The Cornwall
GP example
data](https://songyosr.github.io/spax/articles/cornwall-data.md); this
page needs no data.

``` r

library(spax)
```

Function names follow one rule: the prefix says what a function is for,
and the suffix names the thing.

- [`spax()`](https://songyosr.github.io/spax/reference/spax.md) and
  `spax_<model>()` run an analysis.
  [`spax()`](https://songyosr.github.io/spax/reference/spax.md) runs a
  model specification on prepared data;
  [`spax_huff()`](https://songyosr.github.io/spax/reference/spax_huff.md)
  and
  [`spax_clm()`](https://songyosr.github.io/spax/reference/spax_huff.md)
  run the Huff model and the conditional logit from raw inputs;
  [`spax_2sfca()`](https://songyosr.github.io/spax/reference/spax_2sfca.md),
  [`spax_e2sfca()`](https://songyosr.github.io/spax/reference/spax_e2sfca.md)
  and the other FCA functions compute an FCA index.
- `prep_` prepares data:
  [`prep_data()`](https://songyosr.github.io/spax/reference/prep_data.md).
- `spec_` specifies a model without data:
  [`spec_huff()`](https://songyosr.github.io/spax/reference/spec_clm.md),
  [`spec_clm()`](https://songyosr.github.io/spax/reference/spec_clm.md).
- `decay_` declares a decay kernel:
  [`decay_gaussian()`](https://songyosr.github.io/spax/reference/decay_gaussian.md),
  [`decay_exponential()`](https://songyosr.github.io/spax/reference/decay_gaussian.md),
  [`decay_power()`](https://songyosr.github.io/spax/reference/decay_gaussian.md),
  [`decay_custom()`](https://songyosr.github.io/spax/reference/decay_gaussian.md)
  and
  [`decay_kernel()`](https://songyosr.github.io/spax/reference/decay_kernel.md).
- `obs_` declares observations:
  [`obs_flows()`](https://songyosr.github.io/spax/reference/obs_flows.md),
  [`obs_totals()`](https://songyosr.github.io/spax/reference/obs_flows.md).
- `calc_`, `gather_` and `spread_` are the building blocks.

A model keeps its name as the suffix:
[`spax_huff()`](https://songyosr.github.io/spax/reference/spax_huff.md)
runs the Huff model and
[`spec_huff()`](https://songyosr.github.io/spax/reference/spec_clm.md)
specifies it. The earlier allocation names stay: the model declarations
[`clm_allocation()`](https://songyosr.github.io/spax/reference/clm_allocation.md)
and
[`huff_allocation()`](https://songyosr.github.io/spax/reference/huff_allocation.md)
and the steps `<verb>_allocation()`, such as
[`fit_allocation()`](https://songyosr.github.io/spax/reference/fit_allocation.md),
which [`spax()`](https://songyosr.github.io/spax/reference/spax.md) uses
underneath. The allocation functions are experimental. Existing FCA
calls and result types remain supported.

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
| Run an allocation model in one call | [`spax()`](https://songyosr.github.io/spax/reference/spax.md), [`spax_huff()`](https://songyosr.github.io/spax/reference/spax_huff.md), [`spax_clm()`](https://songyosr.github.io/spax/reference/spax_huff.md) | Experimental allocation result (`spax_allocation`) |
| Prepare allocation data | [`prep_data()`](https://songyosr.github.io/spax/reference/prep_data.md) | Prepared inputs |
| Specify an allocation model without data | [`spec_huff()`](https://songyosr.github.io/spax/reference/spec_clm.md), [`spec_clm()`](https://songyosr.github.io/spax/reference/spec_clm.md) with [`decay_gaussian()`](https://songyosr.github.io/spax/reference/decay_gaussian.md), [`decay_exponential()`](https://songyosr.github.io/spax/reference/decay_gaussian.md), [`decay_power()`](https://songyosr.github.io/spax/reference/decay_gaussian.md) or [`decay_custom()`](https://songyosr.github.io/spax/reference/decay_gaussian.md) | Model specification (`spax_spec`) |
| Declare observed counts | [`obs_flows()`](https://songyosr.github.io/spax/reference/obs_flows.md), [`obs_totals()`](https://songyosr.github.io/spax/reference/obs_flows.md) | Observations and their loss (`spax_obs`) |
| Allocation steps: prepare and declare | [`prepare_allocation()`](https://songyosr.github.io/spax/reference/prepare_allocation.md), [`clm_allocation()`](https://songyosr.github.io/spax/reference/clm_allocation.md), [`huff_allocation()`](https://songyosr.github.io/spax/reference/huff_allocation.md) | Prepared inputs and experimental model |
| Allocation steps: fit, evaluate, predict and bootstrap | [`fit_allocation()`](https://songyosr.github.io/spax/reference/fit_allocation.md), [`evaluate_allocation()`](https://songyosr.github.io/spax/reference/evaluate_allocation.md), [`predict_allocation()`](https://songyosr.github.io/spax/reference/predict_allocation.md), [`bootstrap_allocation()`](https://songyosr.github.io/spax/reference/bootstrap_allocation.md) | Model-specific saved results |
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
  # Allocation interface (experimental)
  "spax", "spax_huff", "spax_clm", "prep_data", "spec_huff", "spec_clm",
  "decay_gaussian", "decay_exponential", "decay_power", "decay_custom",
  "obs_flows", "obs_totals",
  # Allocation steps (experimental)
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
7                  spax
8             spax_huff
9              spax_clm
10            prep_data
11            spec_huff
12             spec_clm
13       decay_gaussian
14    decay_exponential
15          decay_power
16         decay_custom
17            obs_flows
18           obs_totals
19   prepare_allocation
20       clm_allocation
21      huff_allocation
22       fit_allocation
23  evaluate_allocation
24   predict_allocation
25 bootstrap_allocation
26     check_allocation
27  compare_allocations
28     calc_travel_time
29          calc_demand
30         decay_kernel
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
| Allocation result of [`spax()`](https://songyosr.github.io/spax/reference/spax.md) | [`coef()`](https://rdrr.io/r/stats/coef.html), [`fitted()`](https://rdrr.io/r/stats/fitted.values.html), [`summary()`](https://rspatial.github.io/terra/reference/summary.html), [`as.data.frame()`](https://rspatial.github.io/terra/reference/as.data.frame.html) with `type` `"parameters"`, `"facilities"`, `"origins"`, `"flows"` or `"starts"`, [`predict()`](https://rspatial.github.io/terra/reference/predict.html) | The bound model with its fit or evaluation; an output that was not saved is computed when asked for |
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
