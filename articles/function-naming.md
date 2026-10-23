# Function map

This page helps you find the function for a task and the methods that
read its result. It explains how the names are built, then lists the
functions by task and the methods by result. It runs no analysis. The
guides that use these functions run on [The Cornwall GP example
data](https://songyosr.github.io/spax/articles/cornwall-data.md). The
[`spax()`](https://songyosr.github.io/spax/reference/spax.md) interface
and the allocation model are experimental and may change in later
versions.

## How the names work

The prefix says what a function is for, and the rest names the thing:

- [`spax()`](https://songyosr.github.io/spax/reference/spax.md) runs a
  model specification on prepared data: an FCA (floating catchment area)
  index or an allocation model. `spax_<model>()` runs one model in one
  call from raw inputs:
  [`spax_e2sfca()`](https://songyosr.github.io/spax/reference/spax_e2sfca.md)
  and the other FCA indices,
  [`spax_huff()`](https://songyosr.github.io/spax/reference/spax_huff.md)
  and
  [`spax_clm()`](https://songyosr.github.io/spax/reference/spax_huff.md).
- `prep_` prepares data:
  [`prep_data()`](https://songyosr.github.io/spax/reference/prep_data.md).
- `spec_` specifies a model without data:
  [`spec_clm()`](https://songyosr.github.io/spax/reference/spec_clm.md)
  (the conditional logit model, CLM: facilities plus an outside option,
  the choice of none of them),
  [`spec_huff()`](https://songyosr.github.io/spax/reference/spec_clm.md)
  (the Huff model: facilities only) and the FCA indices
  [`spec_2sfca()`](https://songyosr.github.io/spax/reference/spec_e2sfca.md),
  [`spec_e2sfca()`](https://songyosr.github.io/spax/reference/spec_e2sfca.md),
  [`spec_m2sfca()`](https://songyosr.github.io/spax/reference/spec_e2sfca.md),
  [`spec_3sfca()`](https://songyosr.github.io/spax/reference/spec_e2sfca.md)
  and
  [`spec_huff2sfca()`](https://songyosr.github.io/spax/reference/spec_e2sfca.md),
  compared in [Which FCA
  index?](https://songyosr.github.io/spax/articles/fca-methods.md).
- `decay_` declares how weight falls with travel time:
  [`decay_gaussian()`](https://songyosr.github.io/spax/reference/decay_gaussian.md),
  [`decay_exponential()`](https://songyosr.github.io/spax/reference/decay_gaussian.md),
  [`decay_power()`](https://songyosr.github.io/spax/reference/decay_gaussian.md),
  [`decay_custom()`](https://songyosr.github.io/spax/reference/decay_gaussian.md)
  and
  [`decay_kernel()`](https://songyosr.github.io/spax/reference/decay_kernel.md).
  [`decay_step()`](https://songyosr.github.io/spax/reference/decay_step.md)
  and
  [`decay_inverse()`](https://songyosr.github.io/spax/reference/decay_step.md)
  are for FCA indices only.
- `obs_` declares observed counts:
  [`obs_flows()`](https://songyosr.github.io/spax/reference/obs_flows.md)
  and
  [`obs_totals()`](https://songyosr.github.io/spax/reference/obs_flows.md).
- `calc_`, `gather_` and `spread_` are the building blocks of an index.
  [`calc_travel_time()`](https://songyosr.github.io/spax/reference/calc_travel_time.md)
  and
  [`calc_demand()`](https://songyosr.github.io/spax/reference/calc_demand.md)
  compute inputs.
- `transform_`, `rasterize_`, `update_` and `sample_` move area totals
  onto a grid, and `read_` and `save_` read and write files.
- `<verb>_allocation()` are the steps that
  [`spax()`](https://songyosr.github.io/spax/reference/spax.md) runs
  underneath for an allocation model, such as
  [`fit_allocation()`](https://songyosr.github.io/spax/reference/fit_allocation.md).

A model keeps its name as the suffix:
[`spec_huff()`](https://songyosr.github.io/spax/reference/spec_clm.md)
specifies the Huff model, and
[`spax_huff()`](https://songyosr.github.io/spax/reference/spax_huff.md)
runs it.
[`check_fit()`](https://songyosr.github.io/spax/reference/check_fit.md),
[`compare_fits()`](https://songyosr.github.io/spax/reference/compare_fits.md)
and
[`bootstrap_fit()`](https://songyosr.github.io/spax/reference/bootstrap_fit.md)
work on fitted results: a result of
[`spax()`](https://songyosr.github.io/spax/reference/spax.md), or a
model and its fit from the steps. They are also named
[`check_allocation()`](https://songyosr.github.io/spax/reference/check_fit.md),
[`compare_allocations()`](https://songyosr.github.io/spax/reference/compare_fits.md)
and
[`bootstrap_allocation()`](https://songyosr.github.io/spax/reference/bootstrap_fit.md):
each pair is one function.

## Find a function by task

Every exported function appears on this page, most of them in this
table.

| Task | Functions | What you get |
|:---|:---|:---|
| Read the example grids | [`read_spax_example()`](https://songyosr.github.io/spax/reference/read_spax_example.md) | A population or travel raster |
| Prepare inputs for any model | [`prep_data()`](https://songyosr.github.io/spax/reference/prep_data.md) | Prepared data: numeric inputs, or raster data |
| Spread area totals over a grid | [`transform_pmf()`](https://songyosr.github.io/spax/reference/transform_pmf.md), [`transform_demand()`](https://songyosr.github.io/spax/reference/transform_demand.md) | A grid of shares that sum to 1 ([`transform_pmf()`](https://songyosr.github.io/spax/reference/transform_pmf.md)), or with `density = TRUE` counts that keep each area’s total ([`transform_demand()`](https://songyosr.github.io/spax/reference/transform_demand.md)) |
| Reweight a grid pattern by an area rate | [`rasterize_demand()`](https://songyosr.github.io/spax/reference/rasterize_demand.md), or its parts [`transform_likelihood()`](https://songyosr.github.io/spax/reference/transform_likelihood.md) and [`update_pmf()`](https://songyosr.github.io/spax/reference/update_pmf.md) | A grid of shares or of counts |
| Draw random point patterns from a grid | [`sample_pmf()`](https://songyosr.github.io/spax/reference/sample_pmf.md) | Counts per cell, one layer per draw |
| Compute travel times over a friction surface | [`calc_travel_time()`](https://songyosr.github.io/spax/reference/calc_travel_time.md) | A travel matrix or raster |
| Compute demand from population groups and rates | [`calc_demand()`](https://songyosr.github.io/spax/reference/calc_demand.md) | A demand raster or vector |
| Compute an FCA index | [`spax()`](https://songyosr.github.io/spax/reference/spax.md) with [`spec_2sfca()`](https://songyosr.github.io/spax/reference/spec_e2sfca.md), [`spec_e2sfca()`](https://songyosr.github.io/spax/reference/spec_e2sfca.md), [`spec_m2sfca()`](https://songyosr.github.io/spax/reference/spec_e2sfca.md), [`spec_3sfca()`](https://songyosr.github.io/spax/reference/spec_e2sfca.md) or [`spec_huff2sfca()`](https://songyosr.github.io/spax/reference/spec_e2sfca.md) | On raster data, a `spax` result with the index raster; on numeric data, the index at each origin |
| Compute an FCA index in one call | [`spax_2sfca()`](https://songyosr.github.io/spax/reference/spax_2sfca.md), [`spax_e2sfca()`](https://songyosr.github.io/spax/reference/spax_e2sfca.md), [`spax_m2sfca()`](https://songyosr.github.io/spax/reference/spax_m2sfca.md), [`spax_3sfca()`](https://songyosr.github.io/spax/reference/spax_3sfca.md), [`spax_huff2sfca()`](https://songyosr.github.io/spax/reference/spax_huff2sfca.md) | A `spax` result with the index raster |
| Choose the decay of an FCA index | [`decay_gaussian()`](https://songyosr.github.io/spax/reference/decay_gaussian.md), [`decay_exponential()`](https://songyosr.github.io/spax/reference/decay_gaussian.md) or [`decay_power()`](https://songyosr.github.io/spax/reference/decay_gaussian.md) with a number or a raster `sigma`; [`decay_step()`](https://songyosr.github.io/spax/reference/decay_step.md), [`decay_inverse()`](https://songyosr.github.io/spax/reference/decay_step.md), [`decay_custom()`](https://songyosr.github.io/spax/reference/decay_gaussian.md) | A decay for the specification |
| Build an index from its parts | [`calc_decay()`](https://songyosr.github.io/spax/reference/calc_decay.md), [`calc_normalize()`](https://songyosr.github.io/spax/reference/calc_normalize.md), [`calc_choice()`](https://songyosr.github.io/spax/reference/calc_choice.md), [`gather_weighted()`](https://songyosr.github.io/spax/reference/gather_weighted.md), [`gather_demand()`](https://songyosr.github.io/spax/reference/gather_demand.md), [`spread_weighted()`](https://songyosr.github.io/spax/reference/spread_weighted.md), [`spread_access()`](https://songyosr.github.io/spax/reference/spread_access.md), [`compute_access()`](https://songyosr.github.io/spax/reference/compute_access.md) | Weights, demand per facility and index rasters |
| Save and read an FCA result with its rasters | [`save_spax()`](https://songyosr.github.io/spax/reference/save_spax.md), [`read_spax()`](https://songyosr.github.io/spax/reference/read_spax.md) | Files on disk, and the result read back |
| Specify an allocation model | [`spec_clm()`](https://songyosr.github.io/spax/reference/spec_clm.md), [`spec_huff()`](https://songyosr.github.io/spax/reference/spec_clm.md) with [`decay_gaussian()`](https://songyosr.github.io/spax/reference/decay_gaussian.md), [`decay_exponential()`](https://songyosr.github.io/spax/reference/decay_gaussian.md), [`decay_power()`](https://songyosr.github.io/spax/reference/decay_gaussian.md) or [`decay_custom()`](https://songyosr.github.io/spax/reference/decay_gaussian.md) | A model specification |
| Declare a decay curve of your own | [`decay_kernel()`](https://songyosr.github.io/spax/reference/decay_kernel.md), used through [`decay_custom()`](https://songyosr.github.io/spax/reference/decay_gaussian.md) | A kernel declaration |
| Declare observed counts | [`obs_flows()`](https://songyosr.github.io/spax/reference/obs_flows.md), [`obs_totals()`](https://songyosr.github.io/spax/reference/obs_flows.md) | Observed counts with the loss that fits them |
| Fit an allocation model, or run it at fixed parameter values | [`spax()`](https://songyosr.github.io/spax/reference/spax.md); from raw inputs in one call, [`spax_clm()`](https://songyosr.github.io/spax/reference/spax_huff.md) or [`spax_huff()`](https://songyosr.github.io/spax/reference/spax_huff.md) | An allocation result |
| Predict at other origins | [`predict()`](https://rspatial.github.io/terra/reference/predict.html) on an allocation result | Probabilities, outside shares and flows by origin |
| Check what the data identify | [`check_fit()`](https://songyosr.github.io/spax/reference/check_fit.md) | The fit beside simpler allocations |
| Compare fitted models | [`compare_fits()`](https://songyosr.github.io/spax/reference/compare_fits.md) | One row per fit |
| Put intervals on a fit to facility totals | [`bootstrap_fit()`](https://songyosr.github.io/spax/reference/bootstrap_fit.md) | Estimates, intervals and every attempt |
| Run the allocation steps one at a time | [`prepare_allocation()`](https://songyosr.github.io/spax/reference/prepare_allocation.md), [`clm_allocation()`](https://songyosr.github.io/spax/reference/clm_allocation.md), [`huff_allocation()`](https://songyosr.github.io/spax/reference/huff_allocation.md), [`fit_allocation()`](https://songyosr.github.io/spax/reference/fit_allocation.md), [`evaluate_allocation()`](https://songyosr.github.io/spax/reference/evaluate_allocation.md), [`predict_allocation()`](https://songyosr.github.io/spax/reference/predict_allocation.md) | Prepared inputs, a declared model, a fit, an evaluation or a prediction |

## Read a result

Each result has methods that read it. None of them refits the model or
resamples the bootstrap. An output that a fit did not keep, such as the
flows of a fit to facility totals, is computed when you ask for it.

| Result | Made by | Read it with |
|:---|:---|:---|
| FCA index on a grid (`spax`) | [`spax()`](https://songyosr.github.io/spax/reference/spax.md) on raster data, [`spax_e2sfca()`](https://songyosr.github.io/spax/reference/spax_e2sfca.md) and the other one-call functions | [`print()`](https://rdrr.io/r/base/print.html), [`summary()`](https://rspatial.github.io/terra/reference/summary.html), [`plot()`](https://rspatial.github.io/terra/reference/plot.html); `$accessibility` is the index raster |
| FCA index on numeric data (`spax_access_values`) | [`spax()`](https://songyosr.github.io/spax/reference/spax.md) on numeric data | [`print()`](https://rdrr.io/r/base/print.html), [`summary()`](https://rspatial.github.io/terra/reference/summary.html), [`as.data.frame()`](https://rspatial.github.io/terra/reference/as.data.frame.html); `$access` is a matrix with one row per origin |
| Allocation result (`spax_allocation`) | [`spax()`](https://songyosr.github.io/spax/reference/spax.md), [`spax_clm()`](https://songyosr.github.io/spax/reference/spax_huff.md), [`spax_huff()`](https://songyosr.github.io/spax/reference/spax_huff.md) | [`print()`](https://rdrr.io/r/base/print.html), [`summary()`](https://rspatial.github.io/terra/reference/summary.html), [`coef()`](https://rdrr.io/r/stats/coef.html), [`fitted()`](https://rdrr.io/r/stats/fitted.values.html), [`predict()`](https://rspatial.github.io/terra/reference/predict.html), [`as.data.frame()`](https://rspatial.github.io/terra/reference/as.data.frame.html) with `type` `"parameters"`, `"facilities"`, `"origins"`, `"flows"` or `"starts"` |
| Prediction | [`predict()`](https://rspatial.github.io/terra/reference/predict.html), [`predict_allocation()`](https://songyosr.github.io/spax/reference/predict_allocation.md) | `$outputs` holds what you asked for; `$validity` gives each origin a status, such as `valid`, `unknown_travel` or `known_absence`, saying whether its values could be computed |
| Check | [`check_fit()`](https://songyosr.github.io/spax/reference/check_fit.md) | [`print()`](https://rdrr.io/r/base/print.html), [`as.data.frame()`](https://rspatial.github.io/terra/reference/as.data.frame.html) |
| Comparison | [`compare_fits()`](https://songyosr.github.io/spax/reference/compare_fits.md) | [`print()`](https://rdrr.io/r/base/print.html), [`as.data.frame()`](https://rspatial.github.io/terra/reference/as.data.frame.html) with `type` `"fits"` or `"zones"` |
| Bootstrap | [`bootstrap_fit()`](https://songyosr.github.io/spax/reference/bootstrap_fit.md) | [`print()`](https://rdrr.io/r/base/print.html), [`summary()`](https://rspatial.github.io/terra/reference/summary.html), [`plot()`](https://rspatial.github.io/terra/reference/plot.html), [`as.data.frame()`](https://rspatial.github.io/terra/reference/as.data.frame.html) with `type` `"estimates"`, `"draws"` or `"starts"` |
| Fit from the steps (`ae_multistart_fit`) | [`fit_allocation()`](https://songyosr.github.io/spax/reference/fit_allocation.md) | [`print()`](https://rdrr.io/r/base/print.html), [`summary()`](https://rspatial.github.io/terra/reference/summary.html), [`coef()`](https://rdrr.io/r/stats/coef.html), [`fitted()`](https://rdrr.io/r/stats/fitted.values.html), [`plot()`](https://rspatial.github.io/terra/reference/plot.html), [`as.data.frame()`](https://rspatial.github.io/terra/reference/as.data.frame.html) |

An index raster is read with terra, for example
[`terra::values()`](https://rspatial.github.io/terra/reference/values.html)
or
[`terra::as.data.frame()`](https://rspatial.github.io/terra/reference/as.data.frame.html).
[Reading
results](https://songyosr.github.io/spax/articles/reading-results.md)
says how to save each kind of result.

## Next

- [Getting started](https://songyosr.github.io/spax/articles/spax.md):
  compute and map a first FCA index.
- [Where do residents
  register?](https://songyosr.github.io/spax/articles/allocation-workflow.md):
  the allocation functions in one analysis.
- [Inside the E2SFCA
  calculation](https://songyosr.github.io/spax/articles/e2sfca-steps.md):
  the building blocks at work.
- [Reading
  results](https://songyosr.github.io/spax/articles/reading-results.md):
  what each result means, and what it does not.
