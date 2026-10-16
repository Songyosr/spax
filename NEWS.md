# spax 0.2.0.9000

The changes below are on the development branch and are not a stable release
yet; record the source commit when reproducing an analysis.

* New experimental `calc_travel_time()` computes continuous least-cost travel
  times from facilities to every cell of an analysis grid over a friction
  surface (such as the Malaria Atlas Project motorized friction surface), with
  optional barriers and a maximum time. The result is the origin-by-facility
  matrix `prepare_allocation()` accepts, with the matching cell index, or one
  raster layer per facility.
* New experimental `compare_allocations()` summarizes several fitted
  allocations side by side (contact, capacity per contact, spread of contact,
  agreement with observed counts and loss) and, with zones, lists the zones
  whose reporting class changes. Loss differences are reported only when the
  observed targets, loss, demand, supply and travel times are identical; travel
  times are verified when the models are supplied. Saved coverage also gains
  `as.data.frame(type = "zones", zones = ...)`, which aggregates cells to zones
  with demand and contacting-demand weights.
* New experimental `check_allocation()` compares a fitted allocation with an
  allocation without distance decay (within the prepared travel support) and
  one that shares counts by capacity alone, on the same loss, and reports the
  one-way trip lengths each implies. When capacity closely tracks the observed
  facility counts, the counts alone may not identify distance decay; the check
  says so instead of leaving it to be discovered. It reports only and never
  changes the fit.
* `summary.spax()` coverage now counts only cells with a value, so cells
  outside the study area (such as sea) no longer lower it, and reports the
  share of those cells with accessibility above zero. It previously divided
  the number of cells with a value by every cell in the raster.
* Printed corrected CLM fits now show `model: clm` instead of the internal
  family name `huff`.
* The `calc_decay()`, `sample_pmf()` and `rasterize_demand()` examples now
  run; the last uses the Cornwall small areas.
* The README and the guides now use the bundled Cornwall GP data, with real
  maps, instead of generated grids. Only the facility bootstrap guide keeps a
  generated network, where the true parameters are known.
* The guides now follow one reader journey. `vignette("spax")` is a short Get
  started guide; a new `vignette("cornwall-data")` introduces the example data
  once, before `data-prep`; the FCA guides are `fca-methods`, `e2sfca-steps`
  and `grid-resolution` (formerly `spax-101` to `spax-104`); and the
  allocation track runs from `model-choice` through `allocation-workflow` to a
  separate `facility-bootstrap` guide. The pkgdown site groups the articles
  under these headings and sets printed output apart from code.

* **Breaking:** the Thailand Health Region 12 example data have been removed
  from the package, because their redistribution terms could not be fully
  confirmed. All examples and tests now use the open Cornwall GP data.

  | Removed | Use instead |
  |---|---|
  | `hos_iscr.tif`, `phc_iscr.tif` (travel time to facilities) | `read_spax_example("cornwall_gp_travel.tif")` |
  | `u5pd.tif` (population grid) | `read_spax_example("cornwall_pop.tif")` |
  | `hc12_hos`, `hc12_phc` (facilities and supply) | `cornwall_gp` |
  | `bound0`, `bound1` (boundaries) | `cornwall_lsoa` |
  | `hos_iscvec` (isochrone polygons) | no direct replacement |

  The package is about 1 MB instead of about 27 MB. Analyses that need the old
  objects should load them from a saved copy of an earlier spax version.
* `transform_pmf()`, `transform_likelihood()` and `transform_demand()` now accept
  SpatVector input. Their column check read a one-column data frame, so every
  column was rejected; the extent check also rejected single and collinear
  points. `transform_demand(full_output = TRUE)` no longer requires an
  undocumented `id` column, and `transform_demand()` now warns when an area
  covers no grid cell, because its count would be dropped.
* Fixes in the FCA and PMF tools:
  * `rasterize_demand()` without a `prior` now uses the documented uniform
    prior over the non-missing cells of the template's first layer. It
    previously used the observations' own PMF, so the posterior was
    proportional to the squared counts. `standardize = TRUE` is now refused,
    because z-scores are not a likelihood, as is an all-missing template.
  * `update_pmf(normalize = FALSE, density = TRUE)` no longer scales the
    posterior twice.
  * Point input to `transform_pmf()`, `transform_likelihood()` and
    `rasterize_demand()` now adds up points that fall in the same cell,
    ignoring missing values; only the last point's value was kept, so counts
    were lost. Polygon values are still copied to every cell they cover.
  * `transform_pmf()`, `update_pmf()` and `rasterize_demand()` accept
    multi-layer rasters and normalize each layer separately; they previously
    failed with "the condition has length > 1". `update_pmf()` requires
    matching layer counts unless one side has a single layer.
  * `sample_pmf()` takes `size` from the rounded total of raw density input,
    as documented, when only `prob` is given; a PMF input still needs `size`.
    Multi-layer input is refused with a clear message.
  * `calc_normalize()` refuses a missing numeric `ref_value` (`NA_real_`,
    `NaN`) with a clear error instead of a base R error.
  * `calc_normalize()` on a SpatRaster accepts a numeric `ref_value` for the
    `"reference"` method; it previously failed inside `terra::ifel()`. The
    documentation now states that rasters are normalized cell by cell
    across layers.
  * `calc_decay()` accepts integer distances such as `1:5`; logical matrices
    and classed numerics such as `units` objects are refused.
  * `summary.spax()` keeps every requested quantile (it kept only the first),
    and custom quantiles print with percentage labels.
  * `sample_pmf()` rejects an `n` that is not a single non-negative whole
    number (also in `snap` mode) instead of returning an empty raster or
    drawing a truncated count, and an all-NA input gets the PMF error
    instead of a base R error.
  * `spax_2sfca()` validates `threshold` with a message that names it; `NA`
    and `NULL`, which gave all-NaN results or the binary default, are refused.
* `calc_normalize()` and `calc_choice()` now refuse negative weights (outside
  `snap` mode); they previously divided by the remaining sum and returned
  negative "probabilities". A custom `calc_normalize()` method still receives
  its input unchecked.
* `clm_allocation()` requires `fit_v0` and `fit_beta` to be `TRUE` or
  `FALSE`; other values such as `"yes"` were silently treated as `FALSE`.
* `?calc_decay` documents each kernel's formula and the `sigma` used when none
  is given, and notes that power decay is infinite at distance 0.
* `fit_allocation()` and `bootstrap_allocation()` no longer force a full
  garbage collection before every fit: the internal fit timer used
  `system.time()`, which collects garbage first by default. Results are
  unchanged; repeated fits run faster (the package tests take half as long).
* The `update_pmf()`, `transform_demand()` and `calc_normalize()` examples now
  run; the first two previously referred to an undefined object.
* The allocation-workflow vignette now loads its full 399-attempt bootstrap
  result from `vignettes/allocation-bootstrap.rds` instead of recomputing it,
  cutting the vignette rebuild from about 200 s to under 30 s. The printed call,
  seed and every reported number are unchanged; the vignette stops if the
  stored result does not match the live fit. Regenerate it with
  `data-raw/precompute-vignette-bootstrap.R` whenever bootstrap or fitting code
  changes. A test now runs the real bootstrap engine for three attempts.
* New open example data for the 55 GP practices of NHS Cornwall and Isles of
  Scilly: `cornwall_gp`, `cornwall_lsoa`, `cornwall_flows`, `cornwall_travel`,
  and the `cornwall_pop.tif` and `cornwall_gp_travel.tif` grids read with
  `read_spax_example()`. All sources are openly licensed (Open Government
  Licence v3.0, Creative Commons Attribution); see `inst/COPYRIGHTS`. The data
  are rebuilt reproducibly by `data-raw/cornwall_gp.R` from pinned sources.
* The supported minimum is R 4.4.0, with testthat 3.2.0 for tests. This is the
  dependency and testing policy for the development branch. CI targets release R
  on Linux, Windows and macOS, plus Linux R-devel and the exact minimum R version.
  Existing public FCA calling semantics are unchanged by this compatibility floor.
* Tutorials now use portable generated examples; live routing is optional and
  is not executed during routine builds. A model-choice guide connects the public
  FCA and experimental allocation workflows. The full allocation example retains
  all 399 bootstrap attempts and its existing statistical reporting rules.
* Removed the unused `bench` and `fasterize` packages from Suggests after the
  portable tutorial conversion.
* Public FCA functions remain supported, including independent accessibility
  indicators from multiple supply columns. Their equations and normalization
  choices are unchanged by the experimental allocation interface.
* Six experimental functions provide a corrected static CLM workflow:
  `prepare_allocation()`, `clm_allocation()`, `fit_allocation()`,
  `evaluate_allocation()`, `predict_allocation()` and `bootstrap_allocation()`.
  Preparation checks IDs, units declarations and support. Fitting retains
  explicit starts and diagnostics; prediction uses saved facility loads.
* Allocation probabilities, modeled counts and supply attribution have
  distinct meanings. Requested predictions distinguish unknown travel
  from absent connections and expose validity and support flags.
* The conditional facility-case bootstrap supports a restricted Gaussian
  model fitted to complete whole facility counts. Its intervals require the
  declared observation assumptions and a complete 399-attempt run; refusals,
  failed attempts and incomplete results remain available for inspection.
* Saved-result methods provide parameter, bootstrap, regional and cell tables,
  summaries and plots without rerunning fitting or resampling. The allocation
  article demonstrates a supply scenario, export and reload.
* Package help now explains compatibility with public FCA and older private
  allocation results. Older objects may require regeneration for current
  prediction or reporting; `huff_decay` results must not be relabeled as
  corrected CLM outputs.
