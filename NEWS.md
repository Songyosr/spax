# spax (development version)

* The allocation model accepts demand groups (experimental): give `prepare_allocation()`
  demand as a named list by group, and optionally travel as a named list by group.
  `clm_allocation(by_group = )` lets the decay `sigma` (and the outside weight `v0`) differ
  by group, with a family per group if needed, and `fixed = ` holds group values taken from
  other evidence. Group-specific parameters are fitted only against group-labelled targets,
  facility counts by group (`fit_allocation(output = "utilization_by_group")`); with
  aggregated targets they are refused, because simulations and the Cornwall data show that
  aggregated flows cannot tell group decays apart. Evaluation reports pooled outputs, as
  before, and per-group outputs on request (`evaluate_allocation(requested_outputs = )`); `compare_allocations()` compares grouped fits.
  Prediction, `check_allocation()` and the bootstrap do not yet accept grouped models.
* FCA functions accept several demand groups (for example age bands or transport modes):
  give `demand` as a named list of single-layer rasters. Each group can have its own decay
  (`decay_params`, `selection_params`, `threshold`) and its own travel times (`distance`, as
  a named list with the same facilities in the same order); all groups compete for the same
  supply. The result has one layer per group and an overall layer, the demand-weighted
  average across groups, in which a group that reaches no facility counts as no access. This
  follows the multi-modal 2SFCA of Tao et al. (2018) and allows group-specific decay. A
  multi-layer demand raster is still refused, now with a message that points to the group
  form.
* FCA functions (`spax_2sfca()`, `spax_e2sfca()`, `spax_m2sfca()`, `spax_3sfca()`,
  `spax_huff2sfca()` and `compute_access()`) now compute on the same numeric engine as the
  allocation workflow, reading rasters in row blocks. Results are unchanged up to rounding;
  on large grids they are faster (about 4 times on a 600 x 600 grid with 250 facilities) and
  no longer rounded to single precision in temporary files. Decay weights that are infinite
  where travel is finite, such as power decay at distance 0, and negative or infinite demand
  are now refused; they produced meaningless surfaces before. Smaller changes:
  `compute_access()` refuses infinite weights, also with `snap = TRUE`; with `snap = TRUE`,
  negative or infinite demand counts as no demand and a `sigma` vector longer than one is
  refused; facility IDs now always come from the distance layers, so a one-layer `sigma`
  raster or a decay function whose layers keep terra's default names no longer fails with
  "Some weight layer IDs not found"; cells whose weights are all NaN give NA, not NaN.
* Version 0.3.1 is archived on Zenodo: cite it as doi:10.5281/zenodo.23446335.
  doi:10.5281/zenodo.23438611 refers to all versions.

# spax 0.3.1

Patch release. Poisson flow fits are now maximum likelihood estimates; saved
fits from 0.3.0 keep working.

* Poisson fits in `fit_allocation()` now floor modelled counts at 1e-250 instead of 1e-9
  (set by `loss_args = list(eps = ...)`). The old floor capped the loss of positive counts
  that the model puts almost no weight on, such as residents registered far from home, so a
  flow fit was not the Poisson maximum likelihood estimate and moved with the floor: in the
  Cornwall registration guide the Gaussian scale changes from 4.36 to 4.59 minutes and the
  outside weight from 0.0176 to 0.0273. Facility-total estimates are unaffected.
  `fit_allocation()` warns and `check_allocation()` reports cells with positive counts that
  are still modelled below the floor (for example counts on absent edges or at a facility with
  zero supply); `check_allocation()` also reports them for saved fits made with the old floor.
  `compare_allocations()` treats a saved Poisson fit without a stored floor as using 1e-9, and
  compares fits with different floors when neither floor changes either loss.

* Version 0.3.0 is archived on Zenodo: cite it as doi:10.5281/zenodo.23438612.
  doi:10.5281/zenodo.23438611 refers to all versions. `citation("spax")` gives
  the reference.

# spax 0.3.0

First release that includes the experimental allocation workflow. Experimental
functions may change in later versions; record the package version when
reproducing an analysis.

* Fixed: with `loss = "poisson"` and `gradient = TRUE`, the analytic gradient
  is now the gradient of the floored loss. A modelled count below the floor
  (`eps`, 1e-9) adds a constant to the loss, and it now adds nothing to the
  gradient; before, it kept the slope `1 - y / eps`, which misled the
  optimizer on flow targets with positive counts where the model puts almost
  no flow (starts that stopped apart, or line-search failures). Fits whose
  modelled counts all stay above the floor are unchanged; the Poisson loss
  itself is unchanged, so saved fits keep working.
* New experimental `decay_kernel()` declares a distance decay kernel as plain
  data: a built-in kernel by name, or your own function of `distance` and
  `sigma`, kept as the exact text of its code (comments and layout are not kept)
  together with a name, a label for what `sigma` means, bounds on `sigma` and
  fixed extra arguments. The function is run on a grid of distances when it is
  declared, and one that returns weights that are not finite, nonnegative and
  non-increasing is refused. A declaration can replace the method string in
  `calc_decay()` and in the `decay_params` of the FCA functions
  (`spax_e2sfca()`, `spax_m2sfca()`, `spax_3sfca()` and `spax_huff2sfca()`); a
  built-in declaration behaves exactly like its string.
* `clm_allocation()` accepts a declared kernel as `family`. Fitting, evaluation,
  requested-location prediction, `check_allocation()` and `compare_allocations()`
  work with it, and so does `bootstrap_allocation()` when the kernel is smooth
  in `sigma`. A kernel declared with `smooth = FALSE` (a cutoff or a step, for
  example) can be evaluated and used for prediction but not fitted. Results that
  use a declared kernel save its code in a new version of the prediction
  snapshot (`spax_clm_prediction_v2`), so using a saved fit runs that code and
  saved fits should be read only from sources you trust. Snapshots of the
  built-in kernels are unchanged. Printed and summarized fits now label `sigma`
  by its meaning for the kernel, for example `gaussian (sigma = travel scale)`.
* A declared kernel is held to more checks. The function must be elementwise
  (the weight of a distance may not depend on the other distances or on their
  order), because it receives a vector or a matrix of distances. `sigma` must be
  positive, as well as inside the declared bounds, in `calc_decay()`,
  fitting and evaluation, requested-location prediction and when a saved
  snapshot is read ("<name> decay requires positive sigma"). The check at
  declaration also runs the function at a positive `lower` bound and at a finite
  `upper` bound of `sigma`, and `calc_decay()` refuses a negative or non-finite
  weight at a finite distance, which the grid of the declaration cannot rule
  out. A `SpatRaster` is read into memory when a declared kernel is applied to it.
* The finite-difference derivatives that fitting and `bootstrap_allocation()`
  use no longer step outside the bounds of a model's parameters. Next to a
  bound, for example the `lower` bound of a declared kernel, they use a
  one-sided difference. The central step used to leave the bounds there: a fit
  then fell back from its analytic gradient without a message, and
  `bootstrap_allocation()` could stop with an error about `theta` being outside
  its bounds or lose draws that lie on a bound. Results for the built-in kernels
  do not change, except that a parameter closer to zero than one step (about
  `1e-6`) no longer errors. If the score diagnostic of the original fit cannot
  be computed, `bootstrap_allocation()` now refuses intervals with the status
  `original_score_failed` instead of stopping.
* Fitting keeps trial parameter values inside the bounds of the model's
  parameters and the estimate inside the search bounds, also where
  `exp(log(bound))` rounds just outside a bound (for example 7.6). A fit on its
  search bound is then refused by `bootstrap_allocation()` as a boundary
  estimate instead of failing with "invalid saved fit bounds", and a declared
  kernel can be fitted on a declared bound. Other estimates do not change.
* `bootstrap_allocation()` adds a `coverage_note` to its result, and prints it,
  for every model other than built-in Gaussian decay with fitted `sigma` and
  `v0` and `kappa` and `beta` equal to 1, the case the coverage study covered:
  other kernels, a fitted `beta` and other values of `kappa` and `beta` use the
  same procedure without a separate coverage study. Results for the studied case
  are unchanged and have no note.
* New experimental `spax_3sfca()` computes the three-step floating catchment
  area index. Each origin first divides its demand among the facilities with
  selection weights, a facility's share of the origin's decay weights, and both
  steps then use the product of the decay and selection weights. The selection
  weights can use a different decay from the one that weights distance
  (`selection_params`). Because both steps share one kernel, the index returns
  the total supply of the facilities that gather demand.
* New experimental `spax_huff2sfca()` is the same calculation with Huff
  selection probabilities, which also depend on facility attractiveness (by
  default the single supply measure; `alpha` is its exponent). With
  `alpha = 0` it equals `spax_3sfca()`. Several supply measures need an
  explicit `attractiveness`.
* New experimental `calc_demand()` computes demand from population groups
  (for example age bands) and group-specific rates, times a common scale. With
  a total-population grid, each cell's group composition is applied to its
  total, which reconciles gridded group and total products that do not sum
  exactly.
* New experimental `calc_travel_time()` computes continuous least-cost travel
  times from facilities to every cell of an analysis grid over a friction
  surface (such as the Malaria Atlas Project motorized friction surface), with
  optional barriers, a maximum time and bilinear or averaging transfer of the
  friction surface to the grid. The result is the origin-by-facility
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
  says so instead of leaving it to be discovered. A fitted attractiveness
  exponent is re-optimized in both comparators. It reports only and never
  changes the fit.
* The no-decay comparator of `check_allocation()` is exact: it is the same
  model with a constant kernel (weight 1 on every finite travel time), whatever
  kernel the model uses, instead of a built-in kernel at an extreme parameter
  value. Its reported parameters no longer include `sigma`, which that
  allocation does not use: `parameters$no_decay` holds the re-optimized
  parameters other than `sigma`, and the table reads `no decay`, followed by the
  re-optimized `v0` (and `beta`) when the model fits them. Reported losses
  move by at most about 1e-8 relative for travel times in minutes.
* `summary.spax()` coverage now counts only cells with a value, so cells
  outside the study area (such as sea) no longer lower it, and reports the
  share of those cells with accessibility above zero. It previously divided
  the number of cells with a value by every cell in the raster.
* Printed corrected CLM fits now show `model: clm` instead of the internal
  family name `huff`.
* A fit saved with `saveRDS()` and read back is no longer rejected by
  `bootstrap_allocation()` or reported as not like for like by
  `compare_allocations()` when the package keeps source references (in a
  development load, or with `R_KEEP_PKG_SOURCE=yes`). The saved copy of the
  package's Poisson loss function then differed from the original only in those
  references, which the comparison now ignores.
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
  * `print()` of an FCA result whose `decay_params$method` is a function no
    longer fails in `cat()`; it prints `custom function`, and a declared kernel
    prints its name.
  * `print()` of an FCA result whose `decay_params$sigma` is a raster (a variable
    catchment) no longer fails in `sprintf()`; it prints `variable (raster)`.
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
* The conditional facility-case bootstrap supports a static CLM with a fitted
  outside mass, fitted to complete whole facility counts. The decay kernel can
  be built in or declared, if it is smooth in `sigma`, and `beta` can be fixed or
  fitted. Its intervals require the declared observation assumptions and a
  complete 399-attempt run; refusals, failed attempts and incomplete results
  remain available for inspection. Coverage was evaluated for Gaussian decay
  with fitted `sigma` and `v0`; other kernels and a fitted `beta` use the same
  procedure without a separate coverage study.
* Saved-result methods provide parameter, bootstrap, regional and cell tables,
  summaries and plots without rerunning fitting or resampling. The allocation
  article demonstrates a supply scenario, export and reload.
* Package help now explains compatibility with public FCA and older private
  allocation results. Older objects may require regeneration for current
  prediction or reporting; `huff_decay` results must not be relabeled as
  corrected CLM outputs.
