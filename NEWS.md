# spax (development version)

* Version 0.6.0 is archived on Zenodo: cite it as doi:10.5281/zenodo.23538560.
  doi:10.5281/zenodo.23438611 refers to all versions.
* Errors found by the checks of spax name the call you made, such as
  `prep_data(...)` or `predict(fit, ...)`, rather than an internal function you
  never called. The messages are unchanged.
* `missing_travel = "omit"` in `prep_data()`, `prepare_allocation()` and
  `predict_allocation()` leaves out pairs with unknown travel time. It is the new
  name of `"legacy_zero"`, which still works and gives the same results; saved
  data and fits are unchanged. The print of prepared data and the error for
  unknown travel use the new name.

# spax 0.6.0

FCA indices join the experimental interface: `spec_2sfca()`, `spec_e2sfca()`,
`spec_m2sfca()`, `spec_3sfca()` and `spec_huff2sfca()` specify an index without
data, and `spax()` computes it. `prep_data()` now also holds raster data, on which
`spax()` computes FCA indices and fits allocation models. The step functions gain the
general names `check_fit()`, `compare_fits()` and `bootstrap_fit()`, and allocation fits
need less memory and time, with the same results. Calls from 0.5.x keep working and give
the same values; printed results start with new plain headers (see below).

* New experimental FCA specifications. `spec_2sfca(threshold)`, `spec_e2sfca()` and
  `spec_m2sfca()` (with `normalize`), `spec_3sfca()` (with `selection`) and
  `spec_huff2sfca()` (with `selection`, `attractiveness` and `alpha`) are the data-free
  counterparts of `spax_2sfca()` and the other one-call functions. An index is computed,
  not fitted: every decay needs its `sigma`, and `spax()` refuses `observed`, `starts`,
  `lower`, `upper` and fitting controls with an FCA specification. With demand groups,
  `decay` and `selection` can be named lists by group and `threshold` a named vector.
* `spax()` computes FCA indices. On raster data it returns the `spax` object of the
  matching one-call function, with the same `type`, `parameters` and layers; its `call`
  is the `spax()` call. On numeric data (demand, supply and a travel matrix) it returns a
  `spax_access_values`, the index at each active origin with one column per layer,
  computed by the same engine and rules, with `print()`, `summary()` and
  `as.data.frame()` methods. Numeric data has one supply measure, and an origin with zero
  or unknown demand has no row.
* New decays for FCA specifications: `decay_step(threshold)`, a binary catchment, and
  `decay_inverse()`, the weight `1 / (d + 1)`. In an FCA specification,
  `decay_gaussian()`, `decay_exponential()` and `decay_power()` also take a `SpatRaster`
  of `sigma` values, a catchment that varies in space. Allocation specifications refuse
  all three, and numeric data refuses a raster `sigma`.
* `prep_data()` has a raster mode. With a `SpatRaster` as `demand` (or a named list of
  them for demand groups), a travel stack with one layer per facility and a supply table
  named by the new arguments `id_col` and `supply_cols`, it returns a `spax_raster_data`.
  It reads no raster values. The object does not survive `saveRDS()`: `spax()` then asks
  for the data to be prepared again. `id_col` and `supply_cols` come after the existing
  arguments, so positional calls keep working.
* `spax()` fits and evaluates allocation models (`spec_huff()`, `spec_clm()`) on raster
  data. It converts the data to origins on each call and keeps nothing: the cells with
  finite, positive demand are the origins, named by their cell numbers, a missing or
  infinite travel time is an absent pair, and a cell that reaches no facility stays an
  origin (with `spec_clm()` its demand goes to the outside option; `spec_huff()` has no
  outside option and allocates none of it). `obs_totals()` works as it is, and
  `obs_flows()` is keyed by cell number. The model uses one supply measure. The travel of
  the origins is held as a dense matrix: on a synthetic grid with 100,000 origins and 250
  facilities it is 200 MB, the conversion peaks at about 0.5 GB of R memory and a fit to
  facility totals from one start at about 1.6 GB, in about 2.5 minutes (see `?prep_data`).
* New general step names `check_fit()`, `compare_fits()` and `bootstrap_fit()`. They are
  the same functions as `check_allocation()`, `compare_allocations()` and
  `bootstrap_allocation()`, which stay, and differ only in the name that a message gives
  for the step. Given an FCA result, all of them stop: an FCA index has no fit to check,
  compare or bootstrap.
* Printed results of the workflow start with a plain line in place of the class name:
  `<allocation fit> 3 starts, selected scan1` for a fit with several starts and
  `<allocation fit summary>` for its summary, `<allocation fit result>` for one fit,
  `<allocation evaluation>`, `<allocation coverage> gaussian`, `<allocation check>`,
  `<allocation comparison> reference: ...`, `<allocation bootstrap>`, and for FCA results
  `<spax access> E2SFCA, 1 layer on 132 x 223 cells` and
  `<spax access summary> E2SFCA`. Only the printed lines change: classes and fields are
  the same, and saved objects keep working. Legacy SAE, HAAE and `huff_decay` objects
  keep their headers.
* FCA results have the parent class `spax_result`, as allocation results do. The `call`
  of a `spax_2sfca()` result is now the `spax_2sfca()` call; it was the inner
  `spax_e2sfca()` call.
* Allocation specifications from `spec_huff()` and `spec_clm()` now have class
  `c("spax_allocation_spec", "spax_spec")`; they had class `"spax_spec"`. FCA
  specifications have class `c("spax_fca_spec", "spax_spec")`. Test the specific class.
  Specifications and results saved with spax 0.5.0 keep working: `spax()` and
  `print()` take their old class.
* Allocation fits need less memory and time, with the same results: about half the
  memory and less than half the time for a fit to facility totals (one start on
  100,000 origins and 250 facilities: 141 s instead of 318 s).
* Version 0.5.0 is archived on Zenodo: cite it as doi:10.5281/zenodo.23527706.
  doi:10.5281/zenodo.23438611 refers to all versions.

# spax 0.5.0

A new experimental interface fits and evaluates allocation models in one call:
`spax()` takes data from `prep_data()`, a model specified without data with
`spec_huff()` or `spec_clm()`, and observed counts from `obs_flows()` or
`obs_totals()`. `fit_allocation()` now finds starting values and bounds from the
data when they are not given, and known outside counts can be fitted. The step
functions and calls from 0.4.x keep working, and fits that do not use the new
options are unchanged.

* New experimental interface for allocation models. `prep_data()` prepares the demand,
  supply and travel inputs. `spec_huff()` and `spec_clm()` specify a model without data,
  with its decay kernel from `decay_gaussian()`, `decay_exponential()`, `decay_power()` or
  `decay_custom()`. `obs_flows()` and `obs_totals()` declare observed counts and the loss
  that fits them, Poisson by default. `spax(data, model, observed)` fits the model, and
  `spax_huff()` and `spax_clm()` do the same from raw inputs in one call. The step
  functions stay as they are, and `spax()` builds the same objects with them.
* In a specification, a parameter is a number, which holds it, or `"fit"`, which estimates
  it: the decay's `sigma`, the outside weight (`outside`, conditional logit only) and the
  attractiveness exponent `beta`. With demand groups, a named list gives each group its own
  decay or outside weight. When every parameter is a number, `spax()` evaluates the model
  and needs no observations.
* `spax()` returns a `spax_allocation` with `print()`, `summary()`, `coef()`, `fitted()`,
  `as.data.frame()` (parameters, facilities, origins, flows or starts) and `predict()`
  methods. `predict_allocation()`, `check_allocation()`, `compare_allocations()` and
  `bootstrap_allocation()` accept the result in place of the model and its fit. Prepared
  inputs and models declared with `clm_allocation()` or `huff_allocation()` now print a
  short description.
* `fit_allocation()` no longer needs `starts`, `lower` and `upper` for the count outputs.
  Without `starts`, it finds starts and bounds from the data: it probes the decay kernel
  for the range of `sigma` over which it decays across the travel times, scans a grid over
  that range with outside weights seeded from the observed outside share, and fits from the
  best points. `loss` must then be named. Unless you give them, the fit uses analytic
  gradients, when the loss has them, and strict optimizer settings,
  `control = list(maxit = 300, factr = 1e3, pgtol = 1e-2)`: the fit stops when the gradient
  of the loss is below 0.01 per unit of log parameter, which is negligible in likelihood
  terms, or when the loss stops changing at a relative 2e-13. Looser `factr` values (1e5,
  or the optimizer default 1e7) stop early where the loss is nearly flat in the outside
  weight; tighter settings without `pgtol` end starts that reached the optimum with
  line-search failures. The search is stored with the fit and shown when it is printed.
  `spax()` always uses the strict settings. Calls that give `starts`, `lower` and `upper`
  run as before.
* The default lower bound of the outside weight is not below the weight that leaves the
  outside option a millionth of the demand (the seed for an outside share of 1e-6, at the
  best grid point). A fit that wants no outside option now ends on that bound, flagged, and
  the printed result says "v0 at its lower bound: the outside option adds almost nothing
  (compare spec_huff())"; before, the weight drifted to a negligible value without touching
  a bound. Bounds you give are used as given.
* The default search stops with an error when a fitted outside weight has nothing to
  explain, because the counts of a fully observed target reach the total demand, and warns
  when they are within 0.1% of it.
* `spax()` aligns the observations with the data. Counts from origins outside the data,
  excluded from it or with unknown demand are dropped with a message. Flow counts at
  facilities outside the data go to the outside option, or are dropped for the Huff model;
  facility totals at facilities outside the data never enter the fit and are left out, with
  a message. Positive counts that the model cannot produce, from origins with zero demand, at
  facilities with zero supply, on pairs with unknown or infinite travel, or (totals) at
  facilities that no origin can reach, are errors. IDs are matched as text and, when that
  fails, by numeric value, so that `"1e+05"` matches `"100000"`; two observed IDs that
  match one ID of the data are an error.
* The print of `prep_data()` says why origins are not active: with zero demand, unknown
  demand, invalid demand or excluded. `spax_huff()` and `spax_clm()` refuse an argument of
  `prep_data()` given in `...` and name `data_args`. `save_spax()` refuses a `spax()` result
  and names `saveRDS()`.
* Messages about the outside weight, group-specific parameters and the bootstrap now name
  the arguments of the new interface (for example `outside = "fit"` and `obs_totals()` by
  group) beside those of `clm_allocation()`.
* Outside counts are model outputs. A conditional logit model with an outside option
  declares `flow_outside` (`flow` with one more column, `"(outside)"`: each origin's expected
  count at the outside option, so each row adds up to its demand), `utilization_outside`
  (`utilization` with the expected outside count of all origins as one more element) and, with
  demand groups, `utilization_outside_by_group` (one more row per group). `fit_allocation()`
  fits them with the Poisson loss and no new argument: the observed counts carry the outside
  count as the entry `"(outside)"`, `NA` where it is unknown. They need a model with an
  outside option, and `bootstrap_allocation()` refuses them. Fits to the other outputs are
  unchanged. Models and fits saved by earlier versions keep working; bootstrap checkpoints
  written by earlier versions cannot be resumed with a model built by this version.
* `obs_flows(outside = )` and `obs_totals(outside = )` declare observed outside counts, and
  `spax()` then fits the outside outputs above. The default stays `"unknown"`: the fit uses
  the counts at the facilities of the data only. `outside = "known"` takes each origin's
  outside count to be its demand minus its counts at the facilities of the data, for origins
  that the observations mention and that have no `NA` count there; a numeric vector named by
  origin ID gives the counts instead. For facility totals the outside count is one number
  (a vector named by group for totals by group), never derived from the totals. Use
  `"known"` only when demand counts the same people as the observations, for example
  registrations by residence from the same extract and date: it is not a population estimate.
  With `outside = "known"` (each origin's outside count plus its counts equals its demand),
  ungrouped flows and `v0` fitted inside its bounds, the fit is the multinomial likelihood of
  the origins' choices and the fitted total outside count equals the observed one; on
  Cornwall with registered demand, `sigma` is 4.619 and `v0` 0.0486 (4.591 and 0.0273 without
  the outside counts). For totals and demand groups the loss is a Poisson working likelihood
  and no match is claimed. `spax()` refuses a model
  without an outside option (`spec_huff()`), and `compare_allocations()` gives no loss
  difference between fits with and without outside counts. The printed result has a line
  `outside`, `as.data.frame(type = "origins")` adds `outside_observed` and
  `outside_modelled`, and `fitted()` returns the whole target, the `"(outside)"` entry
  included.
* Version 0.4.0 is archived on Zenodo: cite it as doi:10.5281/zenodo.23495125.
  doi:10.5281/zenodo.23438611 refers to all versions.

# spax 0.4.0

FCA methods and the allocation model now run on one numeric engine, and both
accept demand groups that compete for the same facilities. `huff_allocation()`
declares the Huff model. Package descriptions now present spax as a modular
engine for floating catchment methods and allocation models. FCA calls and
saved allocation fits from 0.3.x keep working, except for the FCA inputs that
are now refused (first item below).

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
* FCA functions accept several demand groups (for example age bands or transport modes):
  give `demand` as a named list of single-layer rasters. Each group can have its own decay
  (`decay_params`, `selection_params`, `threshold`) and its own travel times (`distance`, as
  a named list with the same facilities in the same order); all groups compete for the same
  supply. The result has one layer per group and an overall layer, the demand-weighted
  average across groups, in which a group that reaches no facility counts as no access. This
  follows the multi-modal 2SFCA of Tao et al. (2018) and allows group-specific decay. A
  multi-layer demand raster is still refused, now with a message that points to the group
  form.
* The allocation model accepts demand groups (experimental): give `prepare_allocation()`
  demand as a named list by group, and optionally travel as a named list by group.
  `clm_allocation(by_group = )` lets the decay `sigma` (and the outside weight `v0`) differ
  by group, with a family per group if needed, and `fixed = ` holds group values taken from
  other evidence. Group-specific parameters are fitted only against group-labelled targets,
  facility counts by group (`fit_allocation(output = "utilization_by_group")`); with
  aggregated targets they are refused, because simulations and the Cornwall data show that
  aggregated flows cannot tell group decays apart. Evaluation reports pooled outputs, as
  before, and per-group outputs on request (`evaluate_allocation(requested_outputs = )`);
  `compare_allocations()` compares grouped fits. Prediction, `check_allocation()` and the
  bootstrap do not yet accept grouped models.
* New experimental `huff_allocation()` declares the Huff model, the allocation model
  of `clm_allocation()` without an outside option, with power distance decay and a
  fitted attractiveness exponent by default.
* `check_allocation()` no longer stops when the no-decay comparator searches a fitted
  attractiveness exponent `beta` into a range where attractiveness is not finite, which
  can happen for a model without an outside weight.
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
