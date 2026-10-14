# spax 0.2.0

## Unreleased changes on codex-assist-main

The changes below are available on `codex-assist-main`. They are not a new stable
release; record the source commit when reproducing an analysis.

* New vignette, "Reach and availability of GP care in Cornwall", fits the
  static CLM to small area × practice registrations and splits access into
  reach and availability.
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
* Allocation probabilities, modeled counts, contact and capacity attribution
  have distinct meanings. Requested predictions distinguish unknown travel
  from absent connections and expose validity and support flags.
* The conditional facility-case bootstrap supports a restricted Gaussian
  model fitted to complete whole facility counts. Its intervals require the
  declared observation assumptions and a complete 399-attempt run; refusals,
  failed attempts and incomplete results remain available for inspection.
* Saved-result methods provide parameter, bootstrap, regional and cell tables,
  summaries and plots without rerunning fitting or resampling. The generated
  allocation article demonstrates paired supply comparison, export and reload.
* Package help now explains compatibility with public FCA and older private
  allocation results. Older objects may require regeneration for current
  prediction or reporting; `huff_decay` results must not be relabeled as
  corrected CLM outputs.
