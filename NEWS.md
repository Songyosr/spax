# spax 0.2.0

## Unreleased changes on codex-assist-main

The changes below are available on `codex-assist-main`. They are not a new stable
release; record the source commit when reproducing an analysis.

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
