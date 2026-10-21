# Package index

## Package Overview and Compatibility

Choose a workflow, preserve input meaning and reuse saved results safely

- [`spax-package`](https://songyosr.github.io/spax/reference/spax-package.md)
  [`allocation-compatibility`](https://songyosr.github.io/spax/reference/spax-package.md)
  : Spatial accessibility and allocation modeling

## Public FCA Accessibility

Raster accessibility surfaces with model-specific normalization and
independent supply indicators

- [`spax_2sfca()`](https://songyosr.github.io/spax/reference/spax_2sfca.md)
  : Calculate Original Two-Step Floating Catchment Area (2SFCA)
  accessibility scores
- [`spax_e2sfca()`](https://songyosr.github.io/spax/reference/spax_e2sfca.md)
  : Calculate Enhanced Two-Step Floating Catchment Area (E2SFCA)
  accessibility scores
- [`spax_m2sfca()`](https://songyosr.github.io/spax/reference/spax_m2sfca.md)
  : Calculate Modified Two-Step Floating Catchment Area (M2SFCA)
  accessibility scores
- [`spax_3sfca()`](https://songyosr.github.io/spax/reference/spax_3sfca.md)
  : Calculate Three-Step Floating Catchment Area (3SFCA) accessibility
  scores
- [`spax_huff2sfca()`](https://songyosr.github.io/spax/reference/spax_huff2sfca.md)
  : Calculate Huff-model Two-Step Floating Catchment Area (Huff-2SFCA)
  scores
- [`compute_access()`](https://songyosr.github.io/spax/reference/compute_access.md)
  : Calculate spatial accessibility using weighted surfaces

## Access Interface (experimental)

Specify a floating catchment area index without data: its decay
(distance weights with a sigma, a step or an inverse decay), its
normalization and its selection. spax() computes it on data from
prep_data(): raster data give the spax object of the one-call functions,
numeric data the index at each origin

- [`spec_2sfca()`](https://songyosr.github.io/spax/reference/spec_e2sfca.md)
  [`spec_e2sfca()`](https://songyosr.github.io/spax/reference/spec_e2sfca.md)
  [`spec_m2sfca()`](https://songyosr.github.io/spax/reference/spec_e2sfca.md)
  [`spec_3sfca()`](https://songyosr.github.io/spax/reference/spec_e2sfca.md)
  [`spec_huff2sfca()`](https://songyosr.github.io/spax/reference/spec_e2sfca.md)
  [`format(`*`<spax_fca_spec>`*`)`](https://songyosr.github.io/spax/reference/spec_e2sfca.md)
  : Specify a floating catchment area index without data
- [`decay_step()`](https://songyosr.github.io/spax/reference/decay_step.md)
  [`decay_inverse()`](https://songyosr.github.io/spax/reference/decay_step.md)
  [`format(`*`<spax_fca_decay>`*`)`](https://songyosr.github.io/spax/reference/decay_step.md)
  [`print(`*`<spax_fca_decay>`*`)`](https://songyosr.github.io/spax/reference/decay_step.md)
  : Declare a step or inverse decay for an FCA specification
- [`format(`*`<spax_access_values>`*`)`](https://songyosr.github.io/spax/reference/spax_access_values-methods.md)
  [`print(`*`<spax_access_values>`*`)`](https://songyosr.github.io/spax/reference/spax_access_values-methods.md)
  [`summary(`*`<spax_access_values>`*`)`](https://songyosr.github.io/spax/reference/spax_access_values-methods.md)
  [`print(`*`<summary.spax_access_values>`*`)`](https://songyosr.github.io/spax/reference/spax_access_values-methods.md)
  [`as.data.frame(`*`<spax_access_values>`*`)`](https://songyosr.github.io/spax/reference/spax_access_values-methods.md)
  : Methods for FCA indices on numeric data

## Allocation Interface (experimental)

Prepare data, specify an allocation model without data (decay kernels,
fitted or held parameters), declare observed counts, run the model and
work with its result

- [`prep_data()`](https://songyosr.github.io/spax/reference/prep_data.md)
  [`format(`*`<ae_prepared_inputs>`*`)`](https://songyosr.github.io/spax/reference/prep_data.md)
  [`print(`*`<ae_prepared_inputs>`*`)`](https://songyosr.github.io/spax/reference/prep_data.md)
  [`format(`*`<spax_raster_data>`*`)`](https://songyosr.github.io/spax/reference/prep_data.md)
  [`print(`*`<spax_raster_data>`*`)`](https://songyosr.github.io/spax/reference/prep_data.md)
  : Prepare data for a spax model
- [`decay_gaussian()`](https://songyosr.github.io/spax/reference/decay_gaussian.md)
  [`decay_exponential()`](https://songyosr.github.io/spax/reference/decay_gaussian.md)
  [`decay_power()`](https://songyosr.github.io/spax/reference/decay_gaussian.md)
  [`decay_custom()`](https://songyosr.github.io/spax/reference/decay_gaussian.md)
  [`format(`*`<spax_decay>`*`)`](https://songyosr.github.io/spax/reference/decay_gaussian.md)
  [`print(`*`<spax_decay>`*`)`](https://songyosr.github.io/spax/reference/decay_gaussian.md)
  : Declare a decay for a model specification
- [`spec_clm()`](https://songyosr.github.io/spax/reference/spec_clm.md)
  [`spec_huff()`](https://songyosr.github.io/spax/reference/spec_clm.md)
  [`format(`*`<spax_allocation_spec>`*`)`](https://songyosr.github.io/spax/reference/spec_clm.md)
  [`format(`*`<spax_spec>`*`)`](https://songyosr.github.io/spax/reference/spec_clm.md)
  [`print(`*`<spax_spec>`*`)`](https://songyosr.github.io/spax/reference/spec_clm.md)
  : Specify an allocation model without data
- [`obs_flows()`](https://songyosr.github.io/spax/reference/obs_flows.md)
  [`obs_totals()`](https://songyosr.github.io/spax/reference/obs_flows.md)
  [`print(`*`<spax_obs>`*`)`](https://songyosr.github.io/spax/reference/obs_flows.md)
  : Declare observed counts for an allocation model
- [`spax()`](https://songyosr.github.io/spax/reference/spax.md) : Run an
  allocation model on prepared data
- [`spax_huff()`](https://songyosr.github.io/spax/reference/spax_huff.md)
  [`spax_clm()`](https://songyosr.github.io/spax/reference/spax_huff.md)
  : Run the Huff or conditional logit model from raw inputs
- [`print(`*`<spax_allocation>`*`)`](https://songyosr.github.io/spax/reference/spax_allocation-methods.md)
  [`summary(`*`<spax_allocation>`*`)`](https://songyosr.github.io/spax/reference/spax_allocation-methods.md)
  [`print(`*`<summary.spax_allocation>`*`)`](https://songyosr.github.io/spax/reference/spax_allocation-methods.md)
  [`coef(`*`<spax_allocation>`*`)`](https://songyosr.github.io/spax/reference/spax_allocation-methods.md)
  [`fitted(`*`<spax_allocation>`*`)`](https://songyosr.github.io/spax/reference/spax_allocation-methods.md)
  [`as.data.frame(`*`<spax_allocation>`*`)`](https://songyosr.github.io/spax/reference/spax_allocation-methods.md)
  [`predict(`*`<spax_allocation>`*`)`](https://songyosr.github.io/spax/reference/spax_allocation-methods.md)
  : Methods for allocation results

## Allocation Steps (experimental)

The step functions under spax(), for full control: prepare inputs,
declare the model, fit, evaluate, predict, check, compare and bootstrap.
Prediction, check, compare and bootstrap also accept a result of spax()

- [`prepare_allocation()`](https://songyosr.github.io/spax/reference/prepare_allocation.md)
  : Prepare numeric inputs for the allocation model
- [`clm_allocation()`](https://songyosr.github.io/spax/reference/clm_allocation.md)
  : Declare corrected static CLM over checked allocation inputs
- [`huff_allocation()`](https://songyosr.github.io/spax/reference/huff_allocation.md)
  : Declare the Huff model over checked allocation inputs
- [`fit_allocation()`](https://songyosr.github.io/spax/reference/fit_allocation.md)
  : Calibrate a declared allocation model, by default from starts found
  in the data
- [`evaluate_allocation()`](https://songyosr.github.io/spax/reference/evaluate_allocation.md)
  : Evaluate a declared allocation model at supplied fixed parameters
- [`predict_allocation()`](https://songyosr.github.io/spax/reference/predict_allocation.md)
  : Predict requested origins against a saved fitted or evaluated CLM
- [`check_fit()`](https://songyosr.github.io/spax/reference/check_fit.md)
  [`check_allocation()`](https://songyosr.github.io/spax/reference/check_fit.md)
  [`print(`*`<ae_allocation_check>`*`)`](https://songyosr.github.io/spax/reference/check_fit.md)
  [`as.data.frame(`*`<ae_allocation_check>`*`)`](https://songyosr.github.io/spax/reference/check_fit.md)
  : Check what fitted facility counts identify in an allocation model
- [`compare_fits()`](https://songyosr.github.io/spax/reference/compare_fits.md)
  [`compare_allocations()`](https://songyosr.github.io/spax/reference/compare_fits.md)
  [`print(`*`<ae_allocation_comparison>`*`)`](https://songyosr.github.io/spax/reference/compare_fits.md)
  [`as.data.frame(`*`<ae_allocation_comparison>`*`)`](https://songyosr.github.io/spax/reference/compare_fits.md)
  : Compare fitted allocations across model and input specifications
- [`bootstrap_fit()`](https://songyosr.github.io/spax/reference/bootstrap_fit.md)
  [`bootstrap_allocation()`](https://songyosr.github.io/spax/reference/bootstrap_fit.md)
  : Conditional facility-case bootstrap for a fitted allocation
- [`coef(`*`<ae_problem_nfxp_fit>`*`)`](https://songyosr.github.io/spax/reference/allocation-fit-methods.md)
  [`coef(`*`<ae_multistart_fit>`*`)`](https://songyosr.github.io/spax/reference/allocation-fit-methods.md)
  [`fitted(`*`<ae_problem_nfxp_fit>`*`)`](https://songyosr.github.io/spax/reference/allocation-fit-methods.md)
  [`fitted(`*`<ae_multistart_fit>`*`)`](https://songyosr.github.io/spax/reference/allocation-fit-methods.md)
  [`summary(`*`<ae_multistart_fit>`*`)`](https://songyosr.github.io/spax/reference/allocation-fit-methods.md)
  : Inspect saved allocation fits without recomputation
- [`as.data.frame(`*`<ae_problem_nfxp_fit>`*`)`](https://songyosr.github.io/spax/reference/allocation-result-tables.md)
  [`as.data.frame(`*`<ae_multistart_fit>`*`)`](https://songyosr.github.io/spax/reference/allocation-result-tables.md)
  [`as.data.frame(`*`<ae_coverage>`*`)`](https://songyosr.github.io/spax/reference/allocation-result-tables.md)
  [`as.data.frame(`*`<ae_allocation_bootstrap>`*`)`](https://songyosr.github.io/spax/reference/allocation-result-tables.md)
  : Inspect saved allocation result tables
- [`print(`*`<ae_allocation_bootstrap>`*`)`](https://songyosr.github.io/spax/reference/allocation-bootstrap-methods.md)
  [`summary(`*`<ae_allocation_bootstrap>`*`)`](https://songyosr.github.io/spax/reference/allocation-bootstrap-methods.md)
  [`print(`*`<summary.ae_allocation_bootstrap>`*`)`](https://songyosr.github.io/spax/reference/allocation-bootstrap-methods.md)
  [`plot(`*`<ae_allocation_bootstrap>`*`)`](https://songyosr.github.io/spax/reference/allocation-bootstrap-methods.md)
  : Inspect a saved facility bootstrap
- [`print(`*`<summary.ae_multistart_fit>`*`)`](https://songyosr.github.io/spax/reference/print.summary.ae_multistart_fit.md)
  : Print a saved allocation multistart summary
- [`plot(`*`<ae_multistart_fit>`*`)`](https://songyosr.github.io/spax/reference/plot.ae_multistart_fit.md)
  : Plot saved diagnostics from the selected allocation fit

## Distance & Decay Functions

Functions for handling distance relationships

- [`calc_decay()`](https://songyosr.github.io/spax/reference/calc_decay.md)
  : Calculate Spatial Distance Decay Weights
- [`decay_kernel()`](https://songyosr.github.io/spax/reference/decay_kernel.md)
  [`print(`*`<spax_decay_kernel>`*`)`](https://songyosr.github.io/spax/reference/decay_kernel.md)
  : Declare a distance decay kernel
- [`calc_choice()`](https://songyosr.github.io/spax/reference/calc_choice.md)
  : Compute Spatial Choice Probabilities
- [`calc_normalize()`](https://songyosr.github.io/spax/reference/calc_normalize.md)
  : Normalize Spatial Weights
- [`calc_travel_time()`](https://songyosr.github.io/spax/reference/calc_travel_time.md)
  : Travel times to facilities over a friction surface
- [`calc_demand()`](https://songyosr.github.io/spax/reference/calc_demand.md)
  : Demand from population groups and group-specific rates

## Data Processing Functions

Functions for processing spatial data

- [`spread_weighted()`](https://songyosr.github.io/spax/reference/spread_weighted.md)
  : Distribute values across weighted surfaces
- [`spread_access()`](https://songyosr.github.io/spax/reference/spread_access.md)
  : Calculate spatial accessibility scores
- [`gather_weighted()`](https://songyosr.github.io/spax/reference/gather_weighted.md)
  : Aggregate weighted values from a raster
- [`gather_demand()`](https://songyosr.github.io/spax/reference/gather_demand.md)
  : Calculate demand captured by each service site
- [`rasterize_demand()`](https://songyosr.github.io/spax/reference/rasterize_demand.md)
  : Convenience wrapper for Bayesian spatial updating
- [`transform_pmf()`](https://songyosr.github.io/spax/reference/transform_pmf.md)
  : Transform input into a probability mass function (PMF)
- [`transform_demand()`](https://songyosr.github.io/spax/reference/transform_demand.md)
  : Transform Case Count Data into Spatial Distribution

## Simulation & Sampling

Spatial probability and demand sampling helpers; distinct from
allocation bootstrap intervals

- [`sample_pmf()`](https://songyosr.github.io/spax/reference/sample_pmf.md)
  : Sample Points from Probability Surface with Multiple Realizations
- [`update_pmf()`](https://songyosr.github.io/spax/reference/update_pmf.md)
  : Update Probability Mass Function with Likelihood Surface
- [`transform_likelihood()`](https://songyosr.github.io/spax/reference/transform_likelihood.md)
  : Transform Input to Likelihood Surface

## FCA Object Methods

Print, summary, and plot methods for public FCA spax objects

- [`print(`*`<spax>`*`)`](https://songyosr.github.io/spax/reference/print.spax.md)
  : Print method for spax objects
- [`summary(`*`<spax>`*`)`](https://songyosr.github.io/spax/reference/summary.spax.md)
  : Summary method for spax objects
- [`print(`*`<summary.spax>`*`)`](https://songyosr.github.io/spax/reference/print.summary.spax.md)
  : Print function for spax summary objects
- [`plot(`*`<spax>`*`)`](https://songyosr.github.io/spax/reference/plot.spax.md)
  : Plot Spatial Accessibility Results

## Utility Functions

Helper functions and data access

- [`read_spax_example()`](https://songyosr.github.io/spax/reference/read_spax_example.md)
  : Read Example Datasets from spax Package
- [`read_spax()`](https://songyosr.github.io/spax/reference/read_spax.md)
  : Read a spax object from disk
- [`save_spax()`](https://songyosr.github.io/spax/reference/save_spax.md)
  : Save a spax accessibility analysis

## Cornwall GP Example Data

Open data for NHS Cornwall and Isles of Scilly GP practices; see
inst/COPYRIGHTS for sources and licences

- [`cornwall_gp`](https://songyosr.github.io/spax/reference/cornwall_gp.md)
  : GP practices in Cornwall and the Isles of Scilly
- [`cornwall_lsoa`](https://songyosr.github.io/spax/reference/cornwall_lsoa.md)
  : Small areas of Cornwall and the Isles of Scilly
- [`cornwall_flows`](https://songyosr.github.io/spax/reference/cornwall_flows.md)
  : Registered patients by small area and practice
- [`cornwall_travel`](https://songyosr.github.io/spax/reference/cornwall_travel.md)
  : Travel time from each small area to each practice
- [`cornwall_pop`](https://songyosr.github.io/spax/reference/cornwall_pop.md)
  : Population grid for Cornwall and the Isles of Scilly
- [`cornwall_gp_travel`](https://songyosr.github.io/spax/reference/cornwall_gp_travel.md)
  : Travel-time grid to each Cornwall GP practice
