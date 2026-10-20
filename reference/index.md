# Package index

## Package Overview and Compatibility

Choose a workflow, preserve input meaning and reuse saved results safely

- [`spax`](https://songyosr.github.io/spax/reference/spax-package.md)
  [`spax-package`](https://songyosr.github.io/spax/reference/spax-package.md)
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

## Experimental Allocation Workflow

One static CLM system: checked inputs, fitting, saved-load prediction,
supply scenarios and restricted conditional bootstrap

- [`prepare_allocation()`](https://songyosr.github.io/spax/reference/prepare_allocation.md)
  : Prepare checked numeric inputs for an experimental allocation
  workflow
- [`clm_allocation()`](https://songyosr.github.io/spax/reference/clm_allocation.md)
  : Declare corrected static CLM over checked allocation inputs
- [`huff_allocation()`](https://songyosr.github.io/spax/reference/huff_allocation.md)
  : Declare the Huff model over checked allocation inputs
- [`fit_allocation()`](https://songyosr.github.io/spax/reference/fit_allocation.md)
  : Calibrate a declared allocation model from explicit starting points
- [`evaluate_allocation()`](https://songyosr.github.io/spax/reference/evaluate_allocation.md)
  : Evaluate a declared allocation model at supplied fixed parameters
- [`predict_allocation()`](https://songyosr.github.io/spax/reference/predict_allocation.md)
  : Predict requested origins against a saved fitted or evaluated CLM
- [`bootstrap_allocation()`](https://songyosr.github.io/spax/reference/bootstrap_allocation.md)
  : Conditional facility-case bootstrap for a fitted allocation
- [`check_allocation()`](https://songyosr.github.io/spax/reference/check_allocation.md)
  [`print(`*`<ae_allocation_check>`*`)`](https://songyosr.github.io/spax/reference/check_allocation.md)
  [`as.data.frame(`*`<ae_allocation_check>`*`)`](https://songyosr.github.io/spax/reference/check_allocation.md)
  : Check what fitted facility counts identify in an allocation model
- [`compare_allocations()`](https://songyosr.github.io/spax/reference/compare_allocations.md)
  [`print(`*`<ae_allocation_comparison>`*`)`](https://songyosr.github.io/spax/reference/compare_allocations.md)
  [`as.data.frame(`*`<ae_allocation_comparison>`*`)`](https://songyosr.github.io/spax/reference/compare_allocations.md)
  : Compare fitted allocations across model and input specifications
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
