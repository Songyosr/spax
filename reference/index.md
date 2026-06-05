# Package index

## Core Accessibility Functions

Main functions for spatial accessibility analysis

- [`spax_2sfca()`](https://songyosr.github.io/spax/reference/spax_2sfca.md)
  : Calculate Original Two-Step Floating Catchment Area (2SFCA)
  accessibility scores
- [`spax_e2sfca()`](https://songyosr.github.io/spax/reference/spax_e2sfca.md)
  : Calculate Enhanced Two-Step Floating Catchment Area (E2SFCA)
  accessibility scores
- [`spax_m2sfca()`](https://songyosr.github.io/spax/reference/spax_m2sfca.md)
  : Calculate Modified Two-Step Floating Catchment Area (M2SFCA)
  accessibility scores
- [`compute_access()`](https://songyosr.github.io/spax/reference/compute_access.md)
  : Calculate spatial accessibility using weighted surfaces

## Distance & Decay Functions

Functions for handling distance relationships

- [`calc_decay()`](https://songyosr.github.io/spax/reference/calc_decay.md)
  : Calculate Spatial Distance Decay Weights
- [`calc_choice()`](https://songyosr.github.io/spax/reference/calc_choice.md)
  : Compute Spatial Choice Probabilities
- [`calc_normalize()`](https://songyosr.github.io/spax/reference/calc_normalize.md)
  : Normalize Spatial Weights

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

Functions for spatial simulation

- [`sample_pmf()`](https://songyosr.github.io/spax/reference/sample_pmf.md)
  : Sample Points from Probability Surface with Multiple Realizations
- [`update_pmf()`](https://songyosr.github.io/spax/reference/update_pmf.md)
  : Update Probability Mass Function with Likelihood Surface
- [`transform_likelihood()`](https://songyosr.github.io/spax/reference/transform_likelihood.md)
  : Transform Input to Likelihood Surface

## Object Methods

Print, summary, and plot methods for spax objects

- [`print(`*`<spax>`*`)`](https://songyosr.github.io/spax/reference/print.spax.md)
  : Print method for spax objects
- [`summary(`*`<spax>`*`)`](https://songyosr.github.io/spax/reference/summary.spax.md)
  : Summary method for spax objects
- [`print(`*`<summary.spax>`*`)`](https://songyosr.github.io/spax/reference/print.summary.spax.md)
  : Print function for spax summary objects
- [`plot(`*`<spax>`*`)`](https://songyosr.github.io/spax/reference/plot.spax.md)
  : Plot Spatial Accessibility Results
- [`print(`*`<spax_field>`*`)`](https://songyosr.github.io/spax/reference/print.spax_field.md)
  : Print a spax_field

## Utility Functions

Helper functions and data access

- [`read_spax_example()`](https://songyosr.github.io/spax/reference/read_spax_example.md)
  : Read Example Datasets from spax Package
- [`read_spax()`](https://songyosr.github.io/spax/reference/read_spax.md)
  : Read a spax object from disk
- [`save_spax()`](https://songyosr.github.io/spax/reference/save_spax.md)
  : Save a spax accessibility analysis

## Package Data

Documentation for included datasets

- [`u5pd`](https://songyosr.github.io/spax/reference/u5pd.md) : Under-5
  Population Density in Thailand's Health Region 12
- [`hos_iscr`](https://songyosr.github.io/spax/reference/hos_iscr.md) :
  Example Isochrone Data for Thailand Health Region 12
- [`hos_iscvec`](https://songyosr.github.io/spax/reference/hos_iscvec.md)
  : Hospital Travel Time Isochrones (Vector Format)
- [`phc_iscr`](https://songyosr.github.io/spax/reference/phc_iscr.md) :
  Example Primary Health Center Travel Time Isochrones
- [`hc12`](https://songyosr.github.io/spax/reference/hc12.md)
  [`hc12_phc`](https://songyosr.github.io/spax/reference/hc12.md)
  [`hc12_hos`](https://songyosr.github.io/spax/reference/hc12.md) :
  Health Facilities in Thailand's Region 12
- [`bound0`](https://songyosr.github.io/spax/reference/bound0.md) :
  Thailand Health Region 12 Boundary Area - Boundary
- [`bound1`](https://songyosr.github.io/spax/reference/bound1.md) :
  Thailand Region 12 ADM1 Boundaries with Attributes
