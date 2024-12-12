# spax: Spatial Accessibility Analysis in R

<!-- badges: start -->
<!-- badges: end -->

## Overview

`spax` is an R package designed for advanced spatial accessibility analysis, with a primary focus on Two-Step Floating Catchment Area (2SFCA) methods and their derivatives. The package emphasizes a raster-based approach to spatial analysis, offering efficient computation and flexible spatial resolution handling.

### Key Features

- **Modular Design**: Core functions are organized into independent, composable components that can be combined for custom analysis workflows
- **Raster-Based Analysis**: Optimized for working with spatial rasters (`SpatRaster`) for efficient handling of large spatial datasets
- **Monte Carlo Simulation**: Tools for uncertainty analysis and stochastic modeling of spatial accessibility
- **Flexible Distance Decay**: Multiple built-in decay functions with customizable parameters:
  - Gaussian decay
  - Exponential decay
  - Power decay
  - Binary (threshold) decay

### Core Components

1. **Data Preparation**
   - Population density processing
   - Service location handling
   - Distance/travel time calculations
   - Weight matrix generation

2. **Accessibility Calculation**
   - Supply-side ratio computation
   - Demand weighting
   - Spatial accessibility scoring
   - Result normalization

3. **Monte Carlo Analysis**
   - Stochastic population sampling
   - Multi-iteration accessibility calculations
   - Statistical summaries and uncertainty quantification

## Installation

You can install the development version of spax from [GitHub](https://github.com/) with:

```r
# install.packages("pak")
pak::pak("Songyosr/spax")
```

## Package Status

This package is currently under active development. While the core functionality for raster-based spatial accessibility analysis is implemented, additional features and improvements are ongoing:

- Enhanced documentation and vignettes
- Additional catchment area methods
- Vector-based analysis support (planned)
- Performance optimizations
- Extended Monte Carlo capabilities

## Dependencies

Core dependencies:
- terra
- sf
- tidyverse
- parallel

## Contributing

Contributions are welcome! Please feel free to submit a Pull Request. For major changes, please open an issue first to discuss what you would like to change.

## License

This project is licensed under the MIT License - see the LICENSE file for details.
