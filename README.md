
<!-- README.md is generated from README.Rmd. Please edit that file -->

# spax: Spatial Accessibility Analysis in R

<!-- badges: start -->
<!-- badges: end -->

`spax` is an R package designed for advanced spatial accessibility
analysis, focusing on Two-Step Floating Catchment Area (2SFCA) methods
and their derivatives. The package offers a fresh perspective on spatial
accessibility through its raster-based computational approach and
modular design.

## Features

- **Computational Simplicity**: Raster-based operations translate
  complex spatial relationships into simple matrix calculations
- **Modular Design**: Functions work like LEGO bricks - mix and match
  components to build custom analysis workflows
- **Complex Demand Handling**: Support for continuous population
  surfaces, letting you work with high-resolution demand data
- **Monte Carlo Integration**: Built-in tools for uncertainty analysis
  and stochastic demand modeling

## Installation

You can install the development version of spax from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("Songyosr/spax")
```

## Example

Here’s how spax’s modular design works in practice, analyzing healthcare
accessibility in Thailand’s Region 12:

``` r
library(spax)
library(terra)
library(sf)

# Load example data (already included in package)
pop <- read_spax_example("u5pd.tif") # Under-5 population density
distance <- read_spax_example("hos_iscr.tif") # Travel times
hospitals <- hc12_hos # Hospital locations and capacity

# Calculate accessibility using Enhanced 2SFCA
result <- spax_e2sfca(
  demand = pop, # Population density
  supply = hospitals |> st_drop_geometry(), # Hospital capacity
  distance = distance, # Travel times
  decay_params = list(
    method = "gaussian",
    sigma = 30 # 30-minute characteristic distance
  ),
  demand_normalize = "standard", # Prevent demand inflation
  id_col = "id",
  supply_cols = c("s_doc", "s_nurse") # Analyze both doctors and nurses
)

# Plot results
plot(result,
  main = c("Access to Doctors", "Access to Nurses"),
  fun = function() lines(bound0)
)
```

<img src="man/figures/README-example-1.png" width="100%" />

``` r
result
#> Spatial Accessibility Analysis (E2SFCA)
#> 
#> Spatial Properties:
#> - Resolution: 520.404 x 520.404
#> - Dimensions: 509 x 647
#> 
#> Measures: 2
#> - s_doc
#> - s_nurse
#> 
#> Facilities: 77
#> 
#> Parameters:
#> - Decay function: gaussian (sigma = 30)
#> - Demand normalization: standard
summary(result)
#> 
#> Summary of Spatial Accessibility Analysis (E2SFCA)
#> --------------------------------------------- 
#> 
#> Accessibility Measures:
#> 
#> s_doc:
#>   Range: 2.14e-10 to 0.1818
#>   Mean (SD): 0.06002 (0.03767)
#>   Quantiles:
#>     Min.:    2.14e-10
#> 
#>   Coverage: 33.2% (109362 of 329323 cells)
#> 
#> s_nurse:
#>   Range: 4.172e-10 to 0.2736
#>   Mean (SD): 0.1066 (0.06332)
#>   Quantiles:
#>     Min.:    4.172e-10
#> 
#>   Coverage: 33.2% (109362 of 329323 cells)
#> 
#> Facility Statistics:
#> -------------------
#> Total Facilities: 77
#> 
#> s_doc:
#>   Total Supply: 4669
#>   Mean (SD): 60.64 (80.09)
#>   Range: 0 to 522
#> 
#> s_nurse:
#>   Total Supply: 8125
#>   Mean (SD): 105.5 (159.1)
#>   Range: 0 to 973
```

This example demonstrates key features of spax:

- Working with continuous population surfaces (demand)

- Handling multiple supply indicators simultaneously

- Gaussian distance decay for more realistic accessibility modeling

## Getting Started

Check out our vignettes for detailed guidance:

## See Also

- [Getting Started with
  spax](https://songyosr.github.io/spax/articles/spax-101-intro.html)
- [Data Preparation
  Guide](https://songyosr.github.io/spax/articles/spax-102-data-prep.html)
- [Dissecting spax: Understanding the package’s inner
  workings](https://songyosr.github.io/spax/articles/spax-103-dissection.html)
- [Understanding Raster
  Trade-offs](https://songyosr.github.io/spax/articles/spax-104-raster-tradeoff.html)

## Contributing

Truth is, I’m just getting started with spax. I’d love to have you on
board to help shape the future of spatial accessibility analysis in R.
Please feel free to submit a Pull Request. For major changes, please
open an issue first to discuss what you would like to change.

## License

This project is licensed under the MIT License - see the LICENSE file
for details.
