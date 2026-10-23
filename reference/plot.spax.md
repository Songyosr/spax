# Plot Spatial Accessibility Results

Plot method for spax objects. This function visualizes the accessibility
surface(s) contained in a spax object using the terra plotting engine.

## Usage

``` r
# S3 method for class 'spax'
plot(x, ...)
```

## Arguments

- x:

  A spax object containing accessibility results

- ...:

  Additional arguments passed to
  [`terra::plot()`](https://rspatial.github.io/terra/reference/plot.html).
  Common options include:

  - y: which layers to plot (number or name)

  - main: title(s) for the plot(s)

  - col: color scheme

  - breaks: break points for the legend

  - legend: logical; whether to show the legend

## Value

The plot is drawn on the current graphics device.

## Details

This is a simple wrapper around
[`terra::plot()`](https://rspatial.github.io/terra/reference/plot.html)
for SpatRaster objects. For full control over plot appearance, including
layer selection, color schemes, legends, and additional map elements,
see the documentation of the terra package (?terra::plot).

## See also

[`terra::plot()`](https://rspatial.github.io/terra/reference/plot.html)
for the complete set of plotting parameters and options

## Examples

``` r
# Load example data
library(terra)
library(sf)
#> Linking to GEOS 3.14.1, GDAL 3.12.2, PROJ 9.7.1; sf_use_s2() is TRUE

# Load data: Cornwall population grid and travel time to each GP practice
pop <- read_spax_example("cornwall_pop.tif")
travel <- read_spax_example("cornwall_gp_travel.tif")

# Calculate accessibility for two GP supply measures
result <- spax_e2sfca(
  demand = pop,
  supply = st_drop_geometry(cornwall_gp),
  distance = travel,
  decay_params = list(method = "gaussian", sigma = 10),
  demand_normalize = "standard",
  id_col = "practice",
  supply_cols = c("gp_fte_extg", "gp_fte")
)

# Basic plot of all layers
plot(result)


# Plot with custom title and color scheme
plot(result, main = c("GPs excluding trainees", "All GPs"),
     col = hcl.colors(100, "YlOrRd"))


# Plot single layer by name
plot(result, y = "gp_fte_extg", main = "Access to GPs")
```
