# Read Example Datasets from spax Package

Lists and loads example datasets included with the spax package. When
called without arguments, lists available datasets. When given a
filename, loads that specific dataset.

## Usage

``` r
read_spax_example(dataset = NULL)
```

## Arguments

- dataset:

  Character. Name of dataset to load. If NULL (default), lists available
  files.

## Value

If dataset is NULL, returns character vector of available files. If
dataset is specified, returns the loaded dataset as a SpatRaster.

## Examples

``` r
# List available datasets
read_spax_example()
#> [1] "cornwall_gp_travel.tif" "cornwall_pop.tif"      

# Load a specific dataset: travel time to each Cornwall GP practice
travel <- read_spax_example("cornwall_gp_travel.tif")
names(travel)[1:3]
#> [1] "L82001" "L82003" "L82004"
```
