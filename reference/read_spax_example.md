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
#> [1] "hos_iscr.tif" "phc_iscr.tif" "u5pd.tif"    

# Load a specific dataset
hos_iscr <- read_spax_example("hos_iscr.tif")
```
