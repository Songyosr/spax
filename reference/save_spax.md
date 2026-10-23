# Save a spax accessibility analysis

Saves the result of a floating catchment analysis (an object of class
`spax`) with its rasters. A result of
[`spax()`](https://songyosr.github.io/spax/reference/spax.md) (a
`spax_allocation`) is saved with
[`saveRDS()`](https://rspatial.github.io/terra/reference/serialize.html).

## Usage

``` r
save_spax(x, file, dir = FALSE, overwrite = FALSE, ..., datatype = "FLT8S")
```

## Arguments

- x:

  A spax object

- file:

  Character; path to save the object (without extension)

- dir:

  Logical; whether to create a directory with same name for files
  (default FALSE)

- overwrite:

  Logical; whether to overwrite existing files/directory (default FALSE)

- ...:

  Additional arguments passed to
  [`terra::writeRaster()`](https://rspatial.github.io/terra/reference/writeRaster.html)

- datatype:

  Character; the data type of the raster file, passed to
  [`terra::writeRaster()`](https://rspatial.github.io/terra/reference/writeRaster.html)
  (give it by name). The default `"FLT8S"` (double precision) keeps the
  values:
  [`read_spax()`](https://songyosr.github.io/spax/reference/read_spax.md)
  returns them as they were. `"FLT4S"` (single precision, terra's own
  default) writes files about half the size, with values rounded to
  about seven significant digits (a relative change of up to 6e-8).

## Value

Invisibly returns the input object

## Examples

``` r
# E2SFCA on the Cornwall population grid, with a Gaussian decay of 10 minutes
pop <- read_spax_example("cornwall_pop.tif")
travel <- read_spax_example("cornwall_gp_travel.tif")
result <- spax_e2sfca(pop, sf::st_drop_geometry(cornwall_gp), travel,
                      decay_params = list(method = "gaussian", sigma = 10),
                      id_col = "practice", supply_cols = "gp_fte_extg")

# Save it to temporary files: the object, and its accessibility raster
path <- tempfile("cornwall_e2sfca")
save_spax(result, path)
basename(list.files(dirname(path), pattern = basename(path)))
#> [1] "cornwall_e2sfca1ce98845704_accessibility.tif"
#> [2] "cornwall_e2sfca1ce98845704_spax.rds"         

# Read it back, then remove the files
read_spax(path)
#> <spax access> E2SFCA, 1 layer on 132 x 271 cells
#> 
#> Spatial Properties:
#> - Resolution: 0.00833333 x 0.00833333
#> - Dimensions: 132 x 271
#> 
#> Measures: 1
#> - gp_fte_extg
#> 
#> Facilities: 55
#> 
#> Parameters:
#> - Decay function: gaussian (sigma = 10)
#> - Demand normalization: identity 
unlink(paste0(path, c("_spax.rds", "_accessibility.tif")))
```
