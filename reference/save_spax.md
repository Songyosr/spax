# Save a spax accessibility analysis

Saves the result of a floating catchment analysis (an object of class
\`spax\`) with its rasters. A result of \[spax()\] (a
\`spax_allocation\`) is saved with \[saveRDS()\].

## Usage

``` r
save_spax(x, file, dir = FALSE, overwrite = FALSE, ...)
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

  Additional arguments passed to terra::writeRaster()

## Value

Invisibly returns the input object
