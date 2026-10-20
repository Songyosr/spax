# Row blocks for reading a list of rasters

Plans the blocks without opening anything. \`same\` maps each element to
the name of its first element that is the same raster object, so a
raster passed twice is read (and counted) once. A block holds at most
\`max_values\` values over the distinct rasters, and at least one row.

## Usage

``` r
.raster_blocks(rasters, max_values = getOption("spax.fca_block_values", 2e+06))
```

## Arguments

- rasters:

  Named list of SpatRasters with the same number of rows and columns.

- max_values:

  Values per block over all layers read.

## Value

\`list(starts, n, ncol, same)\`: first row and number of rows of each
block, columns per row, and the alias map.
