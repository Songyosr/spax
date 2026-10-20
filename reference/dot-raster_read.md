# Read one block of rows from every raster of a list

Returns a named list of numeric matrices with \`n \* ncol\` rows (cell
index of row k is \`(start - 1) \* ncol + k\`) and one column per layer.
Each distinct raster is read once and shared with its aliases.

## Usage

``` r
.raster_read(rasters, same, start, n, only = names(same))
```
