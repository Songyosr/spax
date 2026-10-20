# Open the distinct rasters of a list for block reading

Returns a function that closes them again. Call it as \`close \<-
.raster_open(rasters, same); on.exit(close(), add = TRUE)\`; if one
raster fails to open, the ones already opened are closed.

## Usage

``` r
.raster_open(rasters, same)
```
