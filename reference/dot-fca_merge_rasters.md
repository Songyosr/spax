# Combine lists of named rasters, keeping one raster per key

A key that occurs twice must name one and the same raster object (see
\`.same_raster()\`), which is then read once; two different rasters
under one key are an internal error, since a kernel would read the wrong
one.

## Usage

``` r
.fca_merge_rasters(rasters)
```
