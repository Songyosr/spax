# Reconstruct one saved requested-location prediction vector as a raster

Uses only validated requested-cell geometry and stored vector values.
Other cells remain NA. Matrix outputs require explicit numeric access
and cannot be silently flattened into a surface. This never evaluates
the CLM again.

## Usage

``` r
.prediction_surface(x, output = "A")
```
