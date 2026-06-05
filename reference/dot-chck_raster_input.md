# Validate SpatRaster input

Validate SpatRaster input

## Usage

``` r
.chck_raster_input(x, template = NULL, allow_null = FALSE)
```

## Arguments

- x:

  SpatRaster or SpatRaster-coercible object to validate

- template:

  Optional SpatRaster to check compatibility

- allow_null:

  Logical; if TRUE, x can be NULL if template provided

## Value

Invisible TRUE if valid, error otherwise
