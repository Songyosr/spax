# Validate SpatVector input

Validate SpatVector input

## Usage

``` r
.chck_vector_input(x, value_col = NULL, template = NULL, allow_null = FALSE)
```

## Arguments

- x:

  SpatVector to validate

- value_col:

  Character name of value column

- template:

  SpatRaster template for rasterization

- allow_null:

  Logical; if TRUE, value_col can be NULL

## Value

Invisible TRUE if valid, error otherwise
