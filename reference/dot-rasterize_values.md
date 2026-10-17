# Rasterize vector values onto a template

Points that fall in the same cell add up (missing values are ignored,
and each part of a multipoint carries the feature's full value); a
polygon's value is copied to every cell it covers.

## Usage

``` r
.rasterize_values(x, template, value_col)
```
