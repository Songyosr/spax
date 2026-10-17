# Aggregate compact coverage with contact-weighted conditional intensity

`zones` is NULL for the region, or a data frame with unique `origin_id`
keys and nonmissing `zone` labels covering every retained origin.
Spatial objects also accept true raster `cell` keys for compatibility.
If both keys are supplied they must agree. Numeric registry indices are
never cell keys. It may include origins outside the fitted support,
which contribute no records. Input order is irrelevant. Unknown zone
membership is an error, never silently dropped. Floors are applied after
aggregating raw values. All original demand stays in the denominator,
including low-contact/unreachable cells.

## Usage

``` r
.coverage_aggregate(x, zones = NULL)
```
