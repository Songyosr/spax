# Rewrap a coverage value on fitted demand support only

Omitted zero-demand and missing-demand cells stay NA. This does not
evaluate prediction support or impute access outside the stored fit
support. Quadrant codes: 1 both-low, 2 capacity-limited, 3
contact-limited, 4 adequate; unclassified cells are NA. Other output
names are numeric cell-table columns. Numeric-only coverage has no map;
provide a spatial mapping at preparation.

## Usage

``` r
.coverage_surface(x, output = "A")
```
