# Travel time from each small area to each practice

Motorised travel time in whole minutes from each small area's
population-weighted centroid to each practice's main site.

## Usage

``` r
cornwall_travel
```

## Format

An integer matrix with 337 rows (`cornwall_lsoa$lsoa`) and 55 columns
(`cornwall_gp$practice`). Values are rounded to whole minutes with a
1-minute floor, which covers same-cell and adjacent-cell pairs; about
half of in-area registrations have a value of 1. `NA` marks pairs with
no road connection: the Isles of Scilly area and practice are reachable
only from each other.

## Source

Derived from the Malaria Atlas Project motorised friction surface (Weiss
et al. 2020), licensed under Creative Commons Attribution, and the
locations in
[cornwall_gp](https://songyosr.github.io/spax/reference/cornwall_gp.md)
and
[cornwall_lsoa](https://songyosr.github.io/spax/reference/cornwall_lsoa.md).
See `system.file("COPYRIGHTS", package = "spax")`.

## Details

Times are least-cost paths through the Malaria Atlas Project 2019
motorised friction surface (about 1 km cells), computed with
[`terra::costDist()`](https://rspatial.github.io/terra/reference/costDist.html)
over land in Cornwall and the Isles of Scilly; the sea and the Devon
border act as barriers. Times are measured to the edge of the practice's
cell, so they run slightly (under a minute) shorter than cell-centre to
cell-centre. The road network dates from about 2019, traffic is not
modelled, and 1 km cells are coarse in towns, where several practices
can share a cell. Values are taken from `cornwall_gp_travel.tif` at each
centroid's cell.

## References

Weiss, D. J., Nelson, A., Vargas-Ruiz, C. A., et al. (2020). Global maps
of travel time to healthcare facilities. *Nature Medicine*, 26,
1835-1838.
[doi:10.1038/s41591-020-1059-1](https://doi.org/10.1038/s41591-020-1059-1)

## See also

[cornwall_gp_travel](https://songyosr.github.io/spax/reference/cornwall_gp_travel.md),
[cornwall_flows](https://songyosr.github.io/spax/reference/cornwall_flows.md)

## Examples

``` r
cornwall_travel[1:3, 1:4]
#>           L82001 L82003 L82004 L82006
#> E01018750     67     49     46     78
#> E01018751     67     49     46     78
#> E01018752     68     49     47     79
# Minutes to the nearest practice, mainland areas
summary(apply(cornwall_travel[!cornwall_lsoa$scilly, ], 1, min, na.rm = TRUE))
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#>   1.000   1.000   1.000   2.568   3.000  13.000 
```
