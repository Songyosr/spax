# Population grid for Cornwall and the Isles of Scilly

A WorldPop 2020 population grid (UN-adjusted) clipped to Cornwall and
the Isles of Scilly. The GeoTIFF is bundled in `inst/extdata` and is not
a lazy-loaded object; load it with
`read_spax_example("cornwall_pop.tif")`.

## Format

A one-layer GeoTIFF (`pop`): 132 rows x 271 columns of 30 arc-second
cells (about 1 km; about 0.55 square km at this latitude), EPSG:4326.
Values are estimated residents per cell. Cells crossing the boundary are
weighted by the share of their area inside it; `NA` marks cells outside.
The total (about 543,000) is about 7% below the ONS mid-2024 estimate
used in
[cornwall_lsoa](https://songyosr.github.io/spax/reference/cornwall_lsoa.md)
(585,655); do not mix the two unknowingly.

## Source

WorldPop (2018), Global High Resolution Population Denominators Project,
2020 UN-adjusted 1 km estimates for the United Kingdom,
[doi:10.5258/SOTON/WP00671](https://doi.org/10.5258/SOTON/WP00671) ,
Creative Commons Attribution 4.0. See
`system.file("COPYRIGHTS", package = "spax")`.

## See also

[cornwall_gp_travel](https://songyosr.github.io/spax/reference/cornwall_gp_travel.md)

## Examples

``` r
pop <- read_spax_example("cornwall_pop.tif")
terra::global(pop, "sum", na.rm = TRUE)
#>        sum
#> pop 542666
```
