# Travel-time grid to each Cornwall GP practice

Motorised travel time in whole minutes from every populated cell of
`cornwall_pop.tif` to each of the 55 practices, one layer per practice.
The GeoTIFF is bundled in `inst/extdata`; load it with
`read_spax_example("cornwall_gp_travel.tif")`, which restores the layer
names from `cornwall_gp_travel_names.rds`.

## Format

A 55-layer GeoTIFF on the `cornwall_pop.tif` grid (132 rows x 271
columns, EPSG:4326). Layer names are practice codes
(`cornwall_gp$practice`). Values are rounded to whole minutes with a
1-minute floor; `NA` marks unpopulated cells, pairs without a road
connection (mainland cells to the Isles of Scilly practice and back, and
off-island Scilly cells that cannot reach the St Mary's practice) and
journeys over 180 minutes.

## Source

Derived from the Malaria Atlas Project motorised friction surface (Weiss
et al. 2020, Creative Commons Attribution) and
[cornwall_gp](https://songyosr.github.io/spax/reference/cornwall_gp.md).
See `system.file("COPYRIGHTS", package = "spax")`.

## Details

Computed as for
[cornwall_travel](https://songyosr.github.io/spax/reference/cornwall_travel.md);
see its details for method and limitations.

## See also

[cornwall_travel](https://songyosr.github.io/spax/reference/cornwall_travel.md),
[cornwall_pop](https://songyosr.github.io/spax/reference/cornwall_pop.md)

## Examples

``` r
travel <- read_spax_example("cornwall_gp_travel.tif")
travel
#> class       : SpatRaster
#> size        : 132, 271, 55  (nrow, ncol, nlyr)
#> resolution  : 0.008333333, 0.008333333  (x, y)
#> extent      : -6.40125, -4.142917, 49.85792, 50.95792  (xmin, xmax, ymin, ymax)
#> coord. ref. : lon/lat WGS 84 (EPSG:4326)
#> source      : cornwall_gp_travel.tif
#> names       : L82001, L82003, L82004, L82006, L82007, L82008, ...
#> min values  :      1,      1,      1,      1,      1,      1, ...
#> max values  :     89,     99,     91,    100,    107,    132, ...
```
