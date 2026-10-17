# Small areas of Cornwall and the Isles of Scilly

The 337 Lower layer Super Output Areas (LSOA 2021) of NHS Cornwall and
Isles of Scilly ICB, with population, registered patients and
population-weighted centroids.

## Usage

``` r
cornwall_lsoa
```

## Format

An `sf` polygon object (EPSG:27700) with 337 rows and columns:

- lsoa, name:

  LSOA 2021 code and name; `lsoa` matches `cornwall_flows` and the rows
  of `cornwall_travel`.

- ruc21:

  2021 rural-urban classification.

- pop, pop_0_15, pop_16_64, pop_65p:

  ONS mid-2024 population estimates, all ages and by age band.

- registered:

  Residents registered at any GP practice in England.

- registered_in:

  Residents registered at a Cornwall and Isles of Scilly practice (row
  totals of `cornwall_flows`).

- registered_out:

  Residents registered at a practice elsewhere.

- cent_x, cent_y:

  Population-weighted centroid (EPSG:27700), the origin used for
  `cornwall_travel`.

- scilly:

  `TRUE` for the Isles of Scilly area.

- geometry:

  Super-generalised (200 m) clipped boundary.

## Source

Office for National Statistics: LSOA 2021 boundaries with rural-urban
classification, population-weighted centroids and mid-2024 small-area
population estimates (via Nomis); NHS England registrations (July 2026).
Open Government Licence v3.0; contains OS data. See
`system.file("COPYRIGHTS", package = "spax")`.

## Details

Registered counts exceed the population estimates by about 4%,
reflecting list inflation and the different reference dates.

## See also

[cornwall_gp](https://songyosr.github.io/spax/reference/cornwall_gp.md),
[cornwall_flows](https://songyosr.github.io/spax/reference/cornwall_flows.md),
[cornwall_travel](https://songyosr.github.io/spax/reference/cornwall_travel.md)

## Examples

``` r
areas <- sf::st_drop_geometry(cornwall_lsoa)
summary(areas$registered_in / areas$registered)
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#>  0.6624  0.9965  0.9986  0.9919  0.9994  1.0000 
table(areas$ruc21)
#> 
#>  Larger rural: Further from a major town or city 
#>                                               73 
#>     Larger rural: Nearer to a major town or city 
#>                                                8 
#> Smaller rural: Further from a major town or city 
#>                                               92 
#>    Smaller rural: Nearer to a major town or city 
#>                                               11 
#>         Urban: Further from a major town or city 
#>                                              135 
#>            Urban: Nearer to a major town or city 
#>                                               18 
```
