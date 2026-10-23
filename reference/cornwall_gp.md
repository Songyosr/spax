# GP practices in Cornwall and the Isles of Scilly

The 55 general practices of NHS Cornwall and Isles of Scilly Integrated
Care Board (ICB code QT6) with their GP workforce and registered lists.
Use `gp_fte_extg` as supply in examples: it excludes GPs in training,
whose recording changed in June 2018 and who cluster in training
practices.

## Usage

``` r
cornwall_gp
```

## Format

An `sf` point object in WGS 84 longitude and latitude (EPSG:4326), the
coordinate system of the bundled rasters, with 55 rows and columns:

- practice:

  NHS ODS practice code; matches `cornwall_flows`, the columns of
  `cornwall_travel` and the layers of `cornwall_gp_travel.tif`.

- name, postcode, pcn:

  Practice name, main-site postcode and Primary Care Network.

- gp_fte_extg:

  Full-time equivalent GPs excluding GPs in training (1 FTE = 37.5 hours
  a week).

- gp_fte:

  All GPs, including GPs in training and locums.

- gp_fte_extgl:

  GPs excluding GPs in training and locums.

- nurse_fte:

  Full-time equivalent nurses (may be `NA`).

- list_size:

  Registered patients from all areas.

- list_in_area:

  Registered patients living in Cornwall and the Isles of Scilly.

- fte_source:

  Whether the workforce record includes estimates.

- scilly:

  `TRUE` for the practice on the Isles of Scilly.

- easting, northing:

  British National Grid coordinates (EPSG:27700) of the main-site
  postcode, in metres.

- geometry:

  Main-site location from the postcode, the origin of
  `cornwall_gp_travel.tif`.

## Source

NHS England, Patients Registered at a GP Practice (July 2026) and
General Practice Workforce (30 June 2026), Open Government Licence v3.0.
Locations from the ONS Postcode Directory (November 2025). See
`system.file("COPYRIGHTS", package = "spax")`.

## Details

Registrations are a July 2026 snapshot and workforce a June 2026
snapshot. Only the main site of each practice is represented; branch
surgeries are not. All the bundled data use the WGS 84 datum, so they
combine without a change of datum; for map units in metres project them
to a system on that datum, such as WGS 84 / UTM zone 30N (EPSG:32630).
British National Grid uses another datum (OSGB36): the geometry was
converted from it with the OSGB36 to WGS 84 Helmert transformation
(about 2 m), and `easting`/`northing` keep its coordinates.

## See also

[cornwall_lsoa](https://songyosr.github.io/spax/reference/cornwall_lsoa.md),
[cornwall_flows](https://songyosr.github.io/spax/reference/cornwall_flows.md),
[cornwall_travel](https://songyosr.github.io/spax/reference/cornwall_travel.md),
[cornwall_gp_travel](https://songyosr.github.io/spax/reference/cornwall_gp_travel.md)

## Examples

``` r
gp <- sf::st_drop_geometry(cornwall_gp)
head(gp[, c("practice", "name", "gp_fte_extg", "list_size")])
#>   practice                                name gp_fte_extg list_size
#> 1   L82001             LANDER MEDICAL PRACTICE   10.298887     19477
#> 2   L82003                  PORT ISAAC SURGERY    4.693333      8738
#> 3   L82004 WADEBRIDGE & CAMEL ESTUARY PRACTICE    4.341333      8045
#> 4   L82006                      PENRYN SURGERY    7.973333     20089
#> 5   L82007            CAMELFORD MEDICAL CENTRE    2.888800      6658
#> 6   L82008             STRATTON MEDICAL CENTRE    4.670400     10746
# Registered patients per GP FTE (excluding GPs in training)
summary(gp$list_size / gp$gp_fte_extg)
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#>   247.9  1587.9  1866.9  2069.2  2244.9  7515.9 
```
