# Hospital Travel Time Isochrones (Vector Format)

A spatial dataset containing travel time isochrone polygons for 77
hospitals in Thailand's Region 12. This vector format provides the
original isochrone bands from which
[`hos_iscr`](https://songyosr.github.io/spax/reference/hos_iscr.md) was
derived.

## Usage

``` r
hos_iscvec
```

## Format

An sf object with 2027 features and 5 fields:

- id:

  Numeric ID for each isochrone band

- isomin:

  Minimum travel time in minutes for the band

- isomax:

  Maximum travel time in minutes for the band

- location_id:

  Hospital identifier matching
  [`hc12_hos`](https://songyosr.github.io/spax/reference/hc12.md)

- iso_mean:

  Mean travel time for the band ((isomin + isomax)/2)

- geometry:

  MULTIPOLYGON geometry in UTM zone 47N

## Source

Computed using OSRM with OpenStreetMap data. Hospital locations from
[`hc12_hos`](https://songyosr.github.io/spax/reference/hc12.md).

## Details

This vector dataset is provided to demonstrate the isochrone generation
process and conversion to raster format. The raster version
([`hos_iscr`](https://songyosr.github.io/spax/reference/hos_iscr.md)) is
recommended for accessibility analysis within the spax package.

## References

Luxen, D., & Vetter, C. (2011). Real-time routing with OpenStreetMap
data. In Proceedings of the 19th ACM SIGSPATIAL International Conference
on Advances in Geographic Information Systems (pp. 513-516).
