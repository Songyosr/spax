# Example Primary Health Center Travel Time Isochrones

A GeoTIFF containing travel time isochrones for primary health centers
in Thailand's Health Region 12. The data is stored in \`inst/extdata\`
and should be loaded with \`read_spax_example("phc_iscr.tif")\`.

## Format

A multi-layer GeoTIFF file. Each layer represents travel time to one
primary health center, with layer names restored from the companion
\`phc_iscr_names.rds\` file.

## Source

Computed using OSRM with OpenStreetMap data. Primary health center
locations from
[`hc12_phc`](https://songyosr.github.io/spax/reference/hc12.md).

## Examples

``` r
if (FALSE) { # \dontrun{
phc_iscr <- read_spax_example("phc_iscr.tif")
plot(phc_iscr[[1]], main = "Travel Time to First PHC (minutes)")
} # }
```
