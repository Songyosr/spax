# Realize a source field onto the demand side (facility -\> demand)

spread(r, W)\_i = sum_j W_ij r_j. The per-facility source is combined
directly with the weights (terra recycles it per layer – no
lift-to-raster, SPAX-022) and aggregated over the facility axis. Returns
a raster field on domain I.

## Usage

``` r
.ae_spread(source, weights)
```
