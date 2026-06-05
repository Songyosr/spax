# Project a smaller-domain field onto a larger field's slot order

Returns a payload to apply against \`.field_data(big)\`, aligned by
axis-tuple key on the smaller field's domain (broadcast; never by
position): \* big raster, small vector -\> length-nlyr numeric (terra
recycles per layer) \* big raster, small raster -\> small's layers
replicated to big's layer order \* big vector, small vector -\> length-n
numeric in big's node order The smaller field's domain must be a subset
of the larger's.

## Usage

``` r
.ae_project_onto(small, big)
```
