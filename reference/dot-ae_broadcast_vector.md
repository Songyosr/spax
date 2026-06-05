# Project a vector field onto a raster's layer order as a length-nlyr numeric

Returns one value per raster layer, aligned by axis-tuple key (never by
position). This is what lets \`raster \<op\> vector\` use terra's
per-layer recycling instead of materializing the vector as a raster
stack (SPAX-022): the lift/\`(data\*0)+v\` step is replaced by a plain
numeric the op recycles.

## Usage

``` r
.ae_broadcast_vector(vector_field, raster_field)
```

## Details

The vector's domain must be a subset of the raster's layer axes
(broadcast). An axis present only on the vector would grow the raster
(outer product), which is not supported here and errors clearly.

Footgun guard: terra recycles a numeric per layer ONLY when its length
is exactly nlyr; any other length silently recycles per cell. The length
is asserted before it can reach terra arithmetic.
