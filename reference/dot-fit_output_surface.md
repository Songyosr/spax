# Rewrap one final-fit origin-side output onto the demand raster template

Declared axes prevent facility vectors being interpreted as surfaces
when their lengths happen to equal the origin count. Older/custom fits
without axis metadata retain legacy shape-based discovery; see
`.fit_available_surfaces`.

## Usage

``` r
.fit_output_surface(fit, output)
```
