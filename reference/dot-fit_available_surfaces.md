# List saved origin-side fit outputs without constructing maps

Built-in fits carry provider-declared \`output_axes\` identifying
origin, facility and origin-facility outputs. The map accessor respects
these axes even when axis lengths coincide; diagnostics such as
\`pooled\` remain explicit by-name outputs. Unannotated older/custom
fits retain shape-only inference, which cannot distinguish coincident
axes. Regenerate them with declared axes before relying on semantic
surface discovery. This is metadata for inspection, not a recipe
compiler or an \`output_roles\` alias layer.

## Usage

``` r
.fit_available_surfaces(fit)
```
