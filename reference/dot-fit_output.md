# Read a saved AE fit output by exact name without recomputation

Preserves the stored value, names and dimensions, including matrix
outputs. Does not bind parameters, solve, scan a raster or materialize
spatial output. Numerical \`allocation\` is the model's allocation
operator, not observed flows; multiplying rows by demand produces
implied flows. \`access\` retains its model-specific meaning (CLM
contact, SAE soft-capped opportunity, or HAAE/Huff-decay attenuated
allocation mass). No semantic alias is applied.

## Usage

``` r
.fit_output(fit, output)
```
