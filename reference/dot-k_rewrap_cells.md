# Write a value vector back onto a raster template

\`kept_cell_index = NULL\` means \`values\` already covers every
template cell (the FCA access surface is defined on the full grid). When
the output itself is compacted, pass the kept cell indices and unfilled
cells become NA.

## Usage

``` r
.k_rewrap_cells(values, template, kept_cell_index = NULL)
```
