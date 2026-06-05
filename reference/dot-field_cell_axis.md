# Raster cell axis accessor

Current raster backend contract fixes raster cells as \`I\`. Keep this
behind an accessor so a later cell-axis contract can change one
implementation point.

## Usage

``` r
.field_cell_axis(field)
```
