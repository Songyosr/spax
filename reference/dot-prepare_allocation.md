# Prepare named numeric allocation inputs for existing AE providers

Demand is a numeric vector, supply a nonnegative finite vector, and
distance an origin-by-facility numeric matrix. Demand may also be a
named list of named numeric vectors (demand groups, SPAX-057), and
distance a named list of matrices by group; see
`.prepare_allocation_groups()`. Canonical IDs must be complete, unique
and nonblank. Named axes match independently; unnamed axes are
positional. An unnamed `included` mask follows the supplied demand order
and is reordered with demand. A named mask instead matches canonical
origin IDs independently.

## Usage

``` r
.prepare_allocation(
  demand,
  supply,
  distance,
  origin_ids = NULL,
  facility_ids = NULL,
  included = NULL,
  missing_demand = c("error", "exclude"),
  missing_travel = c("error", "legacy_zero"),
  spatial = NULL,
  metadata = list()
)
```

## Details

Positive included demand enters load support; zero, unknown and
explicitly excluded origins retain distinct registry statuses. Only
active travel rows are validated and retained. Positive infinity is an
absent edge; missing travel errors unless `legacy_zero` is explicitly
selected. Negative travel is invalid. Model-specific finite-distance
domains are checked at binding.

Optional `spatial` is a list with a one-layer raster `template` and a
complete `cell_index` mapping, optionally named by origin ID. The
mapping is distinct from registry order. Only plain geometry is saved,
so serialization does not depend on a live raster pointer or source
file. Metadata declares demand and supply units/periods and travel
units; unspecified fields remain NA and no unit conversion is performed.
The result is private `ae_prepared_inputs`.
