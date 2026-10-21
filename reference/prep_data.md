# Prepare data for an allocation model

Experimental. `prep_data()` checks and aligns the demand, supply and
travel time inputs of an allocation model once, so that model
specifications such as
[`spec_clm()`](https://songyosr.github.io/spax/reference/spec_clm.md)
and
[`spec_huff()`](https://songyosr.github.io/spax/reference/spec_clm.md)
can be bound to them with
[`spax()`](https://songyosr.github.io/spax/reference/spax.md). In this
version it is
[`prepare_allocation()`](https://songyosr.github.io/spax/reference/prepare_allocation.md)
under another name, with `travel` for `distance`: it returns the
identical object, an `ae_prepared_inputs`, and every argument is passed
through. The step functions
([`clm_allocation()`](https://songyosr.github.io/spax/reference/clm_allocation.md)
and the others) accept the object too.

## Usage

``` r
prep_data(
  demand,
  supply,
  travel,
  origin_ids = NULL,
  facility_ids = NULL,
  included = NULL,
  missing_demand = c("error", "exclude"),
  missing_travel = c("error", "legacy_zero"),
  spatial = NULL,
  metadata = list()
)

# S3 method for class 'ae_prepared_inputs'
format(x, ...)

# S3 method for class 'ae_prepared_inputs'
print(x, ...)
```

## Arguments

- demand:

  Numeric origin vector, finite and nonnegative or explicitly permitted
  missing values. Negative or infinite demand is rejected even on
  excluded origins. Named demand matches canonical origin IDs. For
  demand groups, a named list by group of such named vectors with
  identical name sets (any order); the group names must be unique,
  non-empty, not `NA`, not `"all"` and without `"."`. A matrix is
  refused.

- supply:

  Finite nonnegative facility vector. Known zero supply is retained;
  unknown supply is rejected. Names match canonical facility IDs.

- travel:

  Numeric origin-by-facility travel matrix. It is the `distance`
  argument of
  [`prepare_allocation()`](https://songyosr.github.io/spax/reference/prepare_allocation.md):
  the same alignment by IDs applies, and with demand groups it can be
  one matrix shared by the groups or a named list of matrices with one
  element per group.

- origin_ids, facility_ids:

  Complete unique nonblank canonical IDs. Their order determines
  prepared order. The default (`NULL`) takes the names of the demand
  vector (of the first group for grouped demand) and the column names of
  the travel matrix; identities are never inferred from equal axis
  lengths.

- included:

  Nonmissing logical origin mask (default: all `TRUE`). Names align
  independently; an unnamed mask follows supplied demand order (that of
  the first group for grouped demand) and is reordered with demand.

- missing_demand:

  Default `"error"` rejects included missing demand. `"exclude"` omits
  it from loads while recording unknown demand, separately from known
  zero or explicit exclusion. Positive included demand remains active
  even when every inside edge is absent.

- missing_travel:

  Default `"error"` rejects NA/NaN on active fit rows. `"legacy_zero"`
  explicitly omits unknown edges and records that policy. Finite
  nonnegative travel is known, positive infinity is an absent edge, and
  negative travel is invalid. Omitted rows are validated when requested
  by prediction. Model-specific finite domains, such as positive power
  distances, are checked when binding parameters; no imputation bypasses
  them.

- spatial:

  Optional list with a one-layer
  [`terra::SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
  `template` and a complete unique in-range `cell_index` vector,
  optionally named by origin ID. Registry positions are not raster
  cells. Only plain geometry and the aligned mapping are saved. Numeric
  work needs no spatial metadata.

- metadata:

  Optional named list of scalar strings or NA for `demand_units`,
  `demand_period`, `supply_units`, `supply_period`, and `travel_units`.
  Omitted fields remain NA. These are caller declarations; no unit
  conversion or dimensional compatibility check is performed.

- x:

  Prepared inputs, an `ae_prepared_inputs`.

- ...:

  Unused.

## Value

The existing `ae_prepared_inputs`, as returned by
[`prepare_allocation()`](https://songyosr.github.io/spax/reference/prepare_allocation.md).
[`format()`](https://rdrr.io/r/base/format.html) returns the printed
lines as a character vector and
[`print()`](https://rdrr.io/r/base/print.html) returns its argument
invisibly.

## Details

Printing the object lists its origins and facilities, the demand groups,
the unit labels when they were given, the active origins whose travel is
unknown and the policies for missing demand and travel.

## See also

Other allocation interface:
[`decay_gaussian()`](https://songyosr.github.io/spax/reference/decay_gaussian.md),
[`obs_flows()`](https://songyosr.github.io/spax/reference/obs_flows.md),
[`spax()`](https://songyosr.github.io/spax/reference/spax.md),
[`spax_allocation-methods`](https://songyosr.github.io/spax/reference/spax_allocation-methods.md),
[`spax_huff()`](https://songyosr.github.io/spax/reference/spax_huff.md),
[`spec_clm()`](https://songyosr.github.io/spax/reference/spec_clm.md)

## Examples

``` r
# Mainland Cornwall: registered residents, GP FTE and travel minutes
areas <- sf::st_drop_geometry(cornwall_lsoa[!cornwall_lsoa$scilly, ])
gp <- sf::st_drop_geometry(cornwall_gp[!cornwall_gp$scilly, ])
demand <- stats::setNames(areas$registered, areas$lsoa)
supply <- stats::setNames(gp$gp_fte_extg, gp$practice)
travel <- cornwall_travel[areas$lsoa, gp$practice]
prep_data(demand, supply, travel,
          metadata = list(demand_units = "registered residents",
                          supply_units = "GP FTE", travel_units = "minutes"))
#> <spax data> 336 origins (336 active) x 54 facilities
#>   demand      604,471 registered residents
#>   supply      GP FTE
#>   travel      minutes; unknown for 0 active origins
#>   policies    missing demand: error; missing travel: error
```
