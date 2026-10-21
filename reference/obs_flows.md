# Declare observed counts for an allocation model

`obs_flows()` declares observed origin-facility counts and
`obs_totals()` observed facility counts, in total or by demand group.
Each returns a `spax_obs`: the counts together with the loss used to fit
to them (and its arguments). The loss is part of the observation, so a
model specification carries none. An observation holds no data.
[`spax()`](https://songyosr.github.io/spax/reference/spax.md) aligns the
observations with the data: it matches the IDs, reorders the counts into
the order of the data and drops or refuses what the data cannot hold.

## Usage

``` r
obs_flows(
  x,
  origin = NULL,
  facility = NULL,
  count = NULL,
  loss = c("poisson", "weighted_sse"),
  loss_args = list(),
  outside = "unknown"
)

obs_totals(
  x,
  facility = NULL,
  count = NULL,
  group = NULL,
  loss = c("poisson", "weighted_sse"),
  loss_args = list(),
  outside = "unknown"
)

# S3 method for class 'spax_obs'
print(x, ...)
```

## Arguments

- x:

  For `obs_flows()`, a numeric matrix of origin by facility counts or a
  data frame of origin, facility and count. For `obs_totals()`, a named
  numeric vector (or one-dimensional array or table) of facility counts,
  a facility by group matrix, or a data frame of facility and count, or
  of facility, group and count. For the
  [`print()`](https://rdrr.io/r/base/print.html) method, a `spax_obs`.

- origin, facility, count, group:

  Names of the columns of a data frame `x` holding the origin IDs,
  facility IDs, counts and group IDs. Leave all `NULL` to read the
  columns by position (see Details). They apply to data frames only.

- loss:

  `"poisson"` (default) or `"weighted_sse"`: the loss that fits a model
  to these counts.

- loss_args:

  A named list of arguments for the loss. It is stored and passed on
  unchanged.

- outside:

  The observed outside counts (see Details). For `obs_flows()`:
  `"unknown"` (the default), `"known"` (each origin's demand minus its
  counts at the facilities of the data) or a numeric vector named by
  origin ID. For `obs_totals()`: `"unknown"`, one number (facility
  totals) or a numeric vector named by group ID (totals by group). Used
  with the Poisson loss only, and by a model with an outside option
  ([`spec_clm()`](https://songyosr.github.io/spax/reference/spec_clm.md)).

- ...:

  Not used.

## Value

An object of class `spax_obs`: a list with `type` (`"flow"`,
`"utilization"` or `"utilization_by_group"`, the kind of counts), `x` (a
double matrix, a named double vector, or for flows given as a data
frame, a frame with columns `origin`, `facility` and `count`), `loss`,
`loss_args` and `outside` (`"unknown"`, `"known"`, or the given outside
counts). With outside counts,
[`spax()`](https://songyosr.github.io/spax/reference/spax.md) compares
the model with the output `"flow_outside"`, `"utilization_outside"` or
`"utilization_outside_by_group"` instead of `type` (see
[`fit_allocation()`](https://songyosr.github.io/spax/reference/fit_allocation.md)).

## Details

**IDs.** IDs from data frames are converted to character: factors with
[`as.character()`](https://rdrr.io/r/base/character.html), numbers with
`format(scientific = FALSE)`, so that 100000 stays `"100000"` rather
than `"1e+05"`. Dimnames and names are used as given.

**Counts** are finite and nonnegative, or `NA` for a count that was not
observed (an `NA` cell is left out of the loss). Fractional counts are
allowed.

**Flows.** `x` is a numeric matrix with row names (origin IDs) and
column names (facility IDs), or a data frame with one row per
origin-facility pair. A `table` or `xtabs` object is read as a matrix. A
frame with exactly three columns and no column arguments is read as
origin, facility, count; otherwise name the columns. In a long table a
pair that does not appear has count 0; duplicate pairs are summed,
ignoring `NA`, and a pair whose counts are all `NA` stays `NA`.

**Totals.** `x` is a named numeric vector, a one-dimensional array or
table (from [`tapply()`](https://rdrr.io/r/base/tapply.html) or
[`table()`](https://rdrr.io/r/base/table.html)), or a data frame of
facility and count (two columns by default). These are facility totals.
A matrix with facility row names and group column names, or a data frame
of facility, group and count (three columns by default), gives totals by
demand group. Duplicate facilities in a frame are summed as for flows;
duplicate names in a vector or matrix are an error.
[`tapply()`](https://rdrr.io/r/base/tapply.html) over a factor with
unused levels gives `NA` for those facilities, which reads as
unobserved, not as 0: use
[`table()`](https://rdrr.io/r/base/table.html), or replace the `NA` by 0
if the facility had no counts.

**What the data decides.** When the observations are aligned with the
data: counts from origins outside the data, excluded from it or with
unknown demand are dropped with a message. For flows, counts at
facilities outside the data are the outside option of the model
(dropped, with a message, for a model without one). For totals, counts
at facilities outside the data never enter the fit and are left out,
with a message: there is no origin to send them to. Positive counts from
origins with zero demand, at facilities with zero supply, on pairs with
unknown or infinite travel, or (for totals) at facilities that no origin
can reach are errors, because the model cannot produce them. Flows
missing for an active origin or a facility of the data are taken as 0,
with a message; totals must give every facility of the data (use 0, or
`NA` if unobserved).

IDs match the data as text. An ID that matches nothing is then matched
by its numeric value when exactly one ID of the data has that value,
with a message: R writes the number 100000 as `"1e+05"`, and codes read
as numbers lose leading zeros (`1234` matches `"01234"`).

**Outside counts.** The outside option of a conditional logit model
([`spec_clm()`](https://songyosr.github.io/spax/reference/spec_clm.md))
takes the demand of the active origins that goes to facilities outside
the data or to none. By default (`outside = "unknown"`) a fit uses the
counts at the facilities of the data only. `outside` declares the
observed counts of the outside option, which then become one more cell
per origin of a flow fit (one more element of the totals, one more row
of the totals by group) and are fitted with the Poisson loss, so
`outside` is refused with another loss. A model without an outside
option
([`spec_huff()`](https://songyosr.github.io/spax/reference/spec_clm.md),
or [`spec_clm()`](https://songyosr.github.io/spax/reference/spec_clm.md)
with `outside = 0`) cannot carry them:
[`spax()`](https://songyosr.github.io/spax/reference/spax.md) refuses
the combination.

- `obs_flows(outside = "known")` takes the outside count of an origin to
  be its demand minus its counts at the facilities of the data,
  `y0 = D - sum(y)`. Counts of the origin at facilities outside the data
  are part of `y0`. It is computed for an active origin that appears in
  the observations and has no `NA` count at the facilities of the data.
  Any other active origin has an unknown outside count (`NA`) and is
  fitted on its facility counts alone, as without outside counts. Counts
  that exceed the demand of an origin by more than 1e-8 of it are an
  error; a smaller excess is set to 0.

- `obs_flows(outside = y0)`, with `y0` a numeric vector named by origin
  ID, gives the outside counts: each name an active origin of the data
  once (the names of origins of the data that are not active are dropped
  with a message, any other name is an error), values nonnegative or
  `NA`. An active origin that is not named has an unknown outside count.

- `obs_totals(outside = y0)` gives the outside count of facility totals
  as one number, and of totals by group as a numeric vector named by
  group ID (all groups of the data, `NA` for an unknown one). It is
  never derived from the totals: it is the demand of the data's active
  origins that went to facilities outside the data or to none. Totals
  often include people from outside the data (list sizes, for instance),
  and a difference would be biased.

Declare outside counts as known only when the demand counts the same
people as the observations, for example registrations by residence from
the same extract and date, where every resident is registered somewhere.
They are not a population estimate: list sizes and population differ by
several percent, and the gap goes straight into the outside count.

For an origin whose outside count is known and adds up, with its
facility counts, to its demand (`y0 + sum(y) = D`, as for `"known"`),
the Poisson loss on its cells is the multinomial likelihood of its
choices up to constants. When this holds for every origin of an
ungrouped flow fit and `v0` is fitted inside its search bounds, the
fitted total outside count equals the observed one. That is not claimed
for totals or for demand groups: there the total outside count is not
guaranteed to match. Fits with and without outside counts are fits to
different data, so
[`compare_allocations()`](https://songyosr.github.io/spax/reference/compare_allocations.md)
gives no loss difference between them: compare models on the same
observation, and compare the Huff model
([`spec_huff()`](https://songyosr.github.io/spax/reference/spec_clm.md))
with a conditional logit model on an observation with
`outside = "unknown"`.

## See also

Other allocation interface:
[`decay_gaussian()`](https://songyosr.github.io/spax/reference/decay_gaussian.md),
[`prep_data()`](https://songyosr.github.io/spax/reference/prep_data.md),
[`spax()`](https://songyosr.github.io/spax/reference/spax.md),
[`spax_allocation-methods`](https://songyosr.github.io/spax/reference/spax_allocation-methods.md),
[`spax_huff()`](https://songyosr.github.io/spax/reference/spax_huff.md),
[`spec_clm()`](https://songyosr.github.io/spax/reference/spec_clm.md)

## Examples

``` r
# Origin-facility flows from a long table of registrations
flows <- obs_flows(cornwall_flows, origin = "lsoa", facility = "practice",
                   count = "patients")
flows
#> <spax obs> flows: 3,188 origin-facility counts, total 601,269; loss poisson

# Facility totals, from the same table
totals <- tapply(cornwall_flows$patients, cornwall_flows$practice, sum)
obs_totals(totals)
#> <spax obs> totals: 55 facility counts, total 601,269; loss poisson

# The same flows with the outside counts known: demand counts the residents
# registered somewhere, so each origin's outside count is its demand minus
# its counts at the facilities of the data (used by a model with an outside
# option, see spax())
obs_flows(cornwall_flows, origin = "lsoa", facility = "practice",
          count = "patients", outside = "known")
#> <spax obs> flows: 3,188 origin-facility counts, total 601,269; loss poisson; outside counts known
```
