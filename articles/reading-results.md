# Reading results

This page says what the numbers from spax mean, and what they do not.
Each section answers one question in a few sentences and links to the
guide that shows it in code. The examples use [The Cornwall GP example
data](https://songyosr.github.io/spax/articles/cornwall-data.md). The
[`spax()`](https://songyosr.github.io/spax/reference/spax.md) interface
and the allocation model are experimental and may change in later
versions.

``` r

library(spax)
library(terra)
library(sf)
library(dplyr)
```

## What an FCA index measures

A floating catchment area (FCA) index is the supply around a place per
unit of the demand that competes for it, with nearer facilities counting
for more. Each facility divides its supply (here GP full-time
equivalents, FTE) by the demand around it (here residents), weighted by
travel time, and each place adds up the ratios of the facilities around
it. The result, for example GP FTE per 1,000 residents, measures
potential access: it does not say where anyone goes or how much care
they receive. In the enhanced two-step FCA (E2SFCA), the two steps above
with travel-time weights, residents times the index add up to the total
supply of the practices that have at least one resident within reach.
[Which FCA
index?](https://songyosr.github.io/spax/articles/fca-methods.md) shows
how much supply each index hands back.

## What the allocation model says

The allocation model, a conditional logit model (CLM), gives each origin
a probability for each facility and for an outside option, the choice of
none of the modeled facilities, and these add up to 1. They are
probabilities under a model that knows only travel time and supply.
Demand times probability gives modeled counts. In Cornwall the outside
option is registration with a practice outside mainland Cornwall, in
Devon for example. Registrations are a stock: who is registered where in
July 2026. The model therefore describes where people are registered,
not how often they see a GP. The code counts the registered mainland
residents and those registered at a mainland practice.

``` r

# Mainland small areas and practices, as plain tables
areas <- cornwall_lsoa |>
  filter(!scilly) |>
  st_drop_geometry()
gp <- cornwall_gp |>
  filter(!scilly) |>
  st_drop_geometry()

# Registered mainland residents, and those registered at mainland practices
at_mainland <- cornwall_flows |>
  filter(lsoa %in% areas$lsoa, practice %in% gp$practice)
registered <- c(
  all = sum(areas$registered),
  at_mainland_practices = sum(at_mainland$patients)
)
registered
```

``` spax-output
                  all at_mainland_practices 
               604471                598974 
```

Only 0.9% of registered mainland residents are registered elsewhere. The
outside option describes them: it is registration elsewhere, not unmet
need. [Where do residents
register?](https://songyosr.github.io/spax/articles/allocation-workflow.md)
fits the model.

## What a fit shows, and what it does not

A fit shows which parameter values bring the modeled counts closest to
the observed ones. Another decay curve can fit almost as well, so
compare curves with
[`compare_fits()`](https://songyosr.github.io/spax/reference/compare_fits.md),
which sets such fits side by side.
[`check_fit()`](https://songyosr.github.io/spax/reference/check_fit.md)
sets a fit beside simpler allocations on the same counts: one with no
distance decay and, for facility totals, one in proportion to supply
alone. If a simpler allocation fits almost as well, the counts say
little about travel time. This is common with facility totals, which
show how many people each facility serves but not where they live. The
fit searches from several starting values, and when they reach the same
estimate the search is stable. That does not prove the estimate is the
best possible, or that the data pin it down (identify it). [Where do
residents
register?](https://songyosr.github.io/spax/articles/allocation-workflow.md)
runs these checks.

## Decay: chosen or fitted

In an FCA index you choose the decay curve, for example a Gaussian curve
with travel scale σ = 10 minutes, under which a practice 10 minutes away
counts about 61% as much as one next door. The index changes with that
choice: report it and try other values. In the allocation model σ is
estimated from observed counts, so it describes how quickly registration
falls off with travel time in those data. What σ means depends on the
shape of the curve (the kernel): a scale in travel units for the
Gaussian kernel, a rate for the exponential kernel and an exponent for
the power kernel. Compare σ only between fits with the same kernel. [An
FCA index or an allocation
model?](https://songyosr.github.io/spax/articles/model-choice.md) sets a
chosen and a fitted scale side by side.

## Units, sources and dates

The units and dates you give `prep_data(metadata = )` are labels. spax
keeps them with the data and its results, but it does not convert units
or check that they fit together. A Gaussian σ is in the unit of travel,
and the units of supply and demand set the unit of an index. The
Cornwall inputs come from different sources and dates, and their totals
differ:

``` r

# Residents of Cornwall and the Isles of Scilly in three sources
pop <- read_spax_example("cornwall_pop.tif")
residents <- c(
  worldpop_2020 = round(global(pop, "sum", na.rm = TRUE)[[1]]),
  ons_mid_2024 = sum(cornwall_lsoa$pop),
  registered_july_2026 = sum(cornwall_lsoa$registered)
)
residents
```

``` spax-output
       worldpop_2020         ons_mid_2024 registered_july_2026 
              542666               585655               606796 
```

The grid holds 7.3% fewer residents than the ONS estimate, and the
practice registers 3.6% more. Choose one source of demand for an
analysis and say which. The FCA maps in these guides use the grid. The
allocation fits on small areas, and the comparison in [An FCA index or
an allocation
model?](https://songyosr.github.io/spax/articles/model-choice.md), use
the registered residents. [The Cornwall GP example
data](https://songyosr.github.io/spax/articles/cornwall-data.md) lists
the sources.

## Scenarios

A scenario changes an input, for example closes a practice, and runs the
model again with its parameters held at the fitted values. The
difference between the two runs is what the model implies if everyone
chooses by the same rule as before. It is a model-based comparison, not
an estimate of what would happen, and it has no interval. The bootstrap
gives one only for a model fitted to facility totals (see
[Uncertainty](#uncertainty)). [What if a practice
closed?](https://songyosr.github.io/spax/articles/scenario.md) runs one.

## Uncertainty

[`bootstrap_fit()`](https://songyosr.github.io/spax/reference/bootstrap_fit.md)
gives intervals for the fitted parameters, for the share of demand that
reaches a facility and for the change a supply scenario makes. It needs
the CLM with a fitted outside weight v0, the pull of the outside option
([`spec_clm()`](https://songyosr.github.io/spax/reference/spec_clm.md)
with its default `outside = "fit"`). The model must be fitted to
facility totals with
[`obs_totals()`](https://songyosr.github.io/spax/reference/obs_flows.md),
its default Poisson loss and no outside count. A fit to flows, such as
the Cornwall fit, is outside the method. It also needs a declaration of
how the counts arose: what was counted, over what period, and that
facilities were sampled independently. That declaration, the observation
law, is an assumption the data cannot confirm, and the intervals hold
demand, supply and travel fixed. [How sure are the
estimates?](https://songyosr.github.io/spax/articles/facility-bootstrap.md)
shows the bootstrap on generated data.

## IDs and missing data

spax lines up demand, supply, travel and observations by their IDs.
[`prep_data()`](https://songyosr.github.io/spax/reference/prep_data.md)
takes the origin IDs from the names of demand and the facility IDs from
the column names of travel. Without them it stops, unless you pass
`origin_ids` and `facility_ids`. A supply vector without names is taken
in the order of the travel columns, so name it too.

Demand can be a known zero, unknown (`NA`) or left out on purpose with
`included`, and the prepared data keep the three apart. Unknown demand
stops
[`prep_data()`](https://songyosr.github.io/spax/reference/prep_data.md)
unless you set `missing_demand = "exclude"`, which leaves those areas
out and counts them. The code sets one area of each kind:

``` r

travel <- cornwall_travel[areas$lsoa, gp$practice]
supply <- setNames(gp$gp_fte_extg, gp$practice)

# One area with no residents, one with unknown demand, one left out on purpose
demand_gaps <- setNames(areas$registered, areas$lsoa)
demand_gaps[1] <- 0
demand_gaps[2] <- NA
included <- names(demand_gaps) != areas$lsoa[3]
prep_data(demand_gaps, supply, travel, missing_demand = "exclude", included = included)
```

``` spax-output
<spax data> 336 origins (333 active; 1 with zero demand, 1 with unknown demand, 1 excluded) x 54 facilities
  demand      597,990
  travel      unknown for 0 active origins
  policies    missing demand: exclude; missing travel: error
```

Only the 333 active areas, with known positive demand and not left out,
enter the model. The print counts the other three separately. In travel,
`Inf` marks a pair known to have no link and `NA` an unknown time. On
numeric data,
[`prep_data()`](https://songyosr.github.io/spax/reference/prep_data.md)
stops at unknown travel unless you set `missing_travel = "omit"`, which
leaves those pairs out. An FCA index on a grid drops unknown times from
its sums, so an unknown time counts as no access. Mask such cells first
if they are not truly unreachable. [Bringing your own
data](https://songyosr.github.io/spax/articles/data-prep.md) shows both.

## Which functions may change

The one-call FCA functions
[`spax_2sfca()`](https://songyosr.github.io/spax/reference/spax_2sfca.md),
[`spax_e2sfca()`](https://songyosr.github.io/spax/reference/spax_e2sfca.md)
and
[`spax_m2sfca()`](https://songyosr.github.io/spax/reference/spax_m2sfca.md),
the building blocks shown in [Inside the E2SFCA
calculation](https://songyosr.github.io/spax/articles/e2sfca-steps.md)
and the grid helpers such as
[`transform_demand()`](https://songyosr.github.io/spax/reference/transform_demand.md)
are stable. The others may change:
[`spax()`](https://songyosr.github.io/spax/reference/spax.md) and
[`prep_data()`](https://songyosr.github.io/spax/reference/prep_data.md),
the `spec_*()`, `decay_*()` and `obs_*()` functions,
[`spax_huff()`](https://songyosr.github.io/spax/reference/spax_huff.md),
[`spax_clm()`](https://songyosr.github.io/spax/reference/spax_huff.md),
[`spax_3sfca()`](https://songyosr.github.io/spax/reference/spax_3sfca.md),
[`spax_huff2sfca()`](https://songyosr.github.io/spax/reference/spax_huff2sfca.md),
[`calc_travel_time()`](https://songyosr.github.io/spax/reference/calc_travel_time.md),
[`calc_demand()`](https://songyosr.github.io/spax/reference/calc_demand.md),
the allocation steps listed in the [Function
map](https://songyosr.github.io/spax/articles/function-naming.md),
[`check_fit()`](https://songyosr.github.io/spax/reference/check_fit.md),
[`compare_fits()`](https://songyosr.github.io/spax/reference/compare_fits.md)
and
[`bootstrap_fit()`](https://songyosr.github.io/spax/reference/bootstrap_fit.md).
Record the spax version with every result, for example with
`packageVersion("spax")`.

## Keeping results

Save an allocation result with
[`saveRDS()`](https://rspatial.github.io/terra/reference/serialize.html),
which keeps the model, its data, the observations and the fit together.
A table from
[`as.data.frame()`](https://rspatial.github.io/terra/reference/as.data.frame.html)
keeps only the numbers. An FCA result on a grid holds rasters that
[`saveRDS()`](https://rspatial.github.io/terra/reference/serialize.html)
cannot keep, so save it with
[`save_spax()`](https://songyosr.github.io/spax/reference/save_spax.md)
and read it back with
[`read_spax()`](https://songyosr.github.io/spax/reference/read_spax.md).
[Where do residents
register?](https://songyosr.github.io/spax/articles/allocation-workflow.md)
saves a fit and its tables.

## Next

- [Getting started](https://songyosr.github.io/spax/articles/spax.md):
  compute and map an FCA index.
- [Where do residents
  register?](https://songyosr.github.io/spax/articles/allocation-workflow.md):
  fit the allocation model and check what the data identify.
- [Bringing your own
  data](https://songyosr.github.io/spax/articles/data-prep.md): line up
  IDs, units and missing values before any calculation.
- [Function
  map](https://songyosr.github.io/spax/articles/function-naming.md):
  find the function for a task.
