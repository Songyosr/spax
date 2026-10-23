# An FCA index or an allocation model?

spax computes two kinds of result from the same demand, supply and
travel times. A floating catchment area (FCA) index measures how much
supply a place has relative to the demand that competes for it. An
allocation model says where the residents of a place go: a probability
for each facility and for an outside option, which stands for every
choice outside the facilities in the data. Its parameters are fitted to
the use you observed. This guide computes both for the same small areas
and sets them side by side, so that you can choose the one that answers
your question. It uses the GP registrations, practices and travel times
of [The Cornwall GP example
data](https://songyosr.github.io/spax/articles/cornwall-data.md). The
[`spax()`](https://songyosr.github.io/spax/reference/spax.md) interface
and the allocation model are experimental and may change in later
versions.

``` r

library(spax)
library(terra)
library(sf)
library(dplyr)
library(ggplot2)
```

## Which one answers your question?

Start from the quantity you need and the observations you have.

| Question | Call | What you get |
|:---|:---|:---|
| How much supply does each place have, relative to the demand that can reach it? | `spax(data, spec_e2sfca())`, or another FCA specification: [`spec_2sfca()`](https://songyosr.github.io/spax/reference/spec_e2sfca.md), [`spec_m2sfca()`](https://songyosr.github.io/spax/reference/spec_e2sfca.md), [`spec_3sfca()`](https://songyosr.github.io/spax/reference/spec_e2sfca.md), [`spec_huff2sfca()`](https://songyosr.github.io/spax/reference/spec_e2sfca.md) | An index of supply per travel-weighted resident |
| Where do residents go, among the facilities and an outside option? | `spax(data, spec_clm(decay_gaussian(sigma = 5), outside = 0.03))`, with a value for every parameter | A probability for each facility and the outside option, and the expected number of users of each facility |
| Which parameters fit the use you observed? | `spax(data, spec_clm(), observed = obs_flows())`, or [`obs_totals()`](https://songyosr.github.io/spax/reference/obs_flows.md) for facility totals | Fitted parameters, read with [`coef()`](https://rdrr.io/r/stats/coef.html) and checked with [`check_fit()`](https://songyosr.github.io/spax/reference/check_fit.md) |
| What changes when supply changes, at the same parameters? | [`spax()`](https://songyosr.github.io/spax/reference/spax.md) on the changed data, with the fitted values held | The difference between two modeled scenarios |
| What does the model give at other places? | [`predict()`](https://rspatial.github.io/terra/reference/predict.html) on the fitted result | Probabilities and outside shares at those places |
| How sure are the estimates? | [`bootstrap_fit()`](https://songyosr.github.io/spax/reference/bootstrap_fit.md) on an allocation model with a fitted outside weight, fitted to facility totals | Bootstrap intervals for the parameters and for a scenario |

An FCA index needs no observations, and the analyst chooses how fast its
weights fall with travel time. An allocation model can be fitted to
observed counts, and its fitted counts can then be checked against them.

## One Cornwall system, two calculations

Both calculations below use the same inputs for mainland Cornwall: small
areas and their GP practices. Demand is the number of residents of each
area registered with a GP practice, and supply is each practice’s GP
full-time equivalents (FTE), excluding GPs in training. Travel is a
matrix of minutes from each area to each practice.
[`prep_data()`](https://songyosr.github.io/spax/reference/prep_data.md)
lines the three up by their IDs, and
[`obs_flows()`](https://songyosr.github.io/spax/reference/obs_flows.md)
declares the observed registrations by area and practice.

``` r

areas <- cornwall_lsoa |>
  filter(!scilly) |>
  st_drop_geometry()
gp <- cornwall_gp |>
  filter(!scilly) |>
  st_drop_geometry()
travel <- cornwall_travel[areas$lsoa, gp$practice]
demand <- setNames(areas$registered, areas$lsoa)
supply <- setNames(gp$gp_fte_extg, gp$practice)
d <- prep_data(demand, supply, travel, metadata = list(
  demand_units = "registered residents", demand_period = "July 2026",
  supply_units = "GP FTE excluding GPs in training", supply_period = "June 2026",
  travel_units = "minutes"))
flows <- obs_flows(cornwall_flows, origin = "lsoa", facility = "practice", count = "patients")
d
```

``` spax-output
<spax data> 336 origins (336 active) x 54 facilities
  demand      604,471 registered residents (July 2026)
  supply      GP FTE excluding GPs in training (June 2026)
  travel      minutes; unknown for 0 active origins
  policies    missing demand: error; missing travel: error
```

The prepared data hold 336 small areas (lower layer super output areas,
LSOAs) and 54 practices. spax calls the small areas origins and the
practices facilities, in the print and in its tables. The unit labels
are kept with the data and every result, and spax does not convert
units.

### An FCA index

The enhanced two-step floating catchment area method (E2SFCA) works in
two steps. Each practice divides its supply by the demand that can reach
it, weighted by travel time. Each area then adds up the ratios of the
practices it can reach, with the same weights. Here the weight of a
practice $`d`$ minutes away is $`w(d) = \exp(-d^2 / 2\sigma^2)`$, a
Gaussian curve with a travel scale σ of 10 minutes, so a practice 10
minutes away counts with weight 0.61. The 10 minutes is a choice for
this guide. With small areas as the origins,
[`spax()`](https://songyosr.github.io/spax/reference/spax.md) returns
one value per area. On a population grid it returns a map.
[`as.data.frame()`](https://rspatial.github.io/terra/reference/as.data.frame.html)
gives one row per area, with the index in a column named after the
supply layer, `supply`.

``` r

# The E2SFCA index at each area, with a 10-minute Gaussian decay
fca <- spax(d, spec_e2sfca(decay = decay_gaussian(sigma = 10)))
fca
```

``` spax-output
<spax access> E2SFCA, 1 layer at 336 origins x 54 facilities
  layers      supply
  decay       gaussian (sigma = 10)
  normalize   identity
```

``` r

# One row per area. The index column is named after the supply layer;
# times 1,000 it is in GP FTE per 1,000 travel-weighted registered residents
fca_index <- as.data.frame(fca) |>
  transmute(lsoa = origin_id, fca = 1000 * supply)
summary(fca_index$fca)
```

``` spax-output
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
 0.1609  0.4535  0.5094  0.5131  0.5988  0.6915 
```

The index ranges from 0.16 to 0.69 GP FTE per 1,000 travel-weighted
registered residents across the areas. The print lists one layer,
`supply`: with several supply measures, such as GP FTE with and without
GPs in training, each gives a layer and an index of its own.
`normalize identity` means that demand is used as it is.

### An allocation model fitted to registrations

The conditional logit model (CLM) gives each practice an attraction: its
GP FTE times a decay weight of the travel time. It adds an outside
option with a weight v0 of its own. The residents of area $`i`$ go to
practice $`j`$ with probability

``` math
P_{ij} = \frac{S_j\, w(d_{ij})}{\sum_k S_k\, w(d_{ik}) + v_0},
```

where $`S_j`$ is the practice’s supply and $`w(d_{ij})`$ the decay
weight of the travel time $`d_{ij}`$. They go to the outside option with
the remaining probability. Here the outside option stands for
registration with a practice outside mainland Cornwall, in Devon for
example.
[`spec_clm()`](https://songyosr.github.io/spax/reference/spec_clm.md)
declares the model with a Gaussian decay, and by default
[`spax()`](https://songyosr.github.io/spax/reference/spax.md) fits both
the travel scale σ and the outside weight v0. Fitting searches for the
values whose expected registrations (demand times probability) come
closest to the observed counts, as measured by a Poisson loss.

``` r

# Gaussian decay and outside weight, both fitted to the registrations
clm <- spax(d, spec_clm(), observed = flows)
```

    observed: 2,290 counts from 1 origin outside the data or left out by it are dropped; 5 counts at 1 facility outside the data are not fitted (outside = "known" uses them as outside counts)

``` r

clm
```

``` spax-output
<spax allocation> conditional logit with an outside option
  decay       gaussian (sigma = travel scale): fit
  attraction  (kappa * supply)^beta: kappa = 1, beta = 1
  outside     v0: fit
  data        336 origins x 54 facilities

  parameter  value    status
  sigma      4.591    fitted
  v0         0.02733  fitted
  beta       1        held
  kappa      1        held

  selected    scan1 (start 1 of 3); 3 of 3 starts converged
  boundary    none
  starts      default search, 45 of 45 grid points, loss poisson
```

The message about the Isles of Scilly is explained in [Where do
residents
register?](https://songyosr.github.io/spax/articles/allocation-workflow.html#fit-the-model).
In the print, `beta` and `kappa` shape the attraction. Both are held at
1, so the attraction is the GP FTE itself, as in the formula. No
starting values were given, so
[`spax()`](https://songyosr.github.io/spax/reference/spax.md) evaluated
the loss on a grid of σ and v0 values and started the search from 3 of
them. Each of the 3 searches converged, and
[`spax()`](https://songyosr.github.io/spax/reference/spax.md) kept the
best fit.

The fitted travel scale σ is about 4.6 minutes, much shorter than the 10
minutes chosen for the FCA index: registration is very local. The
outside weight v0 is on the scale of an attraction: 0.027 is the pull of
a practice with that many GP FTE next door. So the outside option
matters only where every practice is far away.

## What one area sees

The two calculations weight the same practices in different ways. Take
the area with the largest total FCA weight, one with many practices
within a short drive.
[`predict()`](https://rspatial.github.io/terra/reference/predict.html)
gives the CLM’s probabilities for every area, and the fit’s flow table
gives the registrations by area and practice. The table lists the
practices within 7 minutes of the area. `clm_prob` is the CLM
probability and `obs_share` the observed share of the area’s registered
residents.

``` r

# CLM probabilities for every area and practice, and outside shares
prediction <- predict(clm, travel, demand,
  outputs = c("allocation", "outside_share"))
probability <- prediction$outputs$allocation

# Registrations by area (rows) and practice (columns), from the flow table
flow_table <- as.data.frame(clm, type = "flows")
observed <- xtabs(observed ~ origin_id + facility_id, flow_table)
observed <- observed[rownames(travel), colnames(travel)]

# The FCA's Gaussian weights, with the 10-minute travel scale
fca_weight <- exp(-travel^2 / (2 * 10^2))

# The area with the largest total FCA weight, and the practices within 7 minutes
area <- which.max(rowSums(fca_weight))
nearest <- order(travel[area, ])
nearest <- nearest[travel[area, nearest] <= 7]

data.frame(
  practice = tools::toTitleCase(tolower(gp$name[nearest])),
  gp_fte = round(supply[nearest], 1),
  minutes = travel[area, nearest],
  fca_weight = round(fca_weight[area, nearest], 2),
  clm_prob = round(probability[area, nearest], 3),
  obs_share = round(observed[area, nearest] / demand[area], 3),
  row.names = NULL
)
```

``` spax-output
                       practice gp_fte minutes fca_weight clm_prob obs_share
1          Carnon Downs Surgery    5.3       2       0.98    0.216     0.573
2                Penryn Surgery    8.0       5       0.88    0.196     0.089
3      Chacewater Health Centre    3.7       6       0.84    0.071     0.171
4       Lander Medical Practice   10.3       7       0.78    0.143     0.028
5 Three Spires Medical Practice   10.3       7       0.78    0.144     0.074
6        Falmouth Health Centre    5.9       7       0.78    0.082     0.020
7            Trescobeas Surgery    6.3       7       0.78    0.087     0.013
```

``` r

# Totals over all practices for this area
round(
  c(
    fca_weights = sum(fca_weight[area, ]),
    clm_practices = sum(probability[area, ]),
    clm_outside = prediction$outputs$outside_share[[area]]
  ),
  4
)
```

``` spax-output
  fca_weights clm_practices   clm_outside 
      10.3451        0.9988        0.0012 
```

The FCA weights are separate discounts, one per practice. They do not
compete with each other, and for this area they add up to 10.3, as much
weight as 10 practices next door. The CLM probabilities compete: a
nearby practice with more GP FTE draws residents away from the others,
and the probabilities over all practices and the outside option add up
to 1. Lander Medical Practice, 7 minutes away with 10.3 GP FTE, gets a
higher probability than Chacewater Health Centre, 6 minutes away with
3.7 GP FTE.

Because they are probabilities, they can be set beside the observed
shares. At this one area they differ markedly: 57% of residents are
registered at the nearest practice, against a modeled 22%. The model
knows only travel time and GP FTE, so the next section judges it across
all areas.

## Side by side

The FCA index gives one value per area. The CLM gives each area a
distribution over the practices, and other quantities follow from it:
expected registrations (demand times probability), each practice’s
expected list (the residents registered with it: its expected
registrations summed over areas) and the mean travel time to the
practice an area’s residents are modeled to register with. The maps show
the FCA index and that mean travel time.

``` r

# The FCA index and the CLM's probability-weighted mean minutes per area
areas <- areas |>
  left_join(fca_index, by = "lsoa") |>
  mutate(clm_minutes = rowSums(probability * travel) / rowSums(probability))

# Join to the small-area polygons for mapping
mapped <- cornwall_lsoa |>
  select(lsoa) |>
  inner_join(areas, by = "lsoa")

ggplot(mapped) +
  geom_sf(aes(fill = fca, color = fca), linewidth = 0.1) +
  geom_sf(data = st_union(mapped), fill = NA, color = "grey20", linewidth = 0.3) +
  scale_fill_viridis_c(
    name = "GP FTE\nper 1,000",
    aesthetics = c("fill", "color")
  ) +
  labs(title = "E2SFCA index (sigma = 10 min)") +
  theme_void()

ggplot(mapped) +
  geom_sf(aes(fill = clm_minutes, color = clm_minutes), linewidth = 0.1) +
  geom_sf(data = st_union(mapped), fill = NA, color = "grey20", linewidth = 0.3) +
  scale_fill_viridis_c(
    name = "Minutes",
    option = "magma",
    direction = -1,
    aesthetics = c("fill", "color")
  ) +
  labs(title = "CLM: mean minutes to modeled practice") +
  theme_void()
```

![Map of mainland Cornwall's small areas colored by the E2SFCA index
with a 10-minute Gaussian decay, in GP FTE per 1,000 travel-weighted
registered residents; it is lowest at the western tip, on the Lizard
peninsula and on the north coast around Padstow, and highest in a few
central and eastern
areas.](model-choice_files/figure-html/maps-1.png)![Map of mainland
Cornwall's small areas colored by the CLM's mean travel time, in
minutes, to the practice that residents are modeled to register with;
the longest times are on the north coast around Padstow, in the rural
north-east and at the western
tip.](model-choice_files/figure-html/maps-2.png)

In both maps darker means less favorable: a lower index, or a longer
modeled trip. Both are dark at the western tip, on the north coast
around Padstow and inland in the north-east. Elsewhere they can part. In
Bude, in the far north-east, the modeled trips are short, at most 3
minutes, yet the index is in the lowest quarter of areas: few GP FTE lie
within reach of the residents who share them. Across areas, a higher
index goes with shorter modeled trips only loosely: the two correlate at
-0.39.

The CLM’s fitted registrations can be compared with the observed ones.
The FCA index has no observed counterpart in these data.

``` r

# Correlations of modeled with observed: area mean minutes, practice lists
observed_minutes <- rowSums(observed * travel) / rowSums(observed)
c(
  mean_minutes = cor(areas$clm_minutes, observed_minutes),
  practice_lists = cor(colSums(fitted(clm)), colSums(observed))
)
```

``` spax-output
  mean_minutes practice_lists 
     0.9023549      0.9326925 
```

``` r

# Share of residents registered outside mainland Cornwall (%), observed and
# modeled
outside <- c(
  observed = 1 - sum(observed) / sum(demand),
  modeled = 1 - sum(fitted(clm)) / sum(demand)
)
round(100 * outside, 2)
```

``` spax-output
observed  modeled 
    0.91     0.54 
```

Modeled and observed mean travel times correlate at 0.90 across areas,
and modeled and observed practice lists at 0.93. The model puts fewer
residents outside mainland Cornwall than observed (0.54% against 0.91%).
This fit uses only the counts at mainland practices, so nothing ties its
outside total to the observed one.

Declared as known, the outside counts make the two totals agree: see
[When outside counts are
known](https://songyosr.github.io/spax/articles/allocation-workflow.html#when-outside-counts-are-known).
Even then, one outside weight for every area cannot reproduce where
registration outside mainland Cornwall happens: mostly in a few areas
near the Devon border, as the outside-option map in [Where do residents
register?](https://songyosr.github.io/spax/articles/allocation-workflow.html#the-outside-option)
shows.

Use an FCA index when you need supply relative to demand, have no counts
of where people go, or want a decay you set yourself. Use the allocation
model when you need to know where residents go, have counts to fit it
to, and want to check it against them.

|  | E2SFCA index | CLM |
|:---|:---|:---|
| Per area | One supply-per-demand value for each supply measure | A probability for each practice and for the outside option |
| Supply | Each supply measure gives its own index | One supply measure, which sets the attraction and does not cap the list |
| Travel decay | Chosen by the analyst (10 minutes here) | Fitted to the registrations (about 4.6 minutes here) |
| Outside option | None | A weight v0, fitted here |
| Check against registrations | No observed counterpart | Fitted registrations, practice lists and outside share |

[Reading
results](https://songyosr.github.io/spax/articles/reading-results.md)
says what each output measures.

## Next

- [Where do residents
  register?](https://songyosr.github.io/spax/articles/allocation-workflow.md):
  the allocation model in full, from the fit to the outside option.
- [Which FCA
  index?](https://songyosr.github.io/spax/articles/fca-methods.md): if
  an index answers your question, choose among five variants and map
  them on the population grid.
- [What if a practice
  closed?](https://songyosr.github.io/spax/articles/scenario.md): hold
  the fitted values and change the supply.
- [Reading
  results](https://songyosr.github.io/spax/articles/reading-results.md):
  what an index, a probability and a fit can and cannot tell you.
