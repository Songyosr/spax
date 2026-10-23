# Inside the E2SFCA calculation

The enhanced two-step floating catchment area (E2SFCA) index gives each
place the GP supply per resident within reach, with weights that fall
with travel time. This guide rebuilds the index of [Getting
started](https://songyosr.github.io/spax/articles/spax.md) one step at a
time from the building blocks of spax, and compares the result with
[`spax()`](https://songyosr.github.io/spax/reference/spax.md). By the
end you can say where each cell’s number comes from, swap one set of
weights to get another index, and see why the calculation has to cover
the whole study area. The inputs are the mainland grids of [The Cornwall
GP example
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

## The formula

A floating catchment area (FCA) index has two steps. First each facility
gathers the demand around it and divides its supply by that demand. Then
each place adds up the ratios of the facilities it can reach. Here
demand is the residents of a grid cell and supply is a practice’s GP
full-time equivalents (FTE).

Let demand at cell $`i`$ be $`D_i`$ and supply at practice $`j`$ be
$`S_j`$. The weights $`G_{ij}`$ decide how much of cell $`i`$’s demand
practice $`j`$ gathers, and the weights $`H_{ij}`$ how much of practice
$`j`$’s ratio cell $`i`$ receives. Then

``` math
L_j=\sum_i D_iG_{ij},\qquad R_j=S_j/L_j,\qquad A_i=\sum_j H_{ij}R_j.
```

$`L_j`$ is the demand practice $`j`$ gathers, $`R_j`$ its
supply-to-demand ratio and $`A_i`$ the index at cell $`i`$. E2SFCA uses
the same decay weights $`W_{ij}`$, which fall with the travel time from
cell $`i`$ to practice $`j`$ (step 1), for both: $`G = H = W`$.

## The inputs

The setup code shared by every grid guide, explained in [The Cornwall GP
example
data](https://songyosr.github.io/spax/articles/cornwall-data.md), reads
the population grid, the mainland practices and one layer of travel
minutes per practice. The supply is also kept as a vector named by
practice code, and a small helper maps a layer around Truro so that
single cells stay visible.

``` r

gp <- cornwall_gp |>
  filter(!scilly) |>
  st_drop_geometry()
pop <- read_spax_example("cornwall_pop.tif")
travel_grid <- read_spax_example("cornwall_gp_travel.tif")[[gp$practice]]
# Keep cells with a travel time to every mainland practice
pop <- trim(mask(pop, sum(travel_grid)))
travel_grid <- crop(travel_grid, pop)
grid_data <- prep_data(pop, gp, travel_grid, id_col = "practice", supply_cols = "gp_fte_extg")

# GP FTE named by practice code
supply <- setNames(gp$gp_fte_extg, gp$practice)

# Map helper: a layer within the Truro display window, with practices
truro_window <- ext(-5.30, -4.80, 50.12, 50.42)
practices <- cornwall_gp |>
  filter(!scilly) |>
  vect()
coast <- cornwall_lsoa |>
  filter(!scilly) |>
  st_union() |>
  vect()
show <- function(layer, title) {
  plot(
    crop(layer, truro_window),
    axes = FALSE,
    main = title,
    col = hcl.colors(50, "viridis")
  )
  lines(crop(coast, truro_window), lwd = 0.6)
  points(crop(practices, truro_window), pch = 3, col = "red")
}
```

The calculation covers all of mainland Cornwall. Only the maps zoom in.

## Step 1: decay weights

With Gaussian decay,
$`W_{ij}=\exp\!\left(-d_{ij}^2/(2\sigma^2)\right)`$, where $`d_{ij}`$ is
the travel time and σ is the travel scale. We use σ = 10 minutes, a
choice for this guide.
[`calc_decay()`](https://songyosr.github.io/spax/reference/calc_decay.md)
works on plain numbers and on rasters alike.

``` r

# The weight left after a few travel times
minutes <- c(0, 5, 10, 15, 20, 30)
weight_left <- calc_decay(minutes, method = "gaussian", sigma = 10)
round(setNames(weight_left, minutes), 3)
```

``` spax-output
    0     5    10    15    20    30 
1.000 0.882 0.607 0.325 0.135 0.011 
```

``` r

# The same decay applied to every travel layer
weights <- calc_decay(travel_grid, method = "gaussian", sigma = 10)
show(
  weights[["L82001"]],
  "Step 1: weight of one Truro practice (L82001), 0 to 1"
)
```

![Raster map of the area around Truro showing the Gaussian decay weight
of one central Truro practice, from 0 to 1: close to 1 near the practice
and fading with travel time, with gaps along the estuaries; red crosses
mark the practices.](e2sfca-steps_files/figure-html/kernel-1.png)

A practice 10 minutes away keeps 61% of the weight it would have next
door, and one 30 minutes away about 1%. Each practice has one weight
layer. The map shows the layer for L82001, a practice in central Truro:
it is the red cross at the center of the bright area (the other crosses
are other practices). The weight is close to 1 near it and fades with
travel time. The shape follows the travel-time surface rather than
straight-line distance, and the gaps are estuaries.

## Step 2 (optional): normalize the demand-side weights

The weights of all practices at one cell can add up to more than 1.
Where many practices are close, each of them counts the same resident.
[`calc_normalize()`](https://songyosr.github.io/spax/reference/calc_normalize.md)
can rescale them: `"standard"` (its default) divides each cell’s weights
by their sum, and `"semi"` does so only where the sum exceeds 1.
`"identity"`, the default of
[`spec_e2sfca()`](https://songyosr.github.io/spax/reference/spec_e2sfca.md),
leaves them as they are. The table gives cell quantiles of the summed
weights, before and after.

``` r

# Sum of all practices' weights at each cell
cell_sum <- function(layers) values(sum(layers), mat = FALSE)
probs <- c(0.05, 0.5, 0.95)

standard <- calc_normalize(weights, method = "standard")
semi <- calc_normalize(weights, method = "semi")

round(
  rbind(
    identity = quantile(cell_sum(weights), probs, na.rm = TRUE),
    standard = quantile(cell_sum(standard), probs, na.rm = TRUE),
    semi = quantile(cell_sum(semi), probs, na.rm = TRUE)
  ),
  2
)
```

``` spax-output
           5%  50%  95%
identity 1.23 3.96 9.06
standard 1.00 1.00 1.00
semi     1.00 1.00 1.00
```

At the median cell the weights sum to 3.96, and even at the 5th
percentile they sum to 1.23. So `"standard"` and `"semi"` give almost
the same result here, and both differ from `"identity"`. The choice
changes the demand each practice gathers and so the index: normalization
is part of the model you choose. `spec_e2sfca(normalize = )` applies it
to the demand side only. We keep `"identity"`, the default, so
$`G = H = W`$.

## Step 3: gather demand to practices

[`gather_weighted()`](https://songyosr.github.io/spax/reference/gather_weighted.md)
sums demand times weight over all cells for each practice:
$`L_j=\sum_i D_iW_{ij}`$. With `simplify = TRUE` it returns a vector
named by the weight layers.

``` r

gathered <- gather_weighted(
  values = pop,
  weights = weights,
  simplify = TRUE  # one named value per practice
)
round(head(gathered))
```

``` spax-output
L82001 L82003 L82004 L82006 L82007 L82008 
 95347  22747  42324  91637  21265  16840 
```

The names are the practice codes, which step 4 uses to line up the
supply. Each value is the weighted number of residents a practice
gathers. A resident within reach of several practices counts toward each
of them, so these values, over all 54 practices, add up to 5.9 times the
mainland’s residents.

## Step 4: supply-to-demand ratios

Each practice’s ratio is its supply over the demand it gathered,
$`R_j=S_j/L_j`$. Index the supply by the names of `gathered`, not by
position. The table shows six practices near Truro.

``` r

ratio <- supply[names(gathered)] / gathered

# Supply, gathered demand and ratio for six practices near Truro
near_truro <- c("L82001", "L82028", "L82015", "L82061", "L82045", "L82048")
data.frame(practice = near_truro) |>
  left_join(select(gp, practice, name), by = "practice") |>
  mutate(
    gp_fte = round(supply[practice], 1),
    weighted_residents = round(gathered[practice]),
    fte_per_1000 = round(1000 * ratio[practice], 2)
  )
```

``` spax-output
  practice                          name gp_fte weighted_residents fte_per_1000
1   L82001       LANDER MEDICAL PRACTICE   10.3              95347         0.11
2   L82028 THREE SPIRES MEDICAL PRACTICE   10.3              95347         0.11
3   L82015      CHACEWATER HEALTH CENTRE    3.7             105955         0.04
4   L82061          CARNON DOWNS SURGERY    5.3             100581         0.05
5   L82045                PROBUS SURGERY    6.6              71415         0.09
6   L82048            ROSELAND SURGERIES    3.3              42552         0.08
```

The ratios look small because, with identity weights, each resident is
counted by several practices. Step 5 adds them back up. The two central
Truro practices share one site, so they gather the same demand. With a
decay that reaches zero, such as a fixed catchment, a practice can
gather no demand. spax gives it a ratio of zero, but the division above
gives `Inf`, so set those ratios to 0 when you calculate by hand.

## Step 5: spread the ratios back to cells

[`spread_weighted()`](https://songyosr.github.io/spax/reference/spread_weighted.md)
multiplies each practice’s ratio by its weight layer and sums over
practices: $`A_i=\sum_j W_{ij}R_j`$. It matches values to layers by
position, so pass the ratios in layer order.

``` r

# Ratios in layer order, without names: matching is by position
access <- spread_weighted(
  values = unname(ratio[names(weights)]),
  weights = weights
)
show(1000 * access, "Step 5: E2SFCA, GP FTE per 1,000 residents")
```

![Raster map of the E2SFCA index around Truro in GP FTE per 1,000
residents, with practices as red crosses; values are highest in a band
from Truro towards the south-west and lower in the east of the
window.](e2sfca-steps_files/figure-html/spread-1.png)

The index is highest in a band west and south-west of Truro and falls
towards the east of the window, where fewer practices are close. A
cell’s index is a sum of contributions from every practice. For the cell
that contains the central Truro practice, the largest contributions are,
in GP FTE per 1,000 residents:

``` r

# The grid cell that contains practice L82001
site <- geom(practices[practices$practice == "L82001", ])
cell <- cellFromXY(pop, site[, c("x", "y"), drop = FALSE])

# Each practice's contribution: its weight at the cell times its ratio
weights_at_cell <- unlist(extract(weights, cell))
contributions <- sort(
  1000 * weights_at_cell * ratio[names(weights_at_cell)],
  decreasing = TRUE
)

round(
  c(
    head(contributions, 5),
    all_others = sum(contributions[-(1:5)]),
    total = sum(contributions)
  ),
  3
)
```

``` spax-output
    L82028     L82001     L82045     L82006     L82061 all_others      total 
     0.108      0.107      0.067      0.048      0.047      0.417      0.794 
```

The two practices at the central Truro site contribute most. The other
49 practices beyond the top five still add 53% of the cell’s index.

## The same index from `spax()`

The surface built by hand, `access`, should match the index from
[`spax()`](https://songyosr.github.io/spax/reference/spax.md), the
one-call function
[`spax_e2sfca()`](https://songyosr.github.io/spax/reference/spax_e2sfca.md)
and the more general
[`compute_access()`](https://songyosr.github.io/spax/reference/compute_access.md).
[`compute_access()`](https://songyosr.github.io/spax/reference/compute_access.md)
takes the two weight sets $`G`$ and $`H`$ of the formula as
`demand_weights` and `access_weights`, so it can build any index of this
form. Each comparison below prints `TRUE` when the two surfaces agree,
cell by cell and in which cells are empty.

``` r

# The index from a specification
by_spec <- spax(grid_data, spec_e2sfca(decay = decay_gaussian(sigma = 10)))

# The one-call function
one_call <- spax_e2sfca(
  demand = pop,
  supply = gp,
  distance = travel_grid,
  decay_params = list(method = "gaussian", sigma = 10),
  id_col = "practice",
  supply_cols = "gp_fte_extg"
)

# The general function, with the decay weights on both sides
general <- compute_access(
  demand = pop,
  supply = supply,
  demand_weights = weights,
  access_weights = weights
)

# Compare a surface with the one built by hand
same_as_by_hand <- function(surface) {
  all.equal(values(surface, mat = FALSE), values(access, mat = FALSE))
}
c(
  spax = same_as_by_hand(by_spec$accessibility),
  spax_e2sfca = same_as_by_hand(one_call$accessibility),
  compute_access = same_as_by_hand(general)
)
```

``` spax-output
          spax    spax_e2sfca compute_access 
          TRUE           TRUE           TRUE 
```

[`all.equal()`](https://rspatial.github.io/terra/reference/all.equal.html)
allows for rounding error in the last digits, and every route agrees.
Each computes the same three quantities: gathered demand, ratios and the
weighted sum of ratios.

## Change the access-side weights: M2SFCA

The modified 2SFCA (M2SFCA) keeps $`W`$ on the demand side but uses
$`W^2`$ on the access side, so $`G = W`$ and $`H = W^2`$. $`W^2`$ is
below $`W`$ except at zero travel time, and far below it for distant
practices, so M2SFCA discounts cells whose practices are far away. Swap
the access-side weights for their squares and
[`compute_access()`](https://songyosr.github.io/spax/reference/compute_access.md)
gives M2SFCA.

``` r

m2sfca <- spax(grid_data, spec_m2sfca(decay = decay_gaussian(sigma = 10)))

# M2SFCA from the blocks: squared weights on the access side only
m2sfca_by_blocks <- compute_access(
  demand = pop,
  supply = supply,
  demand_weights = weights,
  access_weights = weights^2
)
all.equal(
  values(m2sfca_by_blocks, mat = FALSE),
  values(m2sfca$accessibility, mat = FALSE)
)
```

``` spax-output
[1] TRUE
```

The two agree: M2SFCA differs from E2SFCA only in the weights of step 5.

## Other indices from the same blocks

The three-step FCA (3SFCA) and Huff-2SFCA share each cell’s demand among
its practices before the two steps.
[`calc_choice()`](https://songyosr.github.io/spax/reference/calc_choice.md)
turns the decay weights into **selection weights**: each practice’s
share of the cell’s weights, which add up to 1 at every cell. With an
attractiveness, here GP FTE, each weight is first multiplied by it, as
in the Huff model. Both indices then use the decay weight times the
selection weight in both steps, and
[`compute_access()`](https://songyosr.github.io/spax/reference/compute_access.md)
does the rest.

``` r

# 3SFCA: each cell's weights divided by their sum
selection <- calc_choice(weights)
range(values(sum(selection), mat = FALSE), na.rm = TRUE)
```

``` spax-output
[1] 1 1
```

``` r

sfca3_weights <- weights * selection
sfca3_by_blocks <- compute_access(pop, supply, sfca3_weights, sfca3_weights)

# Huff-2SFCA: the same, with each weight times the practice's GP FTE
huff_selection <- calc_choice(weights, attractiveness = supply)
huff_weights <- weights * huff_selection
huff_by_blocks <- compute_access(pop, supply, huff_weights, huff_weights)

# The same indices from spax()
sfca3 <- spax(grid_data, spec_3sfca(decay = decay_gaussian(sigma = 10)))
huff2sfca <- spax(grid_data, spec_huff2sfca(decay = decay_gaussian(sigma = 10)))
c(
  `3SFCA` = all.equal(
    values(sfca3_by_blocks, mat = FALSE),
    values(sfca3$accessibility, mat = FALSE)
  ),
  `Huff-2SFCA` = all.equal(
    values(huff_by_blocks, mat = FALSE),
    values(huff2sfca$accessibility, mat = FALSE)
  )
)
```

``` spax-output
     3SFCA Huff-2SFCA 
      TRUE       TRUE 
```

The selection weights add up to 1 at every cell, as the range shows, and
both rebuilds agree with
[`spax()`](https://songyosr.github.io/spax/reference/spax.md). The same
blocks build an index of your own: any two weight stacks on the grid of
the demand can go into
[`compute_access()`](https://songyosr.github.io/spax/reference/compute_access.md).
[Which FCA
index?](https://songyosr.github.io/spax/articles/fca-methods.md) maps
these indices side by side.

## Why the calculation covers the whole mainland

Cropping the inputs to the Truro window before calculating would be
quicker. But every practice, including those outside the window, would
then gather only the demand inside it. The output gives cell quantiles
of the percentage difference between the cropped and the full
calculation, for cells in the window.

``` r

# Calculate on the Truro window only
truro_data <- prep_data(
  crop(pop, truro_window),
  gp,
  crop(travel_grid, truro_window),
  id_col = "practice",
  supply_cols = "gp_fte_extg"
)
cropped <- spax(truro_data, spec_e2sfca(decay = decay_gaussian(sigma = 10)))

# Compare with the full calculation over the same cells
in_window <- values(crop(by_spec$accessibility, truro_window), mat = FALSE)
relative <- values(cropped$accessibility, mat = FALSE) / in_window - 1
round(quantile(100 * relative, c(0, 0.5, 1), na.rm = TRUE), 1)
```

``` spax-output
    0%    50%   100% 
  15.5   54.5 4980.0 
```

Every cell’s index rises: by 15.5% at the least affected cell and 54.5%
at the median. The worst cell, on the window’s edge, rises to about 51
times its full value. It picks up practices outside the window, whose
small remaining demand gives them very high ratios. Calculate on the
full study area and crop only for display.

## Next

- [Which FCA
  index?](https://songyosr.github.io/spax/articles/fca-methods.md): the
  five indices mapped side by side, and how much supply each hands back.
- [Grid resolution and computational
  cost](https://songyosr.github.io/spax/articles/grid-resolution.md):
  what the cell size does to the index and the run time.
- [An FCA index or an allocation
  model?](https://songyosr.github.io/spax/articles/model-choice.md):
  when the question is where people go rather than supply per resident.
- [Reading
  results](https://songyosr.github.io/spax/articles/reading-results.md):
  what an FCA index measures.
