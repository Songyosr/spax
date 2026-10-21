# Demand from population groups and group-specific rates

Multiplies population counts in each group (for example, age bands) by a
group-specific rate, sums over groups and applies a common scale (for
example, the share of cases that use a given service): \$\$D_i = s
\sum_b r_b \\ P\_{ib}.\$\$

## Usage

``` r
calc_demand(
  groups,
  rates,
  scale = 1,
  total = NULL,
  fallback = c("mean", "error")
)
```

## Arguments

- groups:

  A multilayer `SpatRaster` with one named layer per group, or a numeric
  matrix with one named column per group and one row per origin. A
  missing group value counts as zero; a cell whose groups are all
  missing has missing demand (unless `total` supplies it).

- rates:

  A named numeric vector of nonnegative rates, one per group. Every
  group must have a rate (use 0 for groups without demand) and every
  rate a group.

- scale:

  A positive multiplier applied to every cell (default 1).

- total:

  Optional total population: a one-layer `SpatRaster` on the same grid
  as `groups`, or a numeric vector with one value per row of `groups`.

- fallback:

  What to do with cells that have a total but no group population:
  `"mean"` uses the overall rate, `"error"` stops.

## Value

Demand with the same layout as a single group: a one-layer `SpatRaster`
or a numeric vector. Attributes record the overall implied rate
(`implied_rate`, before `scale`), the number of fallback cells
(`fallback_cells`) and the rates used.

## Details

Gridded products often provide group counts and a total population that
do not sum exactly. With `total`, each cell's group composition is
applied to its total instead: \\D_i = s \\ T_i \sum_b r_b P\_{ib} /
\sum_b P\_{ib}\\. Cells with a total but no group population use the
overall rate across all cells (`fallback = "mean"`) or stop
(`fallback = "error"`).

## See also

Other allocation steps:
[`bootstrap_fit()`](https://songyosr.github.io/spax/reference/bootstrap_fit.md),
[`calc_travel_time()`](https://songyosr.github.io/spax/reference/calc_travel_time.md),
[`check_fit()`](https://songyosr.github.io/spax/reference/check_fit.md),
[`clm_allocation()`](https://songyosr.github.io/spax/reference/clm_allocation.md),
[`compare_fits()`](https://songyosr.github.io/spax/reference/compare_fits.md),
[`decay_kernel()`](https://songyosr.github.io/spax/reference/decay_kernel.md),
[`evaluate_allocation()`](https://songyosr.github.io/spax/reference/evaluate_allocation.md),
[`fit_allocation()`](https://songyosr.github.io/spax/reference/fit_allocation.md),
[`huff_allocation()`](https://songyosr.github.io/spax/reference/huff_allocation.md),
[`predict_allocation()`](https://songyosr.github.io/spax/reference/predict_allocation.md),
[`prepare_allocation()`](https://songyosr.github.io/spax/reference/prepare_allocation.md)

## Examples

``` r
library(terra)

# Two age groups on the Cornwall population grid: under 15 and 15 or over
pop <- read_spax_example("cornwall_pop.tif")
groups <- c(pop * 0.16, pop * 0.84)
names(groups) <- c("age_0_14", "age_15_plus")

# A condition affecting 0.5% of people aged 15 or over, 80% of whom use
# the service, applied to the total population grid
demand <- calc_demand(groups, c(age_0_14 = 0, age_15_plus = 0.005),
                      scale = 0.8, total = pop)
attr(demand, "implied_rate")
#> [1] 0.0042
```
