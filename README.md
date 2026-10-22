
<!-- README.md is generated from README.Rmd. Please edit that file -->

# spax: spatial accessibility and allocation modeling

<!-- badges: start -->

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.23438611.svg)](https://doi.org/10.5281/zenodo.23438611)
<!-- badges: end -->

spax models spatial access to services such as GP practices, and
estimates how quickly the pull of a facility falls with travel time from
where people actually go. Its allocation model is fitted to observed use
(area-to-facility flows or facility totals), so the decay is estimated
rather than assumed. The same inputs, decay curves and numeric engine
compute floating catchment area (FCA) accessibility indices: 2SFCA,
E2SFCA, M2SFCA, 3SFCA and Huff-2SFCA. Decay curves plug in: write your
own as an R function and use it in an index or a fitted model.

The `spax()` interface and the allocation model are experimental and may
change in later versions, while the one-call FCA functions
`spax_2sfca()`, `spax_e2sfca()` and `spax_m2sfca()` are stable.

## Install

Use R 4.4.0 or newer. This is the supported dependency and testing floor
for spax 0.6.0.

Install version 0.6.0 by its release tag rather than from the
repository’s default branch:

``` r
# install.packages("pak")
pak::pak("Songyosr/spax@v0.6.0")
```

Development continues on the `develop` branch
(`pak::pak("Songyosr/spax@develop")`).

Each release is archived on Zenodo; <doi:10.5281/zenodo.23438611> refers
to all versions (version 0.6.0 is <doi:10.5281/zenodo.23538560>). Cite
the version you used with `citation("spax")`.

## A first look

spax ships open data for GP care in Cornwall, in south-west England.
This example computes the enhanced two-step FCA index (E2SFCA) for
mainland Cornwall from a population grid, the GP practices with their GP
full-time equivalents (FTE) and travel minutes from every grid cell to
every practice.

``` r
library(spax)
library(terra)
library(sf)
library(dplyr)

gp <- cornwall_gp |>
  filter(!scilly) |>
  st_drop_geometry()
pop <- read_spax_example("cornwall_pop.tif")
travel_grid <- read_spax_example("cornwall_gp_travel.tif")[[gp$practice]]
# Keep cells with a travel time to every mainland practice
pop <- trim(mask(pop, sum(travel_grid)))
travel_grid <- crop(travel_grid, pop)
grid_data <- prep_data(pop, gp, travel_grid, id_col = "practice", supply_cols = "gp_fte_extg")

# The E2SFCA index with a Gaussian decay of travel scale 10 minutes
fca <- spax(grid_data, spec_e2sfca(decay = decay_gaussian(sigma = 10)))

# Mainland Cornwall's outline, in the grid's longitude and latitude
outline <- cornwall_lsoa |>
  filter(!scilly) |>
  st_union() |>
  st_transform(crs(pop)) |>
  vect()

plot(
  1000 * fca$accessibility,
  axes = FALSE,
  col = hcl.colors(50, "viridis"),
  main = "E2SFCA index, GP FTE per 1,000 residents"
)
lines(outline, lwd = 0.6)
```

<img src="man/figures/README-fca-1.png" alt="Raster map of the E2SFCA index for mainland Cornwall in GP FTE per 1,000 residents; values are highest in the west between Redruth, Truro and Falmouth and lowest on Bodmin Moor and at the tips of the Land's End and Lizard peninsulas."  />

The index is GP supply per resident, counting nearby practices more than
distant ones. It is highest in the bright band between Redruth, Truro
and Falmouth, below and left of center. It is lowest on Bodmin Moor, the
dark patch above and right of center, and at the tips of the Land’s End
(far left) and Lizard (bottom) peninsulas.

## A fitted model

The allocation model describes where the residents of each small area
register. Each practice attracts residents in proportion to its GP FTE,
discounted by travel time, and an outside option takes the residents
registered with a practice outside mainland Cornwall. `spec_clm()`
specifies this model, a conditional logit model (CLM) with an outside
option. By default it estimates two numbers: the travel scale σ of a
Gaussian decay, which sets how fast a practice’s pull falls with travel
time, and the outside weight v0. Here they are fitted to the
registrations of each small area at each practice.

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

# Fit the travel scale sigma and the outside weight v0 to the registrations
clm <- spax(d, spec_clm(), observed = flows)
```

    observed: 2,290 counts from 1 origin outside the data or left out by it are dropped; 5 counts at 1 facility outside the data are not fitted (outside = "known" uses them as outside counts)

``` r
coef(clm)
```

         sigma         v0 
    4.59099725 0.02733454 

`spax()` reports that it drops the 2,290 registrations of the Isles of
Scilly area, and does not fit the 5 mainland residents registered at the
Scilly practice, because the example keeps mainland Cornwall only.

The travel scale σ is about 4.6 minutes, so a practice 9 minutes away,
about twice σ, pulls only about 15% as strongly as one of the same size
next door. Registration is very local. The outside weight v0 acts like a
practice of 0.03 GP FTE next door to every area. With it the model
places 0.54% of registered residents with practices outside mainland
Cornwall, against 0.91% observed. This fit uses only the counts at
mainland practices. [Where do residents
register?](https://songyosr.github.io/spax/articles/allocation-workflow.html#when-outside-counts-are-known)
shows how to give it each area’s known outside count too.

## Learn more

The guides follow one story on the Cornwall data, and each answers one
question. Read them on the [package
website](https://songyosr.github.io/spax/). In R,
`vignette("<name>", package = "spax")` opens one if your installation
built the guides. If it finds nothing, use the website.

| Question | Guide | `vignette()` |
|:---|:---|:---|
| How accessible is GP care in Cornwall? | [Getting started](https://songyosr.github.io/spax/articles/spax.html) | `"spax"` |
| What is in the example data? | [The Cornwall GP example data](https://songyosr.github.io/spax/articles/cornwall-data.html) | `"cornwall-data"` |
| How do I bring my own data? | [Bringing your own data](https://songyosr.github.io/spax/articles/data-prep.html) | `"data-prep"` |
| Which FCA index should I use? | [Which FCA index?](https://songyosr.github.io/spax/articles/fca-methods.html) | `"fca-methods"` |
| What does the E2SFCA index compute, step by step? | [Inside the E2SFCA calculation](https://songyosr.github.io/spax/articles/e2sfca-steps.html) | `"e2sfca-steps"` |
| How fine a grid do I need, and at what cost? | [Grid resolution and computational cost](https://songyosr.github.io/spax/articles/grid-resolution.html) | `"grid-resolution"` |
| Where do residents register? | [Fitting an allocation model](https://songyosr.github.io/spax/articles/allocation-workflow.html) | `"allocation-workflow"` |
| What if a practice closed? | [What if a practice closed?](https://songyosr.github.io/spax/articles/scenario.html) | `"scenario"` |
| Should I use an FCA index or an allocation model? | [An FCA index or an allocation model?](https://songyosr.github.io/spax/articles/model-choice.html) | `"model-choice"` |
| How do I use my own decay, raster data or grid maps? | [Extending the model](https://songyosr.github.io/spax/articles/extending.html) | `"extending"` |
| How sure are the estimates? | [Bootstrap intervals](https://songyosr.github.io/spax/articles/facility-bootstrap.html) | `"facility-bootstrap"` |
| What do the results mean, and not mean? | [Reading results](https://songyosr.github.io/spax/articles/reading-results.html) | `"reading-results"` |
| Which function does what? | [Function map](https://songyosr.github.io/spax/articles/function-naming.html) | `"function-naming"` |

`help(package = "spax")` lists every function.

## Contributing and license

Report reproducible issues or discuss larger changes on the [project
repository](https://github.com/Songyosr/spax). Package code uses the MIT
license; see
[LICENSE](https://github.com/Songyosr/spax/blob/main/LICENSE.md). The
bundled Cornwall GP example data are openly licensed third-party material
(Open Government Licence v3.0 and Creative Commons Attribution), which the
MIT license does not cover. The installed notice file,
`system.file("COPYRIGHTS", package = "spax")` (source `inst/COPYRIGHTS`),
lists each source and its required attribution.
