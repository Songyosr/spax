# Update Probability Mass Function with Likelihood Surface

Updates a spatial probability mass function (PMF) using a likelihood
surface following Bayesian principles. The function combines: - Prior:
Initial spatial PMF representing prior beliefs - Likelihood: Surface
showing relative intensity of observations - Posterior: Updated PMF
incorporating both prior and likelihood

Values in the posterior will be higher where both prior probability and
likelihood are high, representing areas supported by both prior beliefs
and observed data.

## Usage

``` r
update_pmf(prior, likelihood, normalize = TRUE, density = FALSE, snap = FALSE)
```

## Arguments

- prior:

  SpatRaster containing prior PMF (each layer summing to 1)

- likelihood:

  SpatRaster containing likelihood surface. With several layers, prior
  and likelihood must have the same number of layers, or one of them a
  single layer that applies to every layer of the other.

- normalize:

  Logical; if TRUE ensure output sums to 1 (default TRUE), each layer of
  a multi-layer posterior separately

- density:

  Logical; if TRUE return density surface instead of PMF

- snap:

  Logical; if TRUE skip validation

## Value

SpatRaster containing either: - Posterior PMF if density = FALSE -
Posterior density if density = TRUE

## Examples

``` r
pop <- read_spax_example("cornwall_pop.tif")

# Group small areas into larger units (LSOA names end in a letter; dropping it
# gives the containing MSOA) so every unit covers several grid cells
areas <- terra::vect(sf::st_transform(cornwall_lsoa, 4326))
areas$msoa <- sub("[A-Z]$", "", areas$name)
msoa <- terra::aggregate(areas, by = "msoa", fun = "sum")

# Likelihood from a rate, not a count: the share of residents registered
# with a GP outside Cornwall (counts would double-count population)
msoa$out_rate <- msoa$sum_registered_out / msoa$sum_registered
likelihood <- transform_likelihood(msoa, value_col = "out_rate", template = pop)

# Prior from population; the posterior is proportional to population times
# the out-of-area rate
prior <- transform_pmf(pop)
posterior <- update_pmf(prior, likelihood)
```
