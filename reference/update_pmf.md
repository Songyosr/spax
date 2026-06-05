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

  SpatRaster containing prior PMF

- likelihood:

  SpatRaster containing likelihood surface

- normalize:

  Logical; if TRUE ensure output sums to 1 (default TRUE)

- density:

  Logical; if TRUE return density surface instead of PMF

- snap:

  Logical; if TRUE skip validation

## Value

SpatRaster containing either: - Posterior PMF if density = FALSE -
Posterior density if density = TRUE

## Examples

``` r
if (FALSE) { # \dontrun{
# Create prior from population
prior <- transform_pmf(pop)

# Create likelihood from cases
likelihood <- transform_likelihood(
  case_spatial,
  value_col = "cases",
  template = pop
)

# Update prior with likelihood
posterior <- update_pmf(prior, likelihood)

# Get density surface
density <- update_pmf(prior, likelihood, density = TRUE)
} # }
```
