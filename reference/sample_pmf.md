# Sample Points from Probability Surface with Multiple Realizations

Generates multiple realizations of point patterns from a probability
mass function (PMF) surface using a vectorized approach. The function
supports both deterministic sampling with fixed sample sizes and
probabilistic sampling based on population parameters.

## Usage

``` r
sample_pmf(
  x,
  n = NULL,
  size = NULL,
  prob = NULL,
  method = "poisson",
  iterations = 1,
  evolve_prop = 1,
  seed = NULL,
  replace_0 = TRUE,
  snap = FALSE,
  ...
)
```

## Arguments

- x:

  SpatRaster representing spatial distribution. Can be either: - A PMF
  where values sum to 1 - Raw density values that will be converted to
  PMF (except in snap mode)

- n:

  Integer or NULL. Fixed number of samples for deterministic sampling.
  If specified, overrides probabilistic sampling parameters.

- size:

  Integer or NULL. Population size parameter for probability
  distributions. If NULL and input is density (not PMF), will be
  calculated from data.

- prob:

  Numeric between 0 and 1. Probability parameter for sampling methods
  (e.g., disease prevalence, detection probability).

- method:

  Character string or function specifying sampling method: - "poisson":
  Random size with mean = size \* prob - "binomial": Random size from
  binomial(size, prob) - "nbinom": Random size from negative
  binomial(size, prob) - custom function: Must return vector of length
  'iterations'

- iterations:

  Integer. Number of realizations to generate (default = 1)

- evolve_prop:

  Numeric between 0 and 1. Controls sample evolution between
  iterations: - 1 (default): Independent sampling, all new samples each
  iteration - 0: No new samples, returns only first iteration - (0,1):
  Proportion of samples that evolve each iteration

- seed:

  Integer or NULL. Random seed for reproducibility

- replace_0:

  Logical. If TRUE, replaces zero values with NA (default = TRUE)

- snap:

  Logical. If TRUE, enters fast mode with minimal validation (default =
  FALSE)

- ...:

  Additional arguments passed to custom sampling function

## Value

SpatRaster with one layer per realization (or single layer if
evolve_prop = 0), containing counts of samples per cell. If replace_0 =
TRUE, cells with zero counts contain NA.

## Examples

``` r
if (FALSE) { # \dontrun{
# Create sample population density
r <- terra::rast(nrows = 10, ncols = 10)
terra::values(r) <- runif(100) * 100

# Example 1: Independent sampling (default)
samples1 <- sample_pmf(r, n = 50, iterations = 5)

# Example 2: Evolving samples
samples2 <- sample_pmf(r,
  n = 50,
  iterations = 5,
  evolve_prop = 0.3 # 30% new samples each iteration
)

# Example 3: Disease case simulation with evolution
samples3 <- sample_pmf(r,
  size = 10000, # population size
  prob = 0.001, # disease prevalence
  method = "poisson",
  iterations = 100,
  evolve_prop = 0.2
)
} # }
```
