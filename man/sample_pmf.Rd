% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/02-sample_pmf.R
\name{sample_pmf}
\alias{sample_pmf}
\title{Sample Points from Probability Surface with Multiple Realizations}
\usage{
sample_pmf(
  x,
  n = NULL,
  size = NULL,
  prob = NULL,
  method = "poisson",
  iterations = 1,
  seed = NULL,
  replace_0 = TRUE,
  snap = FALSE,
  ...
)
}
\arguments{
\item{x}{SpatRaster representing spatial distribution. Can be either:
- A PMF where values sum to 1
- Raw density values that will be converted to PMF (except in snap mode)}

\item{n}{Integer or NULL. Fixed number of samples for deterministic sampling.
If specified, overrides probabilistic sampling parameters.}

\item{size}{Integer or NULL. Population size parameter for probability distributions.
If NULL and input is density (not PMF), will be calculated from data.}

\item{prob}{Numeric between 0 and 1. Probability parameter for sampling methods
(e.g., disease prevalence, detection probability).}

\item{method}{Character string or function specifying sampling method:
- "poisson": Random size with mean = size * prob
- "binomial": Random size from binomial(size, prob)
- "nbinom": Random size from negative binomial(size, prob)
- custom function: Must return vector of length 'iterations'}

\item{iterations}{Integer. Number of realizations to generate (default = 1)}

\item{seed}{Integer or NULL. Random seed for reproducibility}

\item{replace_0}{Logical. If TRUE, replaces zero values with NA (default = TRUE)}

\item{snap}{Logical. If TRUE, enters fast mode with:
- Minimal input validation
- Requires input to already be PMF
- Skips automatic PMF conversion
- No layer naming
Use for performance in iteration-heavy applications.}

\item{...}{Additional arguments passed to custom sampling function}
}
\value{
SpatRaster with one layer per realization, containing counts of samples per cell.
        If replace_0 = TRUE, cells with zero counts contain NA.
}
\description{
Generates multiple realizations of point patterns from a probability mass function (PMF)
surface using a vectorized approach. The function supports both deterministic sampling
with fixed sample sizes and probabilistic sampling based on population parameters.
}
\details{
The function supports two main sampling approaches:
1. Deterministic: Specify exact number of samples via 'n'
2. Probabilistic: Generate random sample sizes using built-in or custom distributions

In snap mode, the function assumes inputs are valid and skips certain checks for
performance. The input must already be a proper PMF in this mode.
}
\examples{
\dontrun{
# Create sample population density
r <- terra::rast(nrows = 10, ncols = 10)
terra::values(r) <- runif(100) * 100

# Example 1: Fixed sample size
samples1 <- sample_pmf(r, n = 50, iterations = 5)

# Example 2: Disease case simulation
samples2 <- sample_pmf(r,
  size = 10000, # population size
  prob = 0.001, # disease prevalence
  method = "poisson",
  iterations = 100
) # Monte Carlo iterations

# Example 3: Using snap mode with pre-computed PMF
pmf <- transform_pmf(r)
samples3 <- sample_pmf(pmf,
  n = 1000,
  iterations = 50,
  snap = TRUE
)
}

}
\seealso{
\code{\link{transform_pmf}} for converting density to PMF
}
