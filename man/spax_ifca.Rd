% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/03-iterative_chain.R
\name{spax_ifca}
\alias{spax_ifca}
\title{Iterative Floating Catchment Area (iFCA) Analysis}
\usage{
spax_ifca(
  distance_raster,
  demand,
  supply,
  decay_params = list(method = "gaussian", sigma = 30),
  lambda = 0.5,
  max_iter = 100,
  tolerance = 1e-06,
  window_size = 5,
  convergence_type = c("utilization", "ratio"),
  snap = FALSE,
  debug = FALSE
)
}
\arguments{
\item{distance_raster}{A multi-layer SpatRaster where each layer represents distances
to one facility. All layers must share the same extent and resolution.}

\item{demand}{SpatRaster of demand distribution. Can be either:
- Single layer for static demand
- Multiple layers (matching max_iter) for dynamic demand}

\item{supply}{Numeric vector of facility capacities. Length must match number
of layers in distance_raster.}

\item{decay_params}{List of parameters for decay function:
\itemize{
  \item method: "gaussian", "exponential", "power", or custom function
  \item sigma: decay parameter controlling spatial interaction strength
  \item Additional parameters passed to custom decay functions
}}

\item{lambda}{Learning rate between 0 and 1 controlling convergence speed.
Lower values provide more stability but slower convergence.
Default: 0.5}

\item{max_iter}{Maximum number of iterations to attempt. For multi-layer demand,
this must match the number of demand layers. Default: 100}

\item{tolerance}{Convergence tolerance threshold. Iteration stops when the rolling
average of differences falls below this value. Default: 1e-6}

\item{window_size}{Size of rolling window for convergence checking. Default: 5}

\item{convergence_type}{Character string specifying convergence metric, one of:
"utilization" (default) or "ratio"}

\item{snap}{Logical; if TRUE enables fast computation mode returning only
utilization vector. Default: FALSE}

\item{debug}{Logical; if TRUE provides detailed convergence information.
Default: FALSE}
}
\value{
If snap = TRUE:
  Numeric vector of predicted utilization for all facilities

If snap = FALSE:
  A spax object containing:
  \describe{
    \item{accessibility}{SpatRaster of accessibility scores}
    \item{type}{Character string "iFCA"}
    \item{parameters}{List of model parameters including decay_params, lambda, etc.}
    \item{facilities}{data.frame with columns:
      \itemize{
        \item id: Facility identifiers
        \item utilization: Predicted facility utilization
        \item ratio: Supply-to-demand ratios
        \item attractiveness: Final facility attractiveness
      }
    }
    \item{iterations}{List containing:
      \itemize{
        \item history: Array of historical state values if converged
        \item convergence: Convergence details including iterations, status, etc.
      }
    }
    \item{call}{The original function call}
  }
}
\description{
Computes spatial accessibility scores using an iterative floating catchment area method
that combines Huff-model based choice probabilities with distance decay effects. This
enhanced version supports time-series demand patterns and customizable decay functions.

The iFCA method extends traditional FCA approaches by:
\itemize{
  \item Supporting dynamic demand patterns through multi-layer inputs
  \item Incorporating flexible distance decay specifications
  \item Iteratively balancing supply and demand
  \item Using a learning rate to control convergence
}
}
\examples{
\dontrun{
# Load example data
library(terra)
library(sf)

# Prepare inputs
pop <- read_spax_example("u5pd.tif")
distance <- read_spax_example("hos_iscr.tif")
hospitals <- st_drop_geometry(hc12_hos)

# Basic usage with default parameters
result <- spax_ifca(
  distance_raster = distance,
  demand = pop,
  supply = hospitals$s_doc,
  decay_params = list(
    method = "gaussian",
    sigma = 30  # 30-minute characteristic distance
  )
)

# Plot accessibility surface
plot(result$accessibility, main = "Doctor Accessibility (iFCA)")

# Examine facility-level results
head(result$facilities)

# Check convergence information
print(result$iterations$convergence)

# Fast computation mode - returns only utilization
util <- spax_ifca(
  distance_raster = distance,
  demand = pop,
  supply = hospitals$s_doc,
  decay_params = list(method = "gaussian", sigma = 30),
  snap = TRUE
)

# Compare predicted utilization with facility capacity
data.frame(
  id = hospitals$id,
  capacity = hospitals$s_doc,
  predicted = util
)

# Using custom decay function
custom_decay <- function(distance, sigma = 30, threshold = 60) {
  weights <- exp(-distance^2 / (2 * sigma^2))
  weights[distance > threshold] <- 0
  return(weights)
}

result_custom <- spax_ifca(
  distance_raster = distance,
  demand = pop,
  supply = hospitals$s_doc,
  decay_params = list(
    method = custom_decay,
    sigma = 30,
    threshold = 60
  )
)

}

}
\seealso{
\code{\link{calc_decay}} for available decay functions
}
