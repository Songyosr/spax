# Aggregate weighted values from a raster

General-purpose function to aggregate values from a raster (single or
multi-layer) using a stack of weight rasters. Each layer in the weights
represents a different aggregation unit.

## Usage

``` r
gather_weighted(values, weights, na.rm = TRUE, simplify = FALSE, snap = FALSE)
```

## Arguments

- values:

  SpatRaster representing values to be aggregated. Can be single or
  multi-layer. If multi-layer, each layer represents a different
  realization/scenario.

- weights:

  Multi-layer SpatRaster where: - Each layer represents one aggregation
  unit - Values are weights (typically 0-1) for aggregation - Layer
  names should match unit IDs

- na.rm:

  Logical; if TRUE, remove NA values from computation (default = TRUE)

- simplify:

  Logical; if TRUE, returns vector (single layer) or matrix
  (multi-layer). If FALSE, returns data.frame in wide format. (default =
  FALSE)

- snap:

  Logical; if TRUE, avoid input validation and formatting. Used for
  performance optimization in internal functions.

## Value

Depending on input and simplify parameter: - If simplify=TRUE and single
layer: named vector - If simplify=TRUE and multi-layer: matrix with
rownames=units, colnames=scenarios - If simplify=FALSE: data.frame in
wide format with unit_id column

## Examples

``` r
# Load necessary library
library(terra)

# Create test data
values <- rast(matrix(1:9, 3, 3)) # Single layer
weights <- c(
  rast(matrix(runif(9), 3, 3)),
  rast(matrix(runif(9), 3, 3))
)
names(weights) <- c("unit1", "unit2")

# Single layer example
result1 <- gather_weighted(values, weights, simplify = TRUE)
result1_df <- gather_weighted(values, weights)

# Multi-layer example
values_multi <- c(values, values * 2) # Two scenarios
names(values_multi) <- c("sim1", "sim2")
result2 <- gather_weighted(values_multi, weights, simplify = TRUE) # Returns matrix
result2_df <- gather_weighted(values_multi, weights)
```
