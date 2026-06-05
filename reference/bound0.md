# Thailand Health Region 12 Boundary Area - Boundary

A dataset representing the geographic boundary of Health Region 12 in
Thailand without administrative details.

## Usage

``` r
bound0
```

## Format

An \`sf\` object with the following attributes:

- geometry:

  The geometry column containing spatial polygons for the boundary.

## Source

Data processed from Thailand's official administrative boundaries,
available at \[Humanitarian Data Exchange
(HDX)\](https://data.humdata.org/dataset/thailand-administrative-boundaries).

## Details

This dataset includes only the boundary area for Health Region 12,
useful for masking or spatial extent analysis.

## Examples

``` r
if (FALSE) { # \dontrun{
library(sf)
library(ggplot2)

# Load the dataset
bound0 # already lazy-loaded - no need to run data(bound0)

# Plot the boundary
ggplot(bound0) +
  geom_sf(fill = "lightblue") +
  ggtitle("Thailand Region 12 Boundary Area")
} # }
```
