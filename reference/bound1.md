# Thailand Region 12 ADM1 Boundaries with Attributes

A dataset representing administrative boundaries (ADM1) of Region 12 in
Thailand, along with key attributes.

## Usage

``` r
bound1
```

## Format

An \`sf\` object with 7 features and the following attributes:

- Shape_Leng:

  Length of the boundary in map units.

- Shape_Area:

  Area of the boundary in map units.

- ADM1_EN:

  Province name in English.

- ADM1_TH:

  Province name in Thai.

- ADM1_PCODE:

  Province administrative code.

- ADM0_EN:

  Country name in English.

- ADM0_TH:

  Country name in Thai.

- ADM0_PCODE:

  Country administrative code.

- date:

  Date the boundary was created.

- validOn:

  Date the boundary became valid.

- geometry:

  The geometry column containing spatial polygons.

## Source

Data processed from Thailand's official administrative boundaries,
available at \[Humanitarian Data Exchange
(HDX)\](https://data.humdata.org/dataset/thailand-administrative-boundaries).

## Details

This dataset provides detailed administrative information for Region 12
provinces in Thailand.

## Examples

``` r
if (FALSE) { # \dontrun{
library(sf)
library(ggplot2)

bound1 # already lazy-loaded - no need to run data(bound1)

# Plot the ADM1 boundaries with province names
ggplot(bound1) +
  geom_sf(aes(fill = ADM1_EN)) +
  geom_sf_label(aes(label = ADM1_EN)) +
  ggtitle("Thailand Region 12 ADM1 Boundaries")
} # }
```
