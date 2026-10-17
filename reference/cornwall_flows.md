# Registered patients by small area and practice

Residents of each Cornwall and Isles of Scilly small area registered at
each of the 55 ICB practices, as observed flows for calibrating
allocation models.

## Usage

``` r
cornwall_flows
```

## Format

A data frame with 3,188 rows (non-zero pairs only) and columns:

- lsoa:

  LSOA 2021 code (see `cornwall_lsoa`).

- practice:

  Practice code (see `cornwall_gp`).

- patients:

  Registered patients (integer, unrounded and unsuppressed).

## Source

NHS England, Patients Registered at a GP Practice, July 2026 (LSOA 2021
file), Open Government Licence v3.0. See
`system.file("COPYRIGHTS", package = "spax")`.

## Details

Registration records where people are attached, not how often they use
services. Residents registered outside the ICB are summarised in
`cornwall_lsoa$registered_out`.

## See also

[cornwall_gp](https://songyosr.github.io/spax/reference/cornwall_gp.md),
[cornwall_lsoa](https://songyosr.github.io/spax/reference/cornwall_lsoa.md),
[cornwall_travel](https://songyosr.github.io/spax/reference/cornwall_travel.md)

## Examples

``` r
head(cornwall_flows)
#>        lsoa practice patients
#> 1 E01018750   L82012     2185
#> 2 E01018750   L82016       35
#> 3 E01018750   L82025        1
#> 4 E01018750   L82046        4
#> 5 E01018750   L82066        1
#> 6 E01018751   L82012     1691
# Practice totals from Cornwall residents
head(sort(tapply(cornwall_flows$patients, cornwall_flows$practice, sum),
          decreasing = TRUE))
#> Y04957 L82041 L82010 L82006 L82001 L82028 
#>  32755  29174  23824  19800  19387  17652 
```
