# Constructor for spax summary class

Creates a new spax summary object containing analysis results
statistics. This is an internal constructor and should be used with
.chck_spax_summary().

## Usage

``` r
.new_spax_summary(
  accessibility = NULL,
  facilities = NULL,
  model_specific = NULL,
  type = NULL,
  parameters = NULL
)
```

## Arguments

- accessibility:

  List of accessibility measure statistics

- facilities:

  List of facility-level statistics (optional)

- model_specific:

  List containing model-specific statistics (optional)

- type:

  Character string specifying model type

- parameters:

  List of model parameters

## Value

A summary.spax object
