# Create a spax summary object with validation

Main function for creating spax summary objects, performing validation
first. For internal use by summary methods.

## Usage

``` r
.create_spax_summary(
  accessibility = NULL,
  facilities = NULL,
  model_specific = NULL,
  type = NULL,
  parameters = NULL,
  snap = FALSE
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

- snap:

  Logical; if TRUE skip validation (default = FALSE)

## Value

A summary.spax object
