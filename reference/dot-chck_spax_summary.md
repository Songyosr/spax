# Validate inputs for spax summary object creation

Checks if inputs are valid for creating a spax summary object. Called
before object creation to ensure valid inputs.

## Usage

``` r
.chck_spax_summary(
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

Invisible TRUE if validation passes
