# Validate inputs for spax object creation

Checks if inputs are valid for creating a spax object. Called before
object creation to ensure valid inputs.

## Usage

``` r
.chck_spax(
  accessibility = NULL,
  type = NULL,
  parameters = list(),
  facilities = NULL,
  iterations = NULL,
  variations = NULL,
  call = NULL
)
```

## Arguments

- accessibility:

  SpatRaster containing accessibility scores

- type:

  Character string specifying model type

- parameters:

  List of model parameters

- facilities:

  data.frame containing facility-level results (optional)

- iterations:

  List containing iteration info (optional)

- variations:

  List containing variation results (optional)

- call:

  The original function call

## Value

Invisible TRUE if validation passes
