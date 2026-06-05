# Create a spax object with validation

Main function for creating spax objects, performing validation first.
For internal use by model functions.

## Usage

``` r
.create_spax(
  accessibility = NULL,
  type = NULL,
  parameters = list(),
  facilities = NULL,
  iterations = NULL,
  variations = NULL,
  call = NULL,
  snap = FALSE
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

- snap:

  Logical; if TRUE skip validation (default = FALSE)

## Value

A spax object
