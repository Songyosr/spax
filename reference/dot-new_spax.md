# Constructor for spax class

Creates a new spax object for storing spatial accessibility analysis
results. This is an internal constructor and should be used with
.chck_spax().

## Usage

``` r
.new_spax(
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

A spax object
