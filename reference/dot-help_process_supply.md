# Process and validate supply data for accessibility calculations

Process and validate supply data for accessibility calculations

## Usage

``` r
.help_process_supply(
  supply,
  id_col = NULL,
  supply_cols = NULL,
  weight_ids = NULL
)
```

## Arguments

- supply:

  vector, matrix, or data.frame containing supply capacity values

- id_col:

  Character; column name for facility IDs if supply is a data.frame

- supply_cols:

  Character vector; names of supply columns if supply is a data.frame

- weight_ids:

  Character vector of IDs from weight layers for matching

## Value

List containing:

- values:

  Matrix or vector of supply values

- ids:

  Vector of facility IDs

- cols:

  Names of supply measures
