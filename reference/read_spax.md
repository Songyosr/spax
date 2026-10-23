# Read a spax object from disk

Read a spax object from disk

## Usage

``` r
read_spax(file)
```

## Arguments

- file:

  Character; path/name of saved spax files

## Value

A spax object

## Examples

``` r
# E2SFCA on the Cornwall population grid for two supply measures
pop <- read_spax_example("cornwall_pop.tif")
travel <- read_spax_example("cornwall_gp_travel.tif")
result <- spax_e2sfca(pop, sf::st_drop_geometry(cornwall_gp), travel,
                      decay_params = list(method = "gaussian", sigma = 10),
                      id_col = "practice",
                      supply_cols = c("gp_fte_extg", "gp_fte"))

# Save it to temporary files
path <- tempfile("cornwall_e2sfca")
save_spax(result, path)

# Read it back: the layers keep their names and values
restored <- read_spax(path)
names(restored$accessibility)
#> [1] "gp_fte_extg" "gp_fte"     
all.equal(terra::values(restored$accessibility),
          terra::values(result$accessibility))
#> [1] TRUE

# Remove the files
unlink(paste0(path, c("_spax.rds", "_accessibility.tif", "_names.rds")))
```
