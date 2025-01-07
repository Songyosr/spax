# R/utils-test_data_creators.R

#' Create test data for spax functions
#'
#' @description
#' Creates a small test dataset for internal testing of spax functions.
#' Uses seeded random values for reproducibility.
#'
#' @param seed Numeric seed for random number generation (default = 42)
#' @return A list containing:
#'   \item{demand}{SpatRaster of population/demand}
#'   \item{distance}{Multi-layer SpatRaster of distances}
#'   \item{supply_df}{data.frame of supply locations}
#'   \item{supply_matrix}{matrix version of supply data}
#'   \item{supply_vector}{vector version of supply data}
#'   \item{boundary}{sf polygon of study area}
#' @keywords internal
.create_test_data <- function(seed = 42) {
  # Set seed for reproducibility
  set.seed(seed)

  # Create small demand raster with random population (1-100)
  demand <- terra::rast(nrows = 5, ncols = 5)
  terra::values(demand) <- round(runif(25, 1, 100))

  # Create distance rasters with simple distance patterns
  dist1 <- terra::rast(nrows = 5, ncols = 5)
  dist2 <- terra::rast(nrows = 5, ncols = 5)

  # Generate distances - higher values toward edges for first facility
  terra::values(dist1) <- round(runif(25, 1, 60))
  # And opposite pattern for second facility
  terra::values(dist2) <- round(runif(25, 1, 60))

  distance <- c(dist1, dist2)
  names(distance) <- c("facility1", "facility2")

  # Sample supply data with random capacities (10-50)
  doctors <- round(runif(2, 10, 50))
  nurses <- round(runif(2, 10, 50))

  supply_df <- data.frame(
    id = c("facility1", "facility2"),
    doctors = doctors,
    nurses = nurses
  )

  supply_matrix <- matrix(
    c(doctors, nurses),
    nrow = 2,
    dimnames = list(c("facility1", "facility2"), c("doctors", "nurses"))
  )

  supply_vector <- setNames(doctors, c("facility1", "facility2"))

  # Create boundary
  boundary <- sf::st_bbox(c(xmin = 0, xmax = 5, ymin = 0, ymax = 5)) |>
    sf::st_as_sfc()

  list(
    demand = demand,
    distance = distance,
    supply_df = supply_df,
    supply_matrix = supply_matrix,
    supply_vector = supply_vector,
    boundary = boundary
  )
}
