test_that("Cornwall GP tables have aligned identifiers", {
  gp <- sf::st_drop_geometry(cornwall_gp)
  areas <- sf::st_drop_geometry(cornwall_lsoa)

  expect_equal(nrow(gp), 55L)
  expect_equal(nrow(areas), 337L)
  expect_false(anyDuplicated(gp$practice) > 0)
  expect_false(anyDuplicated(areas$lsoa) > 0)
  expect_equal(sf::st_crs(cornwall_gp)$epsg, 27700L)
  expect_equal(sf::st_crs(cornwall_lsoa)$epsg, 27700L)

  expect_true(all(cornwall_flows$practice %in% gp$practice))
  expect_true(all(cornwall_flows$lsoa %in% areas$lsoa))
  expect_true(all(cornwall_flows$patients > 0))
  in_area <- tapply(cornwall_flows$patients, cornwall_flows$lsoa, sum)
  expect_equal(as.integer(in_area[areas$lsoa]), areas$registered_in)
  expect_equal(areas$registered_out, areas$registered - areas$registered_in)

  expect_true(all(gp$gp_fte_extg > 0))
  expect_true(all(gp$gp_fte >= gp$gp_fte_extg - 1e-8))
})

test_that("Cornwall travel matrix matches the tables and the travel grid", {
  scilly_area <- cornwall_lsoa$scilly
  scilly_gp <- cornwall_gp$scilly

  expect_identical(rownames(cornwall_travel), cornwall_lsoa$lsoa)
  expect_identical(colnames(cornwall_travel), cornwall_gp$practice)
  expect_type(cornwall_travel, "integer")
  expect_true(all(cornwall_travel >= 1L, na.rm = TRUE))
  # Mainland pairs are complete; the Isles of Scilly have no road link.
  expect_false(anyNA(cornwall_travel[!scilly_area, !scilly_gp]))
  expect_true(all(is.na(cornwall_travel[scilly_area, !scilly_gp])))
  expect_true(all(is.na(cornwall_travel[!scilly_area, scilly_gp])))

  pop <- read_spax_example("cornwall_pop.tif")
  travel <- read_spax_example("cornwall_gp_travel.tif")
  expect_identical(names(travel), cornwall_gp$practice)
  expect_true(terra::compareGeom(pop, travel, stopOnError = FALSE))

  skip_if_no_proj()
  centroids <- sf::st_transform(sf::st_as_sf(
    sf::st_drop_geometry(cornwall_lsoa), coords = c("cent_x", "cent_y"),
    crs = 27700), 4326)
  cells <- terra::cellFromXY(travel, sf::st_coordinates(centroids))
  mainland <- which(!scilly_area)[1:5]
  expect_equal(unname(as.matrix(travel[cells[mainland]])[, !scilly_gp]),
               unname(cornwall_travel[mainland, !scilly_gp]),
               ignore_attr = TRUE)
})
