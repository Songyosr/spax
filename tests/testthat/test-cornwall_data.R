test_that("Cornwall GP tables have aligned identifiers", {
  gp <- sf::st_drop_geometry(cornwall_gp)
  areas <- sf::st_drop_geometry(cornwall_lsoa)

  expect_equal(nrow(gp), 55L)
  expect_equal(nrow(areas), 337L)
  expect_false(anyDuplicated(gp$practice) > 0)
  expect_false(anyDuplicated(areas$lsoa) > 0)
  # Every bundled layer is on the WGS 84 datum of the rasters, so the examples
  # and guides never change datum; grid coordinates are kept as columns
  expect_equal(sf::st_crs(cornwall_gp)$epsg, 4326L)
  expect_equal(sf::st_crs(cornwall_lsoa)$epsg, 4326L)
  expect_true(all(c("easting", "northing") %in% names(gp)))
  expect_true(all(c("cent_x", "cent_y", "cent_lon", "cent_lat") %in% names(areas)))

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

  # Grid times are whole minutes from 1 to 180, only for populated cells, and
  # no cell reaches both the Scilly practice and a mainland practice.
  times <- terra::values(travel)
  expect_true(all(times >= 1 & times <= 180 & times == round(times), na.rm = TRUE))
  reaches_mainland <- rowSums(!is.na(times[, !scilly_gp])) > 0
  reaches_scilly <- !is.na(times[, scilly_gp])
  expect_true(any(reaches_mainland) && any(reaches_scilly))
  expect_false(any(reaches_mainland & reaches_scilly))
  expect_false(any((reaches_mainland | reaches_scilly) &
                     is.na(terra::values(pop)[, 1])))

  # The travel matrix is the travel grid at each small area's centroid cell,
  # and each practice's own cell is the 1-minute floor of its layer
  cells <- terra::cellFromXY(travel, cbind(cornwall_lsoa$cent_lon, cornwall_lsoa$cent_lat))
  expect_equal(unname(as.matrix(travel[cells])), unname(cornwall_travel),
               ignore_attr = TRUE)
  own <- terra::cellFromXY(travel, sf::st_coordinates(cornwall_gp))
  expect_equal(vapply(seq_along(own), function(j) travel[own[j]][[j]], numeric(1)),
               rep(1, nrow(cornwall_gp)))
})

test_that("read_spax_example lists and loads the bundled rasters", {
  expect_setequal(read_spax_example(),
                  c("cornwall_gp_travel.tif", "cornwall_pop.tif"))
  expect_equal(names(read_spax_example("cornwall_pop.tif")), "pop")
  expect_error(read_spax_example("cornwall_beds.tif"), "not found")
})
