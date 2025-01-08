test_that("save_spax handles directory mode correctly", {
  skip_if_not_installed("terra")

  td <- .create_test_data()
  result <- spax_e2sfca(
    demand = td$demand,
    supply = td$supply_vector,
    distance = td$distance,
    decay_params = list(method = "gaussian", sigma = 2),
    snap = TRUE
  )

  tmp_dir <- tempdir()

  # Test directory creation
  test_name <- "test_save"
  test_path <- file.path(tmp_dir, test_name)

  # Should create directory and save files inside it
  expect_no_error(save_spax(result, test_path, dir = TRUE))
  expect_true(dir.exists(test_path))
  expect_true(file.exists(file.path(
    test_path,
    paste0(test_name, "_spax.rds")
  )))

  # Should fail if directory exists and overwrite = FALSE
  expect_error(
    save_spax(result, test_path, dir = TRUE),
    "Directory already exists"
  )

  # Should work with overwrite = TRUE
  expect_no_error(save_spax(result, test_path, dir = TRUE, overwrite = TRUE))
})

test_that("save_spax handles file mode correctly", {
  skip_if_not_installed("terra")

  td <- .create_test_data()
  result <- spax_e2sfca(
    demand = td$demand,
    supply = td$supply_vector,
    distance = td$distance,
    decay_params = list(method = "gaussian", sigma = 2),
    snap = TRUE
  )

  tmp_dir <- tempdir()
  test_file <- file.path(tmp_dir, "test_file")

  # Should save files directly in target directory
  expect_no_error(save_spax(result, test_file))
  expect_true(file.exists(paste0(test_file, "_spax.rds")))
  expect_true(file.exists(paste0(test_file, "_accessibility.tif")))

  # Should fail if files exist and overwrite = FALSE
  expect_error(
    save_spax(result, test_file),
    "Files exist"
  )

  # Should work with overwrite = TRUE
  expect_no_error(save_spax(result, test_file, overwrite = TRUE))
})

test_that("save_spax and read_spax handle single-layer case correctly", {
  skip_if_not_installed("terra")

  # Create test object
  td <- .create_test_data()
  result <- spax_e2sfca(
    demand = td$demand,
    supply = td$supply_vector,
    distance = td$distance,
    decay_params = list(method = "gaussian", sigma = 2),
    snap = TRUE
  )

  # Test file mode
  tmp_dir <- tempdir()
  test_file <- file.path(tmp_dir, "test_file")

  expect_no_error(save_spax(result, test_file, overwrite = TRUE))
  loaded <- read_spax(test_file)

  # Verify structure and content
  expect_s3_class(loaded, "spax")
  expect_equal(names(loaded), names(result))
  expect_true(terra::compareGeom(loaded$accessibility, result$accessibility))
  expect_equal(
    terra::values(loaded$accessibility),
    terra::values(result$accessibility),
    tolerance = 1e-6
  )
})

test_that("save_spax and read_spax handle multi-layer case correctly", {
  skip_if_not_installed("terra")

  # Create test object with multiple supply measures
  td <- .create_test_data()
  result <- spax_e2sfca(
    demand = td$demand,
    supply = td$supply_df,
    distance = td$distance,
    decay_params = list(method = "gaussian", sigma = 2),
    demand_normalize = "standard",
    id_col = "id",
    supply_cols = c("doctors", "nurses"),
    snap = TRUE
  )

  # Test directory mode
  tmp_dir <- tempdir()
  test_path <- file.path(tmp_dir, "test_dir")

  expect_no_error(save_spax(result, test_path, dir = TRUE))
  loaded <- read_spax(file.path(test_path, basename(test_path)))

  # Verify structure and content
  expect_s3_class(loaded, "spax")
  expect_equal(names(loaded), names(result))
  expect_true(terra::compareGeom(loaded$accessibility, result$accessibility))
  expect_equal(
    terra::values(loaded$accessibility),
    terra::values(result$accessibility),
    tolerance = 1e-6
  )
  expect_equal(
    names(loaded$accessibility),
    names(result$accessibility)
  )
})
