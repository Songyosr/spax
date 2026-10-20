# save_spax()/read_spax(): round trips and refusals.

test_that("save_spax and read_spax round-trip in file and directory modes", {
  skip_if_no_proj()
  td <- fca_toy()
  result <- spax_e2sfca(
    demand = td$demand,
    supply = td$supply_df,
    distance = td$distance,
    decay_params = list(method = "gaussian", sigma = 2),
    id_col = "id",
    supply_cols = c("doctors", "nurses"),
    snap = TRUE
  )
  single <- result
  single$accessibility <- result$accessibility[[1]]
  tmp <- withr::local_tempdir()

  expect_round_trip <- function(saved, loaded) {
    expect_s3_class(loaded, "spax")
    expect_named(loaded, names(saved))
    expect_identical(loaded[names(loaded) != "accessibility"],
                     saved[names(saved) != "accessibility"])
    expect_true(terra::compareGeom(loaded$accessibility, saved$accessibility))
    expect_equal(names(loaded$accessibility), names(saved$accessibility))
    expect_equal(terra::values(loaded$accessibility),
                 terra::values(saved$accessibility), tolerance = 1e-6)
  }

  # File mode: files sit next to the given path; a layer-names file only for
  # multi-layer results.
  file_base <- file.path(tmp, "single")
  expect_invisible(save_spax(single, file_base))
  expect_true(all(file.exists(paste0(file_base, c("_spax.rds", "_accessibility.tif")))))
  expect_false(file.exists(paste0(file_base, "_names.rds")))
  expect_round_trip(single, read_spax(file_base))
  expect_error(save_spax(single, file_base), "Files exist")
  expect_no_error(save_spax(single, file_base, overwrite = TRUE))

  # Directory mode: the files go inside a directory named after the path.
  dir_path <- file.path(tmp, "multi")
  save_spax(result, dir_path, dir = TRUE)
  expect_true(file.exists(file.path(dir_path, "multi_names.rds")))
  expect_round_trip(result, read_spax(file.path(dir_path, "multi")))
  expect_error(save_spax(result, dir_path, dir = TRUE), "Directory already exists")
  expect_no_error(save_spax(result, dir_path, dir = TRUE, overwrite = TRUE))
})

test_that("save_spax and read_spax refuse invalid objects and missing files", {
  tmp <- withr::local_tempdir()
  expect_error(save_spax(list(accessibility = toy_rast(1)), file.path(tmp, "x")),
               "classes: spax")
  expect_error(read_spax(file.path(tmp, "absent")), "Spax object file not found")

  # An object file without its raster is incomplete.
  saveRDS(list(), file.path(tmp, "orphan_spax.rds"))
  expect_error(read_spax(file.path(tmp, "orphan")),
               "Accessibility raster file not found")
})

test_that("save_spax() refuses an allocation result and names the way to save one", {
  tr <- matrix(c(1, 2, 3, 2, 1, 2), 3, 2, dimnames = list(c("a", "b", "c"), c("f1", "f2")))
  d <- prep_data(c(a = 100, b = 100, c = 100), c(f1 = 5, f2 = 5), tr)
  e <- spax(d, spec_clm(decay_gaussian(2), outside = 0.1))
  file <- file.path(withr::local_tempdir(), "allocation")
  expect_error(save_spax(e, file),
               "save_spax() saves FCA results; save an allocation result with saveRDS()",
               fixed = TRUE)
  expect_length(list.files(dirname(file)), 0L)
  # the way it names works
  path <- paste0(file, ".rds")
  saveRDS(e, path)
  expect_identical(coef(readRDS(path)), coef(e))
})
