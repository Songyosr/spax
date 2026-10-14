vector_fixture <- function() {
  template <- terra::rast(nrows = 4, ncols = 4, xmin = 0, xmax = 4,
                          ymin = 0, ymax = 4, crs = "EPSG:3857")
  terra::values(template) <- 1:16
  points <- terra::vect(cbind(c(0.5, 2.5, 3.5), c(0.5, 1.5, 3.5)),
    atts = data.frame(cases = c(2, 5, 1), label = c("a", "b", "c")),
    crs = "EPSG:3857")
  list(template = template, points = points)
}

test_that("vector input accepts a numeric column", {
  f <- vector_fixture()
  expect_invisible(.chck_vector_input(f$points, "cases", f$template))

  pmf <- transform_pmf(f$points, value_col = "cases", template = f$template)
  expect_s4_class(pmf, "SpatRaster")
  expect_equal(terra::global(pmf, "sum", na.rm = TRUE)[[1]], 1)

  lik <- transform_likelihood(f$points, value_col = "cases", template = f$template)
  expect_s4_class(lik, "SpatRaster")
})

test_that("vector input accepts single and collinear points inside the template", {
  f <- vector_fixture()
  one <- terra::vect(cbind(1.5, 2.5), atts = data.frame(cases = 3), crs = "EPSG:3857")
  expect_invisible(.chck_vector_input(one, "cases", f$template))
  expect_equal(terra::global(transform_pmf(one, value_col = "cases", template = f$template),
                             "sum", na.rm = TRUE)[[1]], 1)
  line <- terra::vect(cbind(c(0.5, 3.5), c(2.5, 2.5)), atts = data.frame(cases = c(1, 2)),
                      crs = "EPSG:3857")
  expect_invisible(.chck_vector_input(line, "cases", f$template))
})

test_that("vector input rejects non-numeric columns and disjoint extents", {
  f <- vector_fixture()
  expect_error(.chck_vector_input(f$points, "label", f$template),
               "must contain numeric values")
  expect_error(.chck_vector_input(f$points, "missing", f$template), "not found")

  far <- terra::vect(cbind(100, 100), atts = data.frame(cases = 1),
                     crs = "EPSG:3857")
  expect_error(.chck_vector_input(far, "cases", f$template),
               "must intersect template extent")
})
