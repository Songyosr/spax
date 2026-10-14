# PMF tools: transform_pmf(), transform_likelihood(), update_pmf(),
# rasterize_demand() and transform_demand() on raster and vector input.

# A 4 x 4 projected grid (values 1..16) and three points with case counts.
# Building these sets a CRS, so callers skip without the PROJ database.
pmf_vector_inputs <- function() {
  template <- terra::rast(nrows = 4, ncols = 4, xmin = 0, xmax = 4,
                          ymin = 0, ymax = 4, crs = "EPSG:3857")
  terra::values(template) <- as.numeric(1:16)
  xy <- cbind(c(0.5, 2.5, 3.5), c(0.5, 1.5, 3.5))
  points <- terra::vect(xy, crs = "EPSG:3857",
                        atts = data.frame(cases = c(2, 5, 1),
                                          label = c("a", "b", "c")))
  areas <- terra::vect(c("POLYGON ((0 0, 2 0, 2 4, 0 4, 0 0))",
                         "POLYGON ((2 0, 4 0, 4 4, 2 4, 2 0))"),
                       crs = "EPSG:3857")
  areas$cases <- c(10, 30)
  list(template = template, points = points, areas = areas,
       cells = terra::cellFromXY(template, xy))
}

# Values on the 16 template cells: `vals` at `cells`, NA elsewhere.
on_cells <- function(cells, vals) {
  out <- rep(NA_real_, 16)
  out[cells] <- vals
  out
}

test_that("transform_pmf divides a raster by its total", {
  x <- toy_rast(c(NA, 10, 30, 0, 60, 0), 2, 3)
  pmf <- transform_pmf(x)

  expect_true(terra::compareGeom(pmf, x))
  expect_equal(terra::values(pmf)[, 1], c(NA, 0.1, 0.3, 0, 0.6, 0))
  expect_equal(terra::values(transform_pmf(x, snap = TRUE)), terra::values(pmf))
  with_total <- transform_pmf(x, return_total = TRUE)
  expect_named(with_total, c("pmf", "total"))
  expect_equal(with_total$total, 100)
  expect_equal(terra::values(with_total$pmf), terra::values(pmf))

  # Each layer of a multi-layer raster is a separate PMF.
  two <- toy_rast(c(1, 3, 2, 2), 2, 1, nlyr = 2)
  both <- transform_pmf(two, return_total = TRUE)
  expect_equal(unname(terra::values(both$pmf)), cbind(c(0.25, 0.75), c(0.5, 0.5)))
  expect_equal(both$total, c(4, 4))
  expect_error(transform_pmf(toy_rast(c(1, 3, 0, 0), 2, 1, nlyr = 2)),
               "Sum of all values")
})

test_that("transform_pmf rejects inputs that cannot form a PMF", {
  expect_error(transform_pmf(c(1, 2, 3)), "x must be one of")
  expect_error(transform_pmf(toy_rast(c(-1, 10, 20, 30), 2, 2)),
               "values in x must be >= 0")
  expect_error(transform_pmf(toy_rast(-1, 2, 2)), "values in x must be >= 0")
  expect_error(transform_pmf(toy_rast(NA_real_, 2, 2)), "contains only NA values")
  expect_error(transform_pmf(toy_rast(0, 2, 2)), "Sum of all values in 'x' is zero")
  # snap skips validation, but a zero total is still refused
  expect_error(transform_pmf(toy_rast(0, 2, 2), snap = TRUE),
               "Sum of all values in 'x' is zero")
})

test_that("transform_likelihood scales a raster by a reference value", {
  v <- c(10, 20, 30, 40)
  x <- toy_rast(v, 2, 2)
  likelihood <- function(...) terra::values(transform_likelihood(x, ...))[, 1]

  expect_equal(likelihood(), v / 25)
  expect_equal(likelihood(method = "sum"), v / 100)
  expect_equal(likelihood(method = "custom", ref_value = 8), v / 8)
  expect_equal(likelihood(standardize = TRUE), (v - mean(v)) / sd(v))
  expect_equal(likelihood(template = x, snap = TRUE), v / 25)

  expect_error(transform_likelihood(v), "either SpatRaster or SpatVector")
  expect_error(transform_likelihood(x, method = "median"), "Invalid method")
  expect_error(transform_likelihood(x, method = "custom"),
               "ref_value must be provided")
  expect_error(transform_likelihood(x, template = v), "Template must be a SpatRaster")
  expect_error(transform_likelihood(x, template = toy_rast(1, 3, 3)),
               "same resolution")
  shifted <- terra::rast(nrows = 2, ncols = 2, xmin = 0, xmax = 360,
                         ymin = -90, ymax = 90)
  expect_error(transform_likelihood(x, template = shifted), "same extent")
})

test_that("vector input is rasterised onto the template", {
  skip_if_no_proj()
  f <- pmf_vector_inputs()

  expect_equal(
    terra::values(transform_pmf(f$points, value_col = "cases",
                                template = f$template))[, 1],
    on_cells(f$cells, c(2, 5, 1) / 8)
  )
  expect_equal(
    terra::values(transform_likelihood(f$points, value_col = "cases",
                                       template = f$template))[, 1],
    on_cells(f$cells, c(2, 5, 1) / mean(c(2, 5, 1)))
  )

  # A single point or collinear points have a zero-area extent.
  one <- terra::vect(cbind(1.5, 2.5), atts = data.frame(cases = 3),
                     crs = "EPSG:3857")
  expect_equal(sum(terra::values(transform_pmf(one, "cases", f$template)),
                   na.rm = TRUE), 1)
  line <- terra::vect(cbind(c(0.5, 3.5), c(2.5, 2.5)),
                      atts = data.frame(cases = c(1, 3)), crs = "EPSG:3857")
  expect_equal(sort(terra::values(transform_pmf(line, "cases", f$template))),
               c(0.25, 0.75))

  # Points in the same cell add up instead of keeping only the last one.
  shared <- terra::vect(cbind(c(0.2, 0.8, 2.5), c(0.5, 0.5, 1.5)),
                        atts = data.frame(cases = c(2, 5, 1)), crs = "EPSG:3857")
  cells <- f$cells[1:2]
  expect_equal(terra::values(transform_pmf(shared, "cases", f$template))[, 1],
               on_cells(cells, c(7, 1) / 8))
  expect_equal(terra::values(transform_likelihood(shared, "cases",
                                                  template = f$template,
                                                  method = "sum"))[, 1],
               on_cells(cells, c(7, 1) / 8))

  # A polygon's value is copied to every cell it covers (8 cells each).
  expect_equal(terra::values(transform_pmf(f$areas, "cases", f$template))[, 1],
               rep(rep(c(10, 30), each = 2), 4) / 320)
  expect_equal(terra::values(transform_likelihood(f$areas, "cases",
                                                  template = f$template))[, 1],
               rep(rep(c(10, 30), each = 2), 4) / 20)
})

test_that("vector input needs a numeric column and an overlapping template", {
  skip_if_no_proj()
  f <- pmf_vector_inputs()
  far <- terra::vect(cbind(100, 100), atts = data.frame(cases = 1),
                     crs = "EPSG:3857")
  lonlat <- terra::rast(nrows = 4, ncols = 4, xmin = 0, xmax = 4,
                        ymin = 0, ymax = 4, crs = "EPSG:4326")

  expect_error(transform_pmf(f$points, "label", f$template),
               "must contain numeric values")
  expect_error(transform_pmf(f$points, "missing", f$template), "not found")
  expect_error(transform_pmf(f$points, template = f$template),
               "value_col must be provided")
  expect_error(transform_pmf(f$points, "cases", template = 1),
               "Template must be a SpatRaster")
  expect_error(transform_pmf(f$points, "cases", lonlat), "same CRS")
  expect_error(transform_pmf(far, "cases", f$template),
               "must intersect template extent")
  expect_error(transform_likelihood(f$points, "label", f$template),
               "must contain numeric values")
  projected <- toy_rast(1, 2, 2)
  terra::crs(projected) <- "EPSG:3857"
  expect_error(transform_likelihood(toy_rast(1, 2, 2), template = projected),
               "same CRS")
})

test_that("update_pmf multiplies the prior by the likelihood", {
  prior <- transform_pmf(toy_rast(c(1, 2, 3, 4), 2, 2))
  likelihood <- toy_rast(c(2, 1, 1, 0.5), 2, 2)
  product <- c(0.2, 0.2, 0.3, 0.2)
  posterior <- function(...) terra::values(update_pmf(prior, likelihood, ...))[, 1]

  expect_equal(posterior(), product / sum(product))
  expect_equal(posterior(snap = TRUE), product / sum(product))
  expect_equal(posterior(normalize = FALSE), product)
  # The density is the posterior PMF scaled by the prior-weighted likelihood,
  # with or without normalizing first.
  expect_equal(posterior(density = TRUE), product)
  expect_equal(posterior(normalize = FALSE, density = TRUE), product)
  # A multi-layer likelihood gives one normalized posterior per layer.
  layered <- c(likelihood, toy_rast(c(2, 2, 3, 2), 2, 2))
  product2 <- c(0.2, 0.4, 0.9, 0.8)
  expect_equal(unname(terra::values(update_pmf(prior, layered))),
               cbind(product / sum(product), product2 / sum(product2)))

  expect_error(update_pmf(c(0.5, 0.5), likelihood), "must be a SpatRaster")
  expect_error(update_pmf(prior, toy_rast(1, 3, 3)), "same resolution")
  expect_error(update_pmf(prior * 2, likelihood), "PMF must sum to 1")
  expect_error(update_pmf(toy_rast(c(-0.2, 0.4, 0.4, 0.4), 2, 2), likelihood),
               "PMF cannot contain negative values")
})

test_that("rasterize_demand updates a prior with observed counts", {
  skip_if_no_proj()
  f <- pmf_vector_inputs()
  prior <- transform_pmf(f$template)
  prior_at <- f$cells / 136
  product <- prior_at * c(2, 5, 1) / mean(c(2, 5, 1))

  expect_equal(terra::values(rasterize_demand(f$points, "cases", prior = prior))[, 1],
               on_cells(f$cells, product / sum(product)))
  expect_equal(terra::values(rasterize_demand(f$points, "cases", prior = prior,
                                              density = TRUE))[, 1],
               on_cells(f$cells, product))
  # Without a prior the prior is uniform, so the posterior follows the
  # likelihood alone.
  from_template <- rasterize_demand(f$points, "cases", template = f$template)
  expect_equal(terra::values(from_template)[, 1],
               on_cells(f$cells, c(2, 5, 1) / 8))
  # Cells missing in the template carry no prior mass.
  masked <- f$template
  masked[f$cells[3]] <- NA
  expect_equal(terra::values(rasterize_demand(f$points, "cases",
                                              template = masked))[, 1],
               on_cells(f$cells[1:2], c(2, 5) / 7))

  expect_error(rasterize_demand(f$points, "cases"), "Either prior or template")
  for (standardize in list(TRUE, 1)) {
    expect_error(rasterize_demand(f$points, "cases", template = f$template,
                                  standardize = standardize), "negative likelihood")
  }
  empty <- f$template
  terra::values(empty) <- NA_real_
  expect_error(rasterize_demand(f$points, "cases", template = empty),
               "no non-missing cells")
  expect_error(rasterize_demand(f$points, "missing", prior = prior), "not found")
  expect_error(rasterize_demand(f$points, "cases", prior = f$template),
               "PMF must sum to 1")
})

test_that("transform_demand spreads area counts within each area by the PMF", {
  skip_if_no_proj()
  f <- pmf_vector_inputs()
  pmf <- transform_pmf(f$template)
  v <- as.numeric(1:16)
  west <- c(1, 2, 5, 6, 9, 10, 13, 14)
  density <- ifelse(seq_len(16) %in% west, v * 10 / sum(v[west]),
                    v * 30 / sum(v[-west]))

  surface <- transform_demand(f$areas, value_col = "cases", pmf = pmf,
                              density = TRUE)
  expect_equal(terra::values(surface)[, 1], density, tolerance = 1e-6)
  expect_equal(terra::values(transform_demand(f$areas, "cases", pmf = pmf))[, 1],
               density / 40, tolerance = 1e-6)

  out <- transform_demand(f$areas, value_col = "cases", pmf = pmf, density = TRUE,
                          full_output = TRUE)
  expect_named(out, c("surface", "totals"))
  expect_equal(out$totals, data.frame(id = 1:2, original = c(10, 30),
                                      redistributed = c(10, 30)),
               tolerance = 1e-6)

  # Without a PMF, counts are spread uniformly over the template cells.
  expect_message(
    uniform <- transform_demand(f$areas, "cases", template = f$template,
                                density = TRUE),
    "No PMF provided"
  )
  expect_equal(terra::values(uniform)[, 1],
               ifelse(seq_len(16) %in% west, 10 / 8, 30 / 8), tolerance = 1e-6)
})

test_that("transform_demand validates counts, PMF and template", {
  skip_if_no_proj()
  f <- pmf_vector_inputs()
  pmf <- transform_pmf(f$template)
  areas <- f$areas
  areas$label <- c("a", "b")
  areas$negative <- c(-1, 30)

  expect_error(transform_demand(f$template, "cases", pmf = pmf),
               "must be a SpatVector")
  expect_error(transform_demand(areas, "missing", pmf = pmf), "not found")
  expect_error(transform_demand(areas, "label", pmf = pmf), "must be numeric")
  expect_error(transform_demand(areas, "negative", pmf = pmf), "cannot be negative")
  expect_error(transform_demand(areas, "cases", pmf = 1), "PMF must be a SpatRaster")
  expect_error(transform_demand(areas, "cases", pmf = f$template),
               "PMF values must sum to 1")
  expect_error(transform_demand(areas, "cases", template = 1),
               "Template must be a SpatRaster")
  expect_error(transform_demand(areas, "cases"), "Either PMF or template")

  tiny <- terra::vect(c("POLYGON ((0 0, 2 0, 2 4, 0 4, 0 0))",
                        "POLYGON ((2.1 0.1, 2.2 0.1, 2.2 0.2, 2.1 0.2, 2.1 0.1))"),
                      crs = "EPSG:3857")
  tiny$cases <- c(10, 30)
  expect_warning(transform_demand(tiny, "cases", pmf = pmf), "cover no grid cell")
})
