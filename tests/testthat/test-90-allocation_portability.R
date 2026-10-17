# Saved allocation objects in a fresh R process: prediction, maps and coverage
# need no live terra pointer, source raster or session state. The process
# loads the package the same way as this session: from source under
# devtools/testthat, or the installed package under R CMD check. No other
# test predicts from a saved snapshot or rebuilds these rasters from saved
# plain geometry in a fresh process, so this test is not skipped on CRAN.

test_that("a fresh process predicts, maps and rebuilds coverage from saved objects", {
  outputs <- c("rho", "A", "abar", "flow")
  requests <- list()
  for (entry in alloc_prediction_sources()) for (source in entry[c("solved", "fit")]) {
    request <- entry$case$distance[c(3, 1), ]
    rownames(request) <- c("newA", "newB")
    requests[[length(requests) + 1L]] <- list(source = source, request = request)
  }
  # A declared kernel keeps its code in the snapshot as plain text: the fresh
  # process rebuilds and runs it, without the function declared here.
  plateau <- decay_kernel(
    function(distance, sigma) {
      # Full weight within 1.5 minutes, Gaussian decay beyond.
      ifelse(distance <= 1.5, 1, exp(-(distance - 1.5)^2 / (2 * sigma^2)))
    },
    name = "plateau", parameter = "scale"
  )
  case <- alloc_prediction_case()
  declared <- evaluate_allocation(clm_allocation(case$model$substrate, family = plateau,
    kappa = .65, beta = 1.3, v0 = 4), c(sigma = 2))
  request <- case$distance[c(3, 1), ]
  rownames(request) <- c("newA", "newB")
  requests[[length(requests) + 1L]] <- list(source = declared, request = request)
  expected <- lapply(requests, function(r) {
    predict_allocation(r$source, r$request, outputs = outputs)
  })
  template <- terra::rast(nrows = 2, ncols = 4, xmin = 20, xmax = 24, ymin = 0, ymax = 2)
  entry <- alloc_prediction_sources()[["gaussian fixed"]]
  mapped <- predict_allocation(entry$solved, entry$case$distance,
    spatial = list(template = template, cell_index = c(o3 = 7, o1 = 2, o7 = 5)))
  f <- alloc_small_case()
  prepared <- prepare_allocation(f$demand, f$supply, f$travel, missing_demand = "exclude",
    spatial = list(template = template,
                   cell_index = c(o1 = 7, o2 = 3, o3 = 8, o4 = 4, o5 = 2)))
  coverage <- .problem_coverage(clm_allocation(prepared, v0 = 5), c(sigma = 2), norm = 3)
  restored <- run_in_fresh_r(
    list(requests = requests, outputs = outputs, mapped = mapped, prepared = prepared,
         coverage = coverage),
    c("geometry <- function(m) list(values = terra::values(m),",
      "  extent = as.vector(terra::ext(m)), crs = terra::crs(m))",
      "rebuilt <- spax:::.problem_coverage(spax::clm_allocation(x$prepared, v0 = 5),",
      "  c(sigma = 2), norm = 3)",
      "result <- list(predictions = lapply(x$requests, function(r)",
      "    spax::predict_allocation(r$source, r$request, outputs = x$outputs)),",
      "  map = geometry(spax:::.prediction_surface(x$mapped)),",
      "  coverage_map = geometry(spax:::.coverage_surface(x$coverage)),",
      "  cells = rebuilt$cells)"))
  expect_identical(restored$predictions, expected)
  expect_equal(restored$map$values, terra::values(.prediction_surface(mapped)))
  expect_equal(restored$coverage_map$values, terra::values(.coverage_surface(coverage)))
  for (map in restored[c("map", "coverage_map")]) {
    expect_equal(map$extent, as.vector(terra::ext(template)))
    expect_identical(map$crs, terra::crs(template))
  }
  expect_equal(restored$cells, coverage$cells)
})
