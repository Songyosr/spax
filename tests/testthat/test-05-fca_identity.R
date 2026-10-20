# Identity suite of the block FCA engine (SPAX-055 T2).
#
# (a) The block engine against the independent base-R fca_oracle() on the full
#     Cornwall matrices.
# (b) The block engine against surfaces computed with the former field engine
#     at commit 664b821 and stored in fixtures/fca_cornwall_reference.rds. The
#     field engine was removed in T4; the file is the only record of what it
#     computed and is not regenerated.

# --- helpers -----------------------------------------------------------------

fca_values <- function(x) {
  unname(terra::values(if (inherits(x, "SpatRaster")) x else x$accessibility,
                       mat = TRUE))
}

# --- data --------------------------------------------------------------------

# The raw shipped Cornwall example: 132 x 271 cells, 55 practices.
cornwall_raw <- function() {
  pop <- read_spax_example("cornwall_pop.tif")
  travel <- read_spax_example("cornwall_gp_travel.tif")
  supply <- sf::st_drop_geometry(cornwall_gp)
  # Keep the practices that have a travel layer (all of them in the shipped
  # example), in the order of the layers.
  supply <- supply[supply$practice %in% names(travel), ]
  list(
    label = "cornwall", demand = pop, distance = travel, supply = supply,
    id_col = "practice", supply_col = "gp_fte_extg",
    supply_cols2 = c("gp_fte_extg", "nurse_fte"),
    sigma = 10, threshold = 15, selection_sigma = 6, plain_threshold = 20,
    inverse_c = 2, attractiveness = seq(1, 3, length.out = terra::nlyr(travel))
  )
}

# --- tests -------------------------------------------------------------------

test_that("the block engine equals the independent base-R oracle on every Cornwall cell", {
  d <- cornwall_raw()
  distance <- terra::values(d$distance, mat = TRUE)
  D <- terra::values(d$demand, mat = TRUE)[, 1]
  S <- d$supply$gp_fte_extg[match(names(d$distance), d$supply$practice)]
  W <- exp(-distance^2 / (2 * d$sigma^2))
  expect_equal(nrow(W), terra::ncell(d$demand))
  for (norm in c("identity", "standard")) {
    got <- spax_e2sfca(d$demand, d$supply, d$distance,
                       list(method = "gaussian", sigma = d$sigma),
                       demand_normalize = norm, id_col = d$id_col,
                       supply_cols = d$supply_col)
    expected <- fca_oracle(D, W, W, S, norm)
    expect_identical(is.na(fca_values(got)), is.na(expected), info = norm)
    expect_equal(fca_values(got), expected, tolerance = 1e-12, info = norm)
  }
})

test_that("the block engine reproduces the former field-engine reference surfaces of Cornwall", {
  ref <- readRDS(test_path("fixtures", "fca_cornwall_reference.rds"))
  expect_named(ref, c("2sfca_15", "e2sfca_gaussian10_standard", "3sfca_gaussian10",
                      "huff2sfca_gaussian10"))
  d <- cornwall_raw()
  gauss <- list(method = "gaussian", sigma = 10)
  got <- list(
    `2sfca_15` = spax_2sfca(d$demand, d$supply, d$distance, 15,
                            id_col = d$id_col, supply_cols = d$supply_col),
    e2sfca_gaussian10_standard = spax_e2sfca(
      d$demand, d$supply, d$distance, gauss, demand_normalize = "standard",
      id_col = d$id_col, supply_cols = d$supply_col),
    `3sfca_gaussian10` = spax_3sfca(d$demand, d$supply, d$distance, gauss,
                                    id_col = d$id_col, supply_cols = d$supply_col),
    huff2sfca_gaussian10 = spax_huff2sfca(d$demand, d$supply, d$distance, gauss,
                                          id_col = d$id_col, supply_cols = d$supply_col)
  )
  for (nm in names(ref)) {
    v <- fca_values(got[[nm]])[, 1]
    expect_identical(which(is.na(v)), ref[[nm]]$na, info = nm)
    finite <- v[!is.na(v)]
    expect_length(finite, length(ref[[nm]]$values))
    # Relative to the reference, cell by cell (the reference is rounded to 12
    # significant digits).
    expect_true(all(abs(finite - ref[[nm]]$values) <= 1e-11 * abs(ref[[nm]]$values)),
                info = nm)
  }
})
