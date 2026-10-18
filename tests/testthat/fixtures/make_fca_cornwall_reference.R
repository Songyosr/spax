# Reference surfaces of the FCA family on the raw shipped Cornwall example.
#
# Made with the FIELD engine (the raster field layer that SPAX-055 retires) at
# commit 664b821 (branch feat/one-engine, task T1 of SPAX-055). The block
# engine must reproduce these numbers (tests/testthat/test-05-fca_identity.R).
# Do NOT regenerate this fixture after T4: the field engine is gone by then and
# the file is the only record of what it computed.
#
# Stored per case: `na`, the cell indices where the surface is missing, and
# `values`, the finite values (in cell order) rounded to 12 significant digits.
#
# Run from the package root with the field engine available:
#   Rscript tests/testthat/fixtures/make_fca_cornwall_reference.R

Sys.setenv(PROJ_LIB = system.file("proj", package = "sf"))
suppressMessages(pkgload::load_all(".", quiet = TRUE))
options(spax.fca_engine = "field")

pop <- read_spax_example("cornwall_pop.tif")
travel <- read_spax_example("cornwall_gp_travel.tif")
supply <- data.frame(id = cornwall_gp$practice, gp = cornwall_gp$gp_fte_extg)
gauss <- list(method = "gaussian", sigma = 10)

surfaces <- list(
  `2sfca_15` = spax_2sfca(pop, supply, travel, 15,
                          id_col = "id", supply_cols = "gp"),
  e2sfca_gaussian10_standard = spax_e2sfca(
    pop, supply, travel, gauss, demand_normalize = "standard",
    id_col = "id", supply_cols = "gp"),
  `3sfca_gaussian10` = spax_3sfca(pop, supply, travel, gauss,
                                  id_col = "id", supply_cols = "gp"),
  huff2sfca_gaussian10 = spax_huff2sfca(pop, supply, travel, gauss,
                                        id_col = "id", supply_cols = "gp")
)

reference <- lapply(surfaces, function(res) {
  v <- terra::values(res$accessibility, mat = TRUE)
  stopifnot(ncol(v) == 1)
  v <- v[, 1]
  list(na = which(is.na(v)), values = signif(v[!is.na(v)], 12))
})

path <- file.path("tests", "testthat", "fixtures", "fca_cornwall_reference.rds")
saveRDS(reference, path, compress = "xz")
cat("wrote", path, file.size(path), "bytes\n")
