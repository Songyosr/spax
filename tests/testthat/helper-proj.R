# Tests that set a CRS need the PROJ database. Use it when terra finds it (a
# system PROJ, as on Linux); otherwise, when PROJ_LIB is unset, point it at the
# copy bundled with sf. The variable stays set for the rest of the session.
proj_available <- function() {
  works <- function() {
    isTRUE(tryCatch(nzchar(terra::crs("EPSG:4326")),
                    error = function(e) FALSE, warning = function(w) FALSE))
  }
  if (works()) return(TRUE)
  if (!nzchar(Sys.getenv("PROJ_LIB"))) {
    proj <- system.file("proj", package = "sf")
    if (nzchar(proj) && file.exists(file.path(proj, "proj.db"))) {
      Sys.setenv(PROJ_LIB = proj)
    }
  }
  works()
}

skip_if_no_proj <- function() {
  testthat::skip_if_not(proj_available(), "PROJ database unavailable")
}
