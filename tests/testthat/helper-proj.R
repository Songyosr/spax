# Tests that set a CRS need the PROJ database. When PROJ_LIB is unset, point it
# at the copy bundled with sf. The variable stays set for the rest of the
# session.
proj_available <- function() {
  if (!nzchar(Sys.getenv("PROJ_LIB"))) {
    proj <- system.file("proj", package = "sf")
    if (nzchar(proj) && file.exists(file.path(proj, "proj.db"))) {
      Sys.setenv(PROJ_LIB = proj)
    }
  }
  nzchar(Sys.getenv("PROJ_LIB")) &&
    file.exists(file.path(Sys.getenv("PROJ_LIB"), "proj.db"))
}

skip_if_no_proj <- function() {
  testthat::skip_if_not(proj_available(), "PROJ database unavailable")
}
