# Regenerate vignettes/allocation-bootstrap.rds, the stored result of the
# 399-attempt bootstrap_fit() call in vignettes/facility-bootstrap.Rmd.
#
# The vignette is the single source of the inputs, seed (20260930) and call.
# With SPAX_REBUILD_VIGNETTE_BOOTSTRAP=true its `paired-bootstrap` chunk runs
# live (about a minute) and the following chunk saves the unmodified result next
# to the vignette. Run from the package root after installing the current source
# (the vignette loads the installed package), for example into a library of its
# own:
#
#   R CMD INSTALL --library=<lib> .
#   R_LIBS=<lib> Rscript data-raw/precompute-vignette-bootstrap.R
#
# Rerun whenever bootstrap_fit(), the fitting code, the vignette's generated
# bootstrap inputs or the package version change, then commit the .rds file.
stopifnot(file.exists("DESCRIPTION"), file.exists("vignettes/facility-bootstrap.Rmd"))
Sys.setenv(SPAX_REBUILD_VIGNETTE_BOOTSTRAP = "true")
out <- tempfile("facility-bootstrap-")
dir.create(out)
elapsed <- system.time(rmarkdown::render("vignettes/facility-bootstrap.Rmd",
  output_dir = out, intermediates_dir = out, quiet = TRUE, envir = new.env()))
boot <- readRDS("vignettes/allocation-bootstrap.rds")
stopifnot(boot$planned == 399L, boot$attempted == 399L)
cat(sprintf("Saved %d/%d attempts (%d successful, status %s) in %.1f s; file %.1f kB\n",
  boot$attempted, boot$planned, boot$successful, boot$status, elapsed[["elapsed"]],
  file.size("vignettes/allocation-bootstrap.rds") / 1024))
