# Regenerate vignettes/allocation-bootstrap.rds, the stored result of the
# 399-attempt bootstrap_allocation() call in vignettes/allocation-workflow.Rmd.
#
# The vignette is the single source of the inputs, seed (20260930) and call.
# With SPAX_REBUILD_VIGNETTE_BOOTSTRAP=true its `paired-bootstrap` chunk runs
# live (about three minutes) and the following chunk saves the unmodified
# result next to the vignette. Run from the package root after installing the
# current source (the vignette's last chunk uses the installed package):
#
#   R CMD INSTALL .
#   Rscript data-raw/precompute-vignette-bootstrap.R
#
# Rerun whenever bootstrap_allocation(), the fitting code, the vignette's
# generated inputs or the package version change, then commit the .rds file.
stopifnot(file.exists("DESCRIPTION"), file.exists("vignettes/allocation-workflow.Rmd"))
Sys.setenv(SPAX_REBUILD_VIGNETTE_BOOTSTRAP = "true")
out <- tempfile("allocation-workflow-")
dir.create(out)
elapsed <- system.time(rmarkdown::render("vignettes/allocation-workflow.Rmd",
  output_dir = out, intermediates_dir = out, quiet = TRUE, envir = new.env()))
boot <- readRDS("vignettes/allocation-bootstrap.rds")
stopifnot(boot$planned == 399L, boot$attempted == 399L)
cat(sprintf("Saved %d/%d attempts (%d successful, status %s) in %.1f s; file %.1f kB\n",
  boot$attempted, boot$planned, boot$successful, boot$status, elapsed[["elapsed"]],
  file.size("vignettes/allocation-bootstrap.rds") / 1024))
