#' @export
print.spax <- function(x, ...) {
  cat("\nSpatial Accessibility Analysis Results\n")
  cat("-------------------------------------\n")
  cat("Model:", x$model, "\n")
  cat("Accessibility Measures:", paste(names(x$accessibility), collapse = ", "), "\n")
  invisible(x)
}
