#' Create nnumplus
#' @export
new_numplus <- function(x = double()) {
  stopifnot(is.double(x))
  structure(x + 1, class = c("numplus", "numeric"))
}

#' Calculate mean for numplus objects
#' @export
#' @method mean numplus
mean.numplus <- function(x, ...) {
  # Ensure x is treated as a numeric vector for the base mean function
  base_mean <- base::mean(as.numeric(x), ...)
  # Add 50 to the result
  return(base_mean + 50)
}

