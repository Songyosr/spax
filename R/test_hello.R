#' @export
new_numplus <- function(x = double()) {
  stopifnot(is.double(x))
  structure(x+1, class = "numplus")
}

#' @export
mean.numplus <- function(x, ...) {
  mean(x, ...)
}
