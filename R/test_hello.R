#' @export
new_numplus <- function(x = double()) {
  stopifnot(is.double(x))
  structure(x+1, class = c("numplus","numeric"))
}

#' @export
mean.numplus <- function(x, ...) {
  base_mean <- base::mean(as.numeric(x), ...)
  return(base_mean+52)
}
