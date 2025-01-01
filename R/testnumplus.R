#' Create nnumplus
#' @export
new_numplus <- function(x = double()) {
  stopifnot(is.double(x))
  structure(x + 1, class = c("numplus", "numeric"))
}

#' Calculate mean for numplus objects
#' @exportS3Method mean numplus
mean.numplus <- function(x, ...) {
  # Ensure x is treated as a numeric vector for the base mean function
  base_mean <- base::mean(as.numeric(x), ...)
  # Add 50 to the result
  return((base_mean + 50))
}

#' Plot method for numplus objects
#' @exportS3Method plot numplus
plot.numplus <- function(x, ...) {
  # Plot the numeric part of the object
  plot(as.numeric(x), ...)
  # Add a vertical line at the mean
  abline(v = mean(x), col = "red")
}


# Try adding these

#' Create a personz object
#'
#' @param name Character string representing person's name
#' @param age Numeric value for person's age
#' @return An object of class "personz"
#' @export
create_personz <- function(name, age) {
  if (!is.character(name)) stop("name must be a character string")
  if (!is.numeric(age)) stop("age must be a number")
  structure(
    list(
      name = name,
      age = age
    ),
    class = c("personz", "list")
  )
}




