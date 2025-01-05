#' Validate inputs for spax object creation
#'
#' @description
#' Checks if inputs are valid for creating a spax object.
#' Called before object creation to ensure valid inputs.
#'
#' @param accessibility SpatRaster containing accessibility scores
#' @param type Character string specifying model type
#' @param parameters List of model parameters
#' @param facilities data.frame containing facility-level results (optional)
#' @param iterations List containing iteration info (optional)
#' @param variations List containing variation results (optional)
#' @param call The original function call
#' @return Invisible TRUE if validation passes
#' @keywords internal
.chck_spax <- function(accessibility = NULL,
                       type = NULL,
                       parameters = list(),
                       facilities = NULL,
                       iterations = NULL,
                       variations = NULL,
                       call = NULL) {
  # Core requirements
  if (is.null(accessibility)) {
    stop("accessibility results required")
  }
  if (is.null(type)) {
    stop("model type must be specified")
  }

  # Type validation
  .assert_class(accessibility, "SpatRaster", "accessibility")
  .assert_class(type, "character", "type")
  .assert_length(length(type), 1, "type")

  # Parameters validation
  .assert_class(parameters, "list", "parameters")

  # Optional components validation (only if provided)
  if (!is.null(facilities)) {
    .assert_class(facilities, "data.frame", "facilities")
  }

  if (!is.null(iterations)) {
    .assert_class(iterations, "list", "iterations")
  }

  if (!is.null(variations)) {
    .assert_class(variations, "list", "variations")
  }

  invisible(TRUE)
}

#' Constructor for spax class
#'
#' @description
#' Creates a new spax object for storing spatial accessibility analysis results.
#' This is an internal constructor and should be used with .chck_spax().
#'
#' @inheritParams .chck_spax
#' @return A spax object
#' @keywords internal
.new_spax <- function(accessibility = NULL,
                     type = NULL,
                     parameters = list(),
                     facilities = NULL,
                     iterations = NULL,
                     variations = NULL,
                     call = NULL) {
  # Return structured object
  structure(
    list(
      accessibility = accessibility,
      type = type,
      parameters = parameters,
      facilities = facilities,
      iterations = iterations,
      variations = variations,
      call = call
    ),
    class = "spax"
  )
}

#' Create a spax object with validation
#'
#' @description
#' Main function for creating spax objects, performing validation first.
#' For internal use by model functions.
#'
#' @inheritParams .chck_spax
#' @param snap Logical; if TRUE skip validation (default = FALSE)
#' @return A spax object
#' @keywords internal
.create_spax <- function(accessibility = NULL,
                         type = NULL,
                         parameters = list(),
                         facilities = NULL,
                         iterations = NULL,
                         variations = NULL,
                         call = NULL,
                         snap = FALSE) {
  # Validate inputs first (unless in snap mode)
  if (!snap) {
    .chck_spax(
      accessibility = accessibility,
      type = type,
      parameters = parameters,
      facilities = facilities,
      iterations = iterations,
      variations = variations,
      call = call
    )
  }

  # Create and return object
  .new_spax(
    accessibility = accessibility,
    type = type,
    parameters = parameters,
    facilities = facilities,
    iterations = iterations,
    variations = variations,
    call = call
  )
}
