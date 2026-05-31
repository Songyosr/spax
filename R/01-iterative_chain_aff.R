#' Validate inputs for convergence assessment
#' @keywords internal
.chck_assess_convergence <- function(history, names = NULL) {
  if (!is.array(history) || length(dim(history)) != 3) {
    stop("history must be a 3-dimensional array [iterations, facilities, metrics]")
  }

  if (!is.null(names) && length(names) != dim(history)[2]) {
    stop("length of names must match number of facilities")
  }

  invisible(TRUE)
}

#' Assess convergence behavior of iFCA model
#'
#' @description
#' Analyzes the convergence characteristics of an iterative floating catchment
#' area model, including convergence speed, stability, and oscillation patterns
#' for each facility.
#'
#' @param history 3-dimensional array [iterations, facilities, metrics] containing
#'        model state history
#' @param names Optional character vector of facility names
#' @return List containing:
#'   \describe{
#'     \item{summary}{Data frame of facility-specific convergence metrics}
#'     \item{trajectories}{List of convergence trajectories for each metric}
#'     \item{stability}{List of stability measures and oscillation patterns}
#'   }
#' @examples
#' # Create test data
#' distance <- terra::rast(hos_iscr)
#' pop <- terra::rast(u5pd)
#'
#' # Run iFCA model with history tracking
#' model <- spax_ifca(
#'   distance_raster = distance,
#'   demand = pop,
#'   supply = hc12_hos$s_doc,
#'   sigma = 30
#' )
#'
#' # Assess convergence
#' convergence <- assess_convergence(model$history)
#'
#' # Examine results
#' print(convergence$summary)
#' @export
assess_convergence <- function(history, names = NULL) {
  # Validate inputs
  .chck_assess_convergence(history, names)

  # Extract dimensions
  n_iter <- dim(history)[1]
  n_facilities <- dim(history)[2]

  # Use provided names or generate defaults
  facility_names <- names %||% paste0("facility_", seq_len(n_facilities))

  # Extract metric trajectories
  utilization <- history[, , 1]  # [iter, facility]
  ratios <- history[, , 2]
  attractiveness <- history[, , 3]

  # Calculate convergence metrics
  convergence_iter <- apply(utilization, 2, function(x) {
    final_val <- x[length(x)]
    target <- 0.9 * final_val
    which.min(abs(x - target))
  })

  # Calculate stability metrics
  changes <- apply(utilization, 2, diff)
  oscillation_index <- apply(changes, 2, function(x) {
    sum(abs(diff(sign(x)))) / (length(x) - 1)
  })

  # Prepare output
  list(
    summary = data.frame(
      facility = facility_names,
      convergence_iterations = convergence_iter,
      oscillation_index = oscillation_index,
      final_utilization = utilization[n_iter, ],
      final_ratio = ratios[n_iter, ],
      row.names = NULL
    ),
    trajectories = list(
      utilization = utilization,
      ratios = ratios,
      attractiveness = attractiveness
    ),
    stability = list(
      changes = changes,
      oscillation = oscillation_index
    )
  )
}
#' Validate inputs for accuracy assessment
#' @param predicted Numeric vector of predicted values
#' @param observed Numeric vector of observed values
#' @param names Optional character vector of facility names
#' @return Invisible TRUE if valid
#' @keywords internal
.chck_assess_accuracy <- function(predicted, observed, names = NULL) {
  if (!is.numeric(predicted) || !is.numeric(observed)) {
    stop("predicted and observed values must be numeric vectors")
  }

  if (length(predicted) != length(observed)) {
    stop("predicted and observed values must have the same length")
  }

  if (!is.null(names) && length(names) != length(predicted)) {
    stop("length of names must match length of predicted/observed values")
  }

  if (any(observed <= 0, na.rm = TRUE)) {
    stop("observed values must be positive for percentage error calculations")
  }

  invisible(TRUE)
}

#' Assess prediction accuracy of iFCA model
#'
#' @description
#' Evaluates the accuracy of an iterative floating catchment area model by
#' comparing predicted utilization against observed values. Provides both
#' overall accuracy metrics and facility-specific error analysis.
#'
#' @param predicted Numeric vector of model-predicted utilization values
#' @param observed Numeric vector of observed utilization values
#' @param names Optional character vector of facility names
#' @return List containing:
#'   \describe{
#'     \item{summary}{Data frame of facility-specific prediction errors}
#'     \item{metrics}{List of overall accuracy metrics (MAE, RMSE, MAPE)}
#'     \item{errors}{Detailed error components for further analysis}
#'   }
#' @examples
#' # Create simulated observed data
#' observed_values <- hc12_hos$d_pop_moph_05 * 0.1  # 10% visit rate
#'
#' # Run iFCA model
#' distance <- terra::rast(hos_iscr)
#' pop <- terra::rast(u5pd)
#' model <- spax_ifca(
#'   distance_raster = distance,
#'   demand = pop,
#'   supply = hc12_hos$s_doc,
#'   sigma = 30
#' )
#'
#' # Compare with observed data
#' accuracy <- assess_accuracy(
#'   predicted = model$utilization,
#'   observed = observed_values
#' )
#'
#' # Examine results
#' print(accuracy$metrics)
#' @export
assess_accuracy <- function(predicted, observed, names = NULL) {
  # Validate inputs
  .chck_assess_accuracy(predicted, observed, names)

  # Use provided names or generate defaults
  facility_names <- names %||% paste0("facility_", seq_along(predicted))

  # Calculate error components
  errors <- predicted - observed
  abs_errors <- abs(errors)
  pct_errors <- (errors / observed) * 100

  # Calculate overall metrics
  mae <- mean(abs_errors, na.rm = TRUE)
  rmse <- sqrt(mean(errors^2, na.rm = TRUE))
  mape <- mean(abs(pct_errors), na.rm = TRUE)

  # Prepare output
  list(
    summary = data.frame(
      facility = facility_names,
      predicted = predicted,
      observed = observed,
      error = errors,
      pct_error = pct_errors,
      row.names = NULL
    ),
    metrics = list(
      mae = mae,
      rmse = rmse,
      mape = mape
    ),
    errors = list(
      raw = errors,
      absolute = abs_errors,
      percentage = pct_errors
    )
  )
}

#' Comprehensive assessment of iFCA model results
#'
#' @description
#' Provides a complete diagnostic analysis of an iterative floating catchment
#' area model, including both convergence behavior and prediction accuracy
#' (when observed values are available).
#'
#' @param model Result from spax_ifca() containing model history and predictions
#' @param observed Optional numeric vector of observed utilization values
#' @param names Optional character vector of facility names
#' @return List of class "ifca_assessment" containing:
#'   \describe{
#'     \item{convergence}{Results from assess_convergence()}
#'     \item{accuracy}{Results from assess_accuracy() if observed values provided}
#'     \item{parameters}{Original model parameters}
#'   }
#' @examples
#' # Create test data
#' distance <- terra::rast(hos_iscr)
#' pop <- terra::rast(u5pd)
#' observed_values <- hc12_hos$d_pop_moph_05 * 0.1  # 10% visit rate
#'
#' # Run iFCA model
#' model <- spax_ifca(
#'   distance_raster = distance,
#'   demand = pop,
#'   supply = hc12_hos$s_doc,
#'   sigma = 30
#' )
#'
#' # Basic assessment (convergence only)
#' assessment <- assess_ifca(model)
#'
#' # Full assessment with observed data
#' assessment <- assess_ifca(
#'   model = model,
#'   observed = observed_values,
#'   names = hc12_hos$id
#' )
#'
#' # Examine results
#' print(assessment)
#' @export
assess_ifca <- function(model, observed = NULL, names = NULL) {
  # Validate model input
  if (!is.list(model) || is.null(model$history)) {
    stop("model must be a full spax_ifca result (not from snap mode)")
  }

  # Analyze convergence
  convergence_results <- assess_convergence(model$history, names)

  # Analyze accuracy if observed values provided
  accuracy_results <- if (!is.null(observed)) {
    assess_accuracy(model$utilization, observed, names)
  } else {
    NULL
  }

  # Create assessment object
  structure(
    list(
      convergence = convergence_results,
      accuracy = accuracy_results,
      parameters = model$parameters
    ),
    class = "ifca_assessment"
  )
}
