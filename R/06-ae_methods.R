# Private AE object methods --------------------------------------------------

#' Print an AE fixed-point solution
#' @method print ae_equilibrium
#' @keywords internal
#' @export
print.ae_equilibrium <- function(x, digits = 4, ...) {
  cat("<ae_equilibrium>\n")
  cat("converged: ", isTRUE(x$converged), "\n", sep = "")
  cat("iterations: ", x$iterations, "\n", sep = "")
  cat("error: ", .ae_format_number(x$error, digits = digits), "\n", sep = "")
  cat("residual_norm: ",
      .ae_format_number(x$residual_norm, digits = digits), "\n", sep = "")
  if (!is.null(x$state_history)) {
    cat("state_history: ", nrow(x$state_history), " x ",
        ncol(x$state_history), "\n", sep = "")
  }
  invisible(x)
}

#' Print an AE problem calibration fit
#' @method print ae_problem_nfxp_fit
#' @keywords internal
#' @export
print.ae_problem_nfxp_fit <- function(x, digits = 4, ...) {
  cat("<ae_problem_nfxp_fit>\n")
  cat("model: ", x$model, "\n", sep = "")
  cat("family: ", x$family, "\n", sep = "")
  cat("theta:\n")
  theta <- x$theta
  if (is.null(names(theta))) {
    names(theta) <- paste0("theta", seq_along(theta))
  }
  for (nm in names(theta)) {
    cat("  ", nm, ": ", .ae_format_number(theta[[nm]], digits = digits),
        "\n", sep = "")
  }
  cat("loss: ", .ae_format_number(x$loss, digits = digits), "\n", sep = "")
  cat("optim_convergence: ", x$convergence, "\n", sep = "")
  cat("seconds: ", .ae_format_number(x$seconds, digits = digits), "\n", sep = "")
  cat("spectral_radius: ",
      .ae_format_number(x$spectral_radius, digits = digits), "\n", sep = "")
  cat("gradient: ", isTRUE(x$gradient), "\n", sep = "")
  invisible(x)
}

#' Summarize an AE problem calibration fit
#' @method summary ae_problem_nfxp_fit
#' @keywords internal
#' @exportS3Method base::summary
summary.ae_problem_nfxp_fit <- function(object, ...) {
  .chck_class(object, "ae_problem_nfxp_fit", "object")
  theta <- object$theta
  if (is.null(names(theta))) {
    names(theta) <- paste0("theta", seq_along(theta))
  }
  theta_table <- data.frame(
    parameter = names(theta),
    estimate = as.numeric(theta),
    stringsAsFactors = FALSE
  )

  residual_summary <- NULL
  if (!is.null(object$observed) && length(object$observed) == length(object$predicted)) {
    residual_summary <- .ae_numeric_summary(
      as.numeric(object$predicted) - as.numeric(object$observed)
    )
  }

  structure(
    list(
      model = object$model,
      family = object$family,
      theta = theta_table,
      fit = data.frame(
        loss = object$loss,
        convergence = object$convergence,
        seconds = object$seconds,
        spectral_radius = object$spectral_radius,
        gradient = isTRUE(object$gradient),
        stringsAsFactors = FALSE
      ),
      predicted = .ae_numeric_summary(object$predicted),
      state = .ae_numeric_summary(object$state),
      residual = residual_summary
    ),
    class = "summary.ae_problem_nfxp_fit"
  )
}

#' Print an AE problem calibration summary
#' @method print summary.ae_problem_nfxp_fit
#' @keywords internal
#' @export
print.summary.ae_problem_nfxp_fit <- function(x, digits = 4, ...) {
  cat("<summary.ae_problem_nfxp_fit>\n")
  cat("model: ", x$model, "\n", sep = "")
  cat("family: ", x$family, "\n\n", sep = "")
  cat("theta:\n")
  print.data.frame(.ae_round_data_frame(x$theta, digits = digits),
                   row.names = FALSE)
  cat("\nfit:\n")
  print.data.frame(.ae_round_data_frame(x$fit, digits = digits),
                   row.names = FALSE)
  cat("\npredicted:\n")
  print.data.frame(.ae_round_data_frame(x$predicted, digits = digits),
                   row.names = FALSE)
  cat("\nstate:\n")
  print.data.frame(.ae_round_data_frame(x$state, digits = digits),
                   row.names = FALSE)
  if (!is.null(x$residual)) {
    cat("\nresidual:\n")
    print.data.frame(.ae_round_data_frame(x$residual, digits = digits),
                     row.names = FALSE)
  }
  invisible(x)
}

#' Plot AE problem calibration diagnostics
#' @method plot ae_problem_nfxp_fit
#' @keywords internal
#' @exportS3Method graphics::plot
plot.ae_problem_nfxp_fit <- function(x,
                                     type = c("fit", "convergence", "state"),
                                     ...) {
  type <- match.arg(type)
  if (type == "fit") {
    if (is.null(x$observed) || length(x$observed) != length(x$predicted)) {
      stop("fit plot requires stored observed values")
    }
    graphics::plot(
      as.numeric(x$observed),
      as.numeric(x$predicted),
      xlab = "Observed",
      ylab = "Predicted",
      main = "AE fit",
      ...
    )
    lim <- range(c(x$observed, x$predicted), finite = TRUE)
    graphics::abline(a = 0, b = 1, col = "gray50", lty = 2)
    invisible(lim)
  } else if (type == "convergence") {
    history <- x$equilibrium$history
    if (is.null(history) || nrow(history) == 0L) {
      stop("convergence plot requires stored solver history")
    }
    graphics::plot(
      history$iter,
      history$error,
      type = "l",
      xlab = "Iteration",
      ylab = "Error",
      main = "AE inner convergence",
      ...
    )
    invisible(history)
  } else {
    graphics::hist(
      as.numeric(x$state),
      xlab = x$state_name,
      main = "AE final state",
      ...
    )
    invisible(x$state)
  }
}

.ae_numeric_summary <- function(x) {
  x <- as.numeric(x)
  data.frame(
    min = min(x, na.rm = TRUE),
    q25 = stats::quantile(x, 0.25, na.rm = TRUE, names = FALSE),
    median = stats::median(x, na.rm = TRUE),
    mean = mean(x, na.rm = TRUE),
    q75 = stats::quantile(x, 0.75, na.rm = TRUE, names = FALSE),
    max = max(x, na.rm = TRUE),
    stringsAsFactors = FALSE
  )
}

.ae_format_number <- function(x, digits = 4) {
  if (is.null(x) || length(x) == 0L || is.na(x)) {
    return("NA")
  }
  format(signif(as.numeric(x)[1], digits = digits), scientific = FALSE)
}

.ae_round_data_frame <- function(x, digits = 4) {
  out <- x
  numeric_cols <- vapply(out, is.numeric, logical(1))
  out[numeric_cols] <- lapply(out[numeric_cols], signif, digits = digits)
  out
}
