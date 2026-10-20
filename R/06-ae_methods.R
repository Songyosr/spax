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

# Display label for a fitted model: corrected CLM fits are built by the internal
# huff family with allocation form "clm"; show the allocation users declared.
.ae_model_label <- function(x) {
  if (identical(x$model, "huff") && identical(x$allocation_form, "clm")) "clm" else x$model
}

# Display label for the decay family of a fit or fit summary, with what sigma
# means: "gaussian (sigma = travel scale)". The built-in meanings come from a
# fixed map; a declared kernel gives its own through the prediction snapshot.
# Without a known meaning the family is shown alone.
.fit_family_label <- function(x) {
  family <- x$family
  if (!is.character(family) || length(family) != 1L || is.na(family)) return(family)
  builtin <- c(gaussian = "travel scale", exponential = "rate", power = "exponent")
  # Demand groups: a family given by group is listed by group; a shared one is
  # labelled like an ungrouped family (a declared kernel gives its own meaning).
  families <- x[["families", exact = TRUE]]
  if (!is.null(families) && length(families)) {
    labels <- vapply(families, .group_family_label, character(1))
    if (identical(family, "grouped")) {
      return(paste0("by group (", paste0(names(families), ": ", labels, collapse = "; "), ")"))
    }
    return(labels[[1L]])
  }
  parameter <- if (family %in% names(builtin)) builtin[[family]] else
    x$prediction_snapshot$spec$kernel$parameter
  if (!is.character(parameter) || length(parameter) != 1L || is.na(parameter)) return(family)
  paste0(family, " (sigma = ", parameter, ")")
}

# "gaussian (sigma = travel scale)" for one family of a grouped model: a built-in
# name or a declared kernel.
.group_family_label <- function(f) {
  builtin <- c(gaussian = "travel scale", exponential = "rate", power = "exponent")
  if (inherits(f, "spax_decay_kernel")) {
    name <- f$name
    parameter <- f$parameter
  } else {
    name <- f
    parameter <- if (name %in% names(builtin)) builtin[[name]]
  }
  if (!is.character(parameter) || length(parameter) != 1L || is.na(parameter)) return(name)
  paste0(name, " (sigma = ", parameter, ")")
}

# The group lines of a grouped fit or summary (none for an ungrouped one): the
# group IDs, the parameters that differ by group, the parameters held fixed.
.group_lines <- function(x, digits = 4) {
  groups <- x[["groups", exact = TRUE]]
  if (is.null(groups)) return(character())
  by_group <- x[["by_group", exact = TRUE]]
  fixed <- x[["fixed", exact = TRUE]]
  if (is.data.frame(fixed)) fixed <- stats::setNames(fixed$value, fixed$parameter)
  c(paste0("groups: ", paste(groups, collapse = ", ")),
    paste0("by_group: ", if (length(by_group)) paste(by_group, collapse = ", ") else "none"),
    if (length(fixed)) c("fixed:", paste0("  ", names(fixed), ": ",
      vapply(fixed, .ae_format_number, character(1), digits = digits))))
}

#' Print an AE problem calibration fit
#' @method print ae_problem_nfxp_fit
#' @keywords internal
#' @export
print.ae_problem_nfxp_fit <- function(x, digits = 4, ...) {
  cat("<ae_problem_nfxp_fit>\n")
  cat("model: ", .ae_model_label(x), "\n", sep = "")
  cat("family: ", .fit_family_label(x), "\n", sep = "")
  if (!is.null(x$allocation_form)) {
    cat("allocation_form: ", x$allocation_form, "\n", sep = "")
  }
  group_lines <- .group_lines(x, digits = digits)
  if (length(group_lines)) {
    # groups and by_group, then the fixed values after theta
    cat(group_lines[1:2], sep = "\n")
  }
  cat("theta:\n")
  theta <- x$theta
  if (is.null(names(theta))) {
    names(theta) <- paste0("theta", seq_along(theta))
  }
  for (nm in names(theta)) {
    cat("  ", nm, ": ", .ae_format_number(theta[[nm]], digits = digits),
        "\n", sep = "")
  }
  if (length(group_lines) > 2L) cat(group_lines[-(1:2)], sep = "\n")
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
    stringsAsFactors = FALSE, check.names = FALSE
  )

  residual_summary <- NULL
  if (!is.null(object$observed) && length(object$observed) == length(object$predicted)) {
    residual_summary <- .ae_numeric_summary(
      as.numeric(object$predicted) - as.numeric(object$observed)
    )
  }

  out <- structure(
    list(
      model = object$model,
      family = object$family,
      family_label = .fit_family_label(object),
      allocation_form = object$allocation_form,
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
  groups <- object[["groups", exact = TRUE]]
  if (!is.null(groups)) {
    # Appended only for a model with demand groups.
    fixed <- object[["fixed", exact = TRUE]]
    out$groups <- groups
    out$by_group <- object[["by_group", exact = TRUE]]
    out$fixed <- data.frame(parameter = names(fixed), value = as.numeric(fixed),
                            stringsAsFactors = FALSE, check.names = FALSE)
  }
  out
}

#' Print an AE problem calibration summary
#' @method print summary.ae_problem_nfxp_fit
#' @keywords internal
#' @export
print.summary.ae_problem_nfxp_fit <- function(x, digits = 4, ...) {
  cat("<summary.ae_problem_nfxp_fit>\n")
  cat("model: ", .ae_model_label(x), "\n", sep = "")
  # Summaries saved before the label was stored show the family alone.
  cat("family: ", x$family_label %||% x$family, "\n", sep = "")
  if (!is.null(x$allocation_form)) {
    cat("allocation_form: ", x$allocation_form, "\n", sep = "")
  }
  group_lines <- .group_lines(x, digits = digits)
  if (length(group_lines)) cat(group_lines[1:2], sep = "\n")
  cat("\n")
  cat("theta:\n")
  print.data.frame(.ae_round_data_frame(x$theta, digits = digits),
                   row.names = FALSE)
  if (length(group_lines) > 2L) {
    cat("\n")
    cat(group_lines[-(1:2)], sep = "\n")
  }
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

# Bound models and prepared inputs ----------------------------------------------------

# A count or total with thousands separators, to seven significant digits (used by the
# lines of bound models here and of prepared inputs, format.ae_prepared_inputs()).
.format_total <- function(x) {
  format(x, big.mark = ",", digits = 7L, scientific = FALSE, trim = TRUE)
}

#' Format and print a bound allocation model
#'
#' A model bound to its data prints like the specification it came from (see
#' [spec_clm()]): the decay, the attractiveness and the outside weight, then a data line
#' with the numbers of active origins and facilities. The free parameters of the model
#' show as `fit` and the held ones as their value. A model of the historical
#' `huff_decay` allocation (also one saved before the allocation was recorded), and the
#' internal no-decay comparator, print one line and the names of their parameters.
#' @param x A model, an `ae_problem` of the Huff or conditional logit family.
#' @param ... Unused.
#' @method format huff_problem
#' @keywords internal
#' @export
#' @md
format.huff_problem <- function(x, ...) {
  spec <- x$metadata$spec
  # A model from before the allocation was recorded (SPAX-033) is a huff_decay model.
  allocation <- spec[["allocation", exact = TRUE]]
  if (is.null(allocation)) allocation <- "huff_decay"
  if (!identical(allocation, "clm") || identical(spec$family, "none")) {
    return(c(paste0("<ae_problem> ", x$model, " (", allocation, ", family ",
                    spec$family, ")"),
             paste0("parameters: ", if (length(x$theta$names)) {
               paste(x$theta$names, collapse = ", ")
             } else {
               "none"
             })))
  }
  line <- function(label, text) sprintf("  %-10s  %s", label, text)
  huff <- !isTRUE(spec$fit_v0) && !any(c(spec$v0, spec$v0_groups) > 0, na.rm = TRUE)
  s <- x$substrate
  c(paste0("<spax model> ", if (huff) "Huff model (no outside option)" else
             "conditional logit with an outside option"),
    line("decay", .bound_decay_line(x)),
    line("attraction", paste0("(kappa * supply)^beta: kappa = ", .spec_number(spec$kappa),
                              ", beta = ", .bound_status(x, "beta", spec$beta))),
    line("outside", .bound_outside_line(x)),
    line("data", paste0(.format_total(length(s$origin_ids)), " origins x ",
                        .format_total(length(s$facility_ids)), " facilities")))
}

#' @rdname format.huff_problem
#' @method print huff_problem
#' @keywords internal
#' @export
#' @md
print.huff_problem <- function(x, ...) {
  cat(format(x), sep = "\n")
  invisible(x)
}

# "fit" for a free parameter of the model, the value for a held one: a parameter
# fixed at a value (demand groups) or a constant of the specification (`value`).
.bound_status <- function(x, name, value) {
  fixed <- x$metadata$spec[["fixed", exact = TRUE]]
  if (name %in% x$theta$names) return("fit")
  if (name %in% names(fixed)) return(.decay_sigma_text(fixed[[name]]))
  .decay_sigma_text(value)
}

# The decay line of a bound model, as the specification prints it: one label for a
# shared kernel (`fit by group` for one fitted kernel with a sigma for each group) and
# `by group:` with each group's kernel for kernels that differ or are partly held.
.bound_decay_line <- function(x) {
  spec <- x$metadata$spec
  groups <- spec[["groups", exact = TRUE]]
  # The label of the family, as for a fit (`.fit_family_label()`).
  label <- .fit_family_label(list(
    family = spec$family, families = spec[["families", exact = TRUE]],
    prediction_snapshot = list(spec = list(kernel = spec[["kernel", exact = TRUE]]))))
  if (is.null(groups) || !"sigma" %in% spec$by_group) {
    return(paste0(label, ": ", .bound_status(x, "sigma", NA)))
  }
  status <- vapply(groups, function(g) .bound_status(x, paste0("sigma.", g), NA), character(1))
  if (!identical(spec$family, "grouped") && all(status == "fit")) {
    return(paste0(label, ": fit by group"))
  }
  kernels <- vapply(groups, function(g) {
    f <- spec$families[[g]]
    if (inherits(f, "spax_decay_kernel")) f$name else f
  }, character(1))
  paste0("by group: ", paste0(groups, " ", kernels, " (", status, ")", collapse = ", "))
}

# The outside line of a bound model: `none` without an outside weight, a shared weight,
# or the weights by group.
.bound_outside_line <- function(x) {
  spec <- x$metadata$spec
  groups <- spec[["groups", exact = TRUE]]
  held <- spec[["v0_groups", exact = TRUE]]
  if (!is.null(held)) {
    if (all(held == 0)) return("none")
    return(paste0("v0 by group: ", paste(names(held), .spec_number(held), collapse = ", ")))
  }
  if (!isTRUE(spec$fit_v0)) {
    if (spec$v0 == 0) return("none")
    return(paste0("v0: ", .spec_number(spec$v0)))
  }
  if (is.null(groups) || !"v0" %in% spec$by_group) {
    return(paste0("v0: ", .bound_status(x, "v0", spec$v0)))
  }
  status <- vapply(groups, function(g) .bound_status(x, paste0("v0.", g), spec$v0), character(1))
  if (all(status == "fit")) return("v0: fit by group")
  paste0("v0 by group: ", paste(groups, status, collapse = ", "))
}
