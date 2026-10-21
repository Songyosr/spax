# SPAX-008: shared AE inner-equilibrium solver -------------------------------

#' Solve a fixed point with damped Picard iteration
#'
#' Solves `x = map(x, ...)` by iterating
#' `x_next = (1 - lambda) * x + lambda * map(x, ...)`.
#' @keywords internal
solve_equilibrium <- function(map, x0, lambda = 1, tol = 1e-8,
                              max_iter = 1000, norm = c("max", "l2"),
                              keep_history = TRUE, warn = TRUE,
                              check = TRUE, keep_state_history = FALSE, ...) {
  if (!is.function(map)) {
    stop("`map` must be a function")
  }
  .chck_numeric_vector(x0, "x0")
  x <- .coerce_numeric_vector(x0)
  .chck_positive_scalar(lambda, "lambda")
  if (lambda > 1) {
    stop("`lambda` must be <= 1")
  }
  .chck_positive_scalar(tol, "tol")
  .chck_positive_integer(max_iter, "max_iter")
  lambda <- as.numeric(lambda)
  tol <- as.numeric(tol)
  max_iter <- as.integer(max_iter)
  norm <- match.arg(norm)

  history <- if (keep_history) {
    data.frame(iter = integer(), error = numeric())
  } else {
    NULL
  }
  state_history <- if (isTRUE(keep_state_history)) {
    matrix(
      NA_real_,
      nrow = max_iter,
      ncol = length(x),
      dimnames = list(NULL, names(x))
    )
  } else {
    NULL
  }

  converged <- FALSE
  err <- Inf
  iter <- 0L
  message <- "maximum iterations reached"

  for (iter in seq_len(max_iter)) {
    target <- .solver_map_value(map(x, ...), x, check = check)
    x_next <- (1 - lambda) * x + lambda * target
    err <- .solver_norm(x_next - x, norm = norm)

    if (keep_history) {
      history <- rbind(history, data.frame(iter = iter, error = err))
    }
    if (isTRUE(keep_state_history)) {
      state_history[iter, ] <- x_next
    }

    x <- x_next
    if (err <= tol) {
      converged <- TRUE
      message <- "converged"
      break
    }
  }

  residual_norm <- NA_real_
  if (isTRUE(keep_state_history)) {
    state_history <- state_history[seq_len(iter), , drop = FALSE]
    rownames(state_history) <- as.character(seq_len(iter))
  }
  if (converged) {
    final_target <- .solver_map_value(map(x, ...), x, check = check)
    residual_norm <- .solver_norm(x - final_target, norm = norm)
  } else if (warn) {
    warning("fixed-point solver did not converge within `max_iter`")
  }

  structure(
    list(
      x_star = x,
      state = x,
      iters = iter,
      iterations = iter,
      converged = converged,
      error = err,
      residual_norm = residual_norm,
      history = history,
      state_history = state_history,
      message = message
    ),
    class = "ae_equilibrium"
  )
}

#' Spectral radius of a square Jacobian
#' @keywords internal
spectral_radius <- function(jac) {
  jac <- as.matrix(jac)
  if (nrow(jac) != ncol(jac)) {
    stop("`jac` must be square")
  }
  max(Mod(eigen(jac, only.values = TRUE)$values))
}

#' Finite-difference Jacobian of a vector-valued function
#'
#' Column `j` is a central difference with step `eps * max(abs(x[j]), 1)`. With
#' finite `lower` or `upper`, no evaluation point leaves the bounds: where one
#' side of a central step would, the difference is one-sided with the same step
#' (forward, or backward when only the lower side fits). When neither side
#' fits, the larger feasible one-sided step is used, `max(upper - x, x - lower)`,
#' and a coordinate with no room at all gives a zero column. Inside the bounds
#' the result is exactly the unbounded one. The `"forward"` method steps up when
#' that stays inside the bounds and down otherwise.
#' @param fn A function of a numeric vector returning a numeric vector.
#' @param x The point at which to differentiate, a numeric vector.
#' @param ... Further arguments passed to `fn`.
#' @param eps Relative step size.
#' @param method `"central"` or `"forward"` differences.
#' @param check Whether to check that `fn` returns numeric vectors.
#' @param lower,upper Inclusive bounds of the evaluation points, recycled to the
#'   length of `x`.
#' @param lazy_f0 If `TRUE`, `fn(x)` is evaluated (and checked as usual) only
#'   when a coordinate needs a one-sided or room step; the result is identical.
#' @keywords internal
#' @md
fd_jacobian <- function(fn, x, ..., eps = 1e-6,
                        method = c("central", "forward"),
                        check = TRUE, lower = -Inf, upper = Inf,
                        lazy_f0 = FALSE) {
  if (!is.function(fn)) {
    stop("`fn` must be a function")
  }
  .chck_numeric_vector(x, "x")
  .chck_positive_scalar(eps, "eps")
  x <- .coerce_numeric_vector(x)
  eps <- as.numeric(eps)
  method <- match.arg(method)
  n_in <- length(x)
  lower <- .fd_bound(lower, "lower", n_in)
  upper <- .fd_bound(upper, "upper", n_in)
  if (any(lower > upper)) {
    stop("`lower` must not exceed `upper`")
  }

  f0 <- NULL
  get_f0 <- function() {
    if (is.null(f0)) {
      f0 <<- .solver_function_value(fn(x, ...), "fn(x)", check = check)
    }
    f0
  }
  out <- NULL
  alloc <- function(n_out) {
    out <<- matrix(NA_real_, nrow = n_out, ncol = n_in)
    colnames(out) <<- names(x)
  }
  if (!lazy_f0) alloc(length(get_f0()))

  # The function at x moved by `delta` in coordinate j, kept inside the bounds.
  at <- function(j, delta, label) {
    step <- rep(0, n_in)
    step[j] <- delta
    point <- x + step
    point[j] <- min(max(point[j], lower[j]), upper[j])
    .solver_function_value(fn(point, ...), label, check = check)
  }

  for (j in seq_len(n_in)) {
    h <- eps * max(abs(x[j]), 1)
    up <- x[j] + h <= upper[j]
    down <- x[j] - h >= lower[j]
    if (method == "central" && up && down) {
      fp <- at(j, h, "fn(x + h)")
      fm <- at(j, -h, "fn(x - h)")
      if (is.null(out)) alloc(length(fp))
      out[, j] <- (fp - fm) / (2 * h)
    } else if (up) {
      fp <- at(j, h, "fn(x + h)")
      if (is.null(out)) alloc(length(get_f0()))
      out[, j] <- (fp - get_f0()) / h
    } else if (down) {
      fm <- at(j, -h, "fn(x - h)")
      if (is.null(out)) alloc(length(get_f0()))
      out[, j] <- (get_f0() - fm) / h
    } else {
      # Neither side of the step fits: use the larger room that is left.
      room_up <- max(upper[j] - x[j], 0)
      room_down <- max(x[j] - lower[j], 0)
      if (is.null(out)) alloc(length(get_f0()))
      if (room_up >= room_down && room_up > 0) {
        out[, j] <- (at(j, room_up, "fn(x + h)") - get_f0()) / room_up
      } else if (room_down > 0) {
        out[, j] <- (get_f0() - at(j, -room_down, "fn(x - h)")) / room_down
      } else {
        out[, j] <- 0
      }
    }
  }
  if (is.null(out)) alloc(length(get_f0()))

  out
}

# A bound argument of fd_jacobian(): numeric without missing values, of length one
# or the length of x, recycled to the length of x.
.fd_bound <- function(bound, name, n) {
  if (!is.numeric(bound) || anyNA(bound) || !length(bound) ||
      !length(bound) %in% unique(c(1L, n))) {
    stop("`", name, "` must be numeric without missing values, ",
         "of length one or the length of `x`")
  }
  rep_len(as.numeric(bound), n)
}

#' Finite-difference state Jacobian dT/dx
#' @keywords internal
fd_jacobian_state <- function(map, x, ..., eps = 1e-6,
                              method = c("central", "forward"),
                              check = TRUE) {
  fd_jacobian(map, x, ..., eps = eps, method = method, check = check)
}

.coerce_numeric_vector <- function(x) {
  out <- as.numeric(x)
  names(out) <- names(x)
  out
}

.solver_map_value <- function(value, x, check = TRUE) {
  if (!check) {
    return(value)
  }
  .chck_numeric_vector(value, "map result")
  value <- .coerce_numeric_vector(value)
  if (length(value) != length(x)) {
    stop("`map` must return a numeric vector with the same length as `x0`")
  }
  value
}

.solver_function_value <- function(value, name, check = TRUE) {
  if (!check) {
    return(value)
  }
  .chck_numeric_vector(value, name)
  .coerce_numeric_vector(value)
}

.solver_norm <- function(x, norm = c("max", "l2")) {
  norm <- match.arg(norm)
  if (norm == "max") {
    return(max(abs(x)))
  }
  sqrt(sum(x^2))
}
