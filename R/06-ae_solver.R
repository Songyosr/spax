# SPAX-008: shared AE fixed-point solver -------------------------------------

#' Solve a fixed point with damped Picard iteration
#'
#' Solves `x = step(x, ...)` by iterating
#' `x_next = (1 - lambda) * x + lambda * step(x, ...)`.
#' @keywords internal
solve_equilibrium <- function(step, x0, lambda = 1, tol = 1e-8,
                              max_iter = 1000, norm = c("max", "l2"),
                              keep_history = TRUE, warn = TRUE,
                              check_step = TRUE, ...) {
  if (!is.function(step)) {
    stop("`step` must be a function")
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

  converged <- FALSE
  err <- Inf
  iter <- 0L
  message <- "maximum iterations reached"

  for (iter in seq_len(max_iter)) {
    target <- .solver_step_value(step(x, ...), x, check = check_step)
    x_next <- (1 - lambda) * x + lambda * target
    err <- .solver_norm(x_next - x, norm = norm)

    if (keep_history) {
      history <- rbind(history, data.frame(iter = iter, error = err))
    }

    x <- x_next
    if (err <= tol) {
      converged <- TRUE
      message <- "converged"
      break
    }
  }

  residual_norm <- NA_real_
  if (converged) {
    final_target <- .solver_step_value(step(x, ...), x, check = check_step)
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
      message = message
    ),
    class = "ae_equilibrium"
  )
}

#' Fixed-point residual under F(x, theta) = x - T_theta(x)
#' @keywords internal
equilibrium_residual <- function(map, x, ..., check = TRUE) {
  if (!is.function(map)) {
    stop("`map` must be a function")
  }
  .chck_numeric_vector(x, "x")
  x <- .coerce_numeric_vector(x)
  x - .solver_step_value(map(x, ...), x, check = check)
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
#' @keywords internal
fd_jacobian <- function(fn, x, ..., eps = 1e-6,
                        method = c("central", "forward"),
                        check = TRUE) {
  if (!is.function(fn)) {
    stop("`fn` must be a function")
  }
  .chck_numeric_vector(x, "x")
  .chck_positive_scalar(eps, "eps")
  x <- .coerce_numeric_vector(x)
  eps <- as.numeric(eps)
  method <- match.arg(method)

  f0 <- .solver_function_value(fn(x, ...), "fn(x)", check = check)
  n_out <- length(f0)
  n_in <- length(x)
  out <- matrix(NA_real_, nrow = n_out, ncol = n_in)
  colnames(out) <- names(x)

  for (j in seq_len(n_in)) {
    h <- eps * max(abs(x[j]), 1)
    step <- rep(0, n_in)
    step[j] <- h
    if (method == "central") {
      fp <- .solver_function_value(fn(x + step, ...), "fn(x + h)", check = check)
      fm <- .solver_function_value(fn(x - step, ...), "fn(x - h)", check = check)
      out[, j] <- (fp - fm) / (2 * h)
    } else {
      fp <- .solver_function_value(fn(x + step, ...), "fn(x + h)", check = check)
      out[, j] <- (fp - f0) / h
    }
  }

  out
}

#' Finite-difference state Jacobian dT/dx
#' @keywords internal
fd_jacobian_state <- function(map, x, ..., eps = 1e-6,
                              method = c("central", "forward"),
                              check = TRUE) {
  fd_jacobian(map, x, ..., eps = eps, method = method, check = check)
}

#' Finite-difference parameter Jacobian dT/dtheta at fixed state x
#' @keywords internal
fd_jacobian_param <- function(map, theta, x, ..., eps = 1e-6,
                              method = c("central", "forward"),
                              check = TRUE) {
  fn <- function(theta_value, ...) {
    map(x, theta_value, ...)
  }
  fd_jacobian(fn, theta, ..., eps = eps, method = method, check = check)
}

.coerce_numeric_vector <- function(x) {
  out <- as.numeric(x)
  names(out) <- names(x)
  out
}

.solver_step_value <- function(value, x, check = TRUE) {
  if (!check) {
    return(value)
  }
  .chck_numeric_vector(value, "step result")
  value <- .coerce_numeric_vector(value)
  if (length(value) != length(x)) {
    stop("`step` must return a numeric vector with the same length as `x0`")
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
