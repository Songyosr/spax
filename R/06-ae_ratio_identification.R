# Private identification diagnostic for CLM ratio feedback -------------------

#' Compare static, ratio-feedback and free-attractiveness CLM fits
#'
#' Fits one origin-by-facility flow matrix five ways with a Poisson loss and
#' reports how much each model improves on the static CLM:
#'
#' - `static`: sigma and v0 (`feedback_scale = 1`);
#' - `static_b`: adds the supply elasticity b;
#' - `ratio`: adds the feedback coefficient gamma;
#' - `ratio_b`: both b and gamma;
#' - `free`: a separate attractiveness for every facility.
#'
#' The free model can reproduce any equilibrium attractiveness that a
#' feedback rule produces, so it bounds what a cross-section can attribute to
#' feedback. When the ratio models recover most of the free model's gain, the
#' gain is equally consistent with facility differences unrelated to load,
#' including supply that follows load (for example staffing set by list size).
#' In a single cross-section gamma is identified only through the assumed
#' load-response form and cannot be separated from load-correlated facility
#' differences; separating them needs supply that changes over time or capacity
#' that binds.
#'
#' @param inputs Prepared inputs from `prepare_allocation()`.
#' @param observed Origin-by-facility flow matrix with the prepared IDs as
#'   dimnames; unobserved entries are not allowed.
#' @param starts Matrix of explicit `sigma`/`v0` starts, one row per start.
#' @param lower,upper Named bounds for `sigma` and `v0`.
#' @param gamma_upper,b_upper Upper bounds for gamma and b.
#' @param solve_control Named solver controls for the feedback solves.
#' @return A data frame with one row per model: parameters, number of fitted
#'   parameters `k`, Poisson `loss`, `converged`, `gain` over the static fit and
#'   `share_of_free_gain`, with the interpretation as attribute `note`.
#' @keywords internal
#' @md
.ratio_identification <- function(inputs, observed, starts, lower, upper,
                                  gamma_upper = 5, b_upper = 3,
                                  solve_control = list(lambda = 0.4, tol = 1e-10,
                                                       max_iter = 5000)) {
  .chck_class(inputs, "ae_prepared_inputs", "inputs")
  ids <- list(inputs$origin_ids, inputs$facility_ids)
  if (!is.matrix(observed) || !identical(dimnames(observed), ids) ||
      anyNA(observed) || any(observed < 0)) {
    stop("observed must be a complete nonnegative flow matrix with the prepared IDs")
  }
  if (!identical(sort(names(lower)), c("sigma", "v0")) ||
      !identical(sort(names(upper)), c("sigma", "v0"))) {
    stop("lower and upper must be named sigma and v0")
  }
  if (!is.matrix(starts) || !identical(colnames(starts), c("sigma", "v0"))) {
    stop("starts must be a matrix with columns sigma and v0")
  }
  if (!identical(inputs$active_travel_status, rep("known", length(ids[[1]])))) {
    stop("ratio identification requires known travel for every active origin")
  }

  rbar <- sum(inputs$S) / sum(inputs$D_active)
  ratio <- .ratio_problem(inputs, ratio_ref = rbar)
  ratio_b <- .ratio_problem(inputs, fit_b = TRUE, ratio_ref = rbar)
  models <- list(
    static = .fix_problem_parameter(ratio, "feedback_scale", 1),
    static_b = .fix_problem_parameter(ratio_b, "feedback_scale", 1),
    ratio = ratio,
    ratio_b = ratio_b)
  extra <- c(feedback_scale = 1.5, supply_scale = 2)
  bounds_lo <- c(lower, feedback_scale = 1, supply_scale = 1)
  bounds_hi <- c(upper, feedback_scale = 1 + gamma_upper, supply_scale = 1 + b_upper)

  rows <- lapply(names(models), function(name) {
    problem <- models[[name]]
    pars <- problem$theta$names
    s <- vapply(pars, function(p) {
      if (p %in% colnames(starts)) starts[, p] else rep(extra[[p]], nrow(starts))
    }, numeric(nrow(starts)))
    s <- matrix(s, nrow = nrow(starts), dimnames = list(rownames(starts), pars))
    fit <- do.call(.fit_problem_multistart, c(list(problem, observed, s,
      bounds_lo[pars], bounds_hi[pars], output = "flow", loss = .poisson_loss,
      loss_grad = .poisson_gradient, loss_args = list(), gradient = TRUE,
      control = list(maxit = 500)), solve_control))
    if (is.null(fit$best)) stop(sprintf("no eligible fit for the %s model", name))
    th <- fit$best$theta
    data.frame(model = name, k = length(pars), loss = fit$best$loss,
      sigma = th[["sigma"]], v0 = th[["v0"]],
      gamma = if ("feedback_scale" %in% pars) th[["feedback_scale"]] - 1 else 0,
      b = if ("supply_scale" %in% pars) th[["supply_scale"]] - 1 else 1,
      boundary = isTRUE(any(fit$boundary[fit$best_start, ])),
      converged = identical(fit$best$convergence, 0L) &&
        isTRUE(fit$best$equilibrium$converged),
      stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows)

  best <- out[out$model == "static", ]
  free <- .clm_free_attractiveness_fit(inputs, observed, best$sigma, best$v0)
  out <- rbind(out, data.frame(model = "free", k = length(ids[[2]]) + 1L,
    loss = free$loss, sigma = free$sigma, v0 = free$v0, gamma = NA_real_,
    b = NA_real_, boundary = FALSE, converged = free$converged,
    stringsAsFactors = FALSE))

  out$gain <- out$loss[out$model == "static"] - out$loss
  free_gain <- out$gain[out$model == "free"]
  out$share_of_free_gain <- if (free_gain > 0) out$gain / free_gain else NA_real_
  rownames(out) <- NULL
  attr(out, "note") <- paste(
    "Gains are Poisson log-likelihood improvements over the static CLM.",
    "The free model sets each facility's attractiveness separately and bounds",
    "what a cross-section can attribute to feedback. A ratio model that",
    "recovers most of the free gain is equally consistent with facility",
    "differences unrelated to load, such as supply that follows load. In a",
    "single cross-section gamma is identified only by the assumed functional",
    "form and cannot be separated from load-correlated facility differences.")
  out
}

# Static CLM with a free attractiveness per facility (Gaussian kernel), fitted
# by Poisson maximum likelihood on flows with analytic gradients. The first
# facility's attractiveness is fixed at its supply; v0 sets the scale.
.clm_free_attractiveness_fit <- function(inputs, observed, sigma, v0) {
  D <- inputs$D_active
  S <- as.numeric(inputs$S)
  t2 <- inputs$distance_active^2
  Y <- observed
  J <- length(S)
  value <- function(p) {
    la <- c(log(S[1]), p[seq_len(J - 1)])
    sig <- exp(p[J])
    v <- exp(p[J + 1])
    K <- exp(-t2 / (2 * sig^2))
    O <- sweep(K, 2, exp(la), `*`)
    den <- rowSums(O) + v
    P <- O / den
    mu <- pmax(D * P, 1e-9) # same floor as .poisson_loss
    r <- mu - Y
    Rs <- rowSums(r)
    g_la <- colSums(r) - colSums(P * Rs)
    g_ls <- sum(r * t2 / sig^2) - sum(Rs * rowSums(P * t2 / sig^2))
    g_lv <- -sum(Rs * v / den)
    list(loss = sum(mu - Y * log(mu)), grad = c(g_la[-1], g_ls, g_lv))
  }
  p0 <- c(log(S[-1]), log(sigma), log(v0))
  opt <- stats::optim(p0, function(p) value(p)$loss, function(p) value(p)$grad,
                      method = "BFGS", control = list(maxit = 5000, reltol = 1e-14))
  if (opt$convergence != 0) warning("free-attractiveness fit did not converge")
  list(loss = opt$value, converged = opt$convergence == 0,
       sigma = exp(opt$par[J]), v0 = exp(opt$par[J + 1]),
       attractiveness = exp(c(log(S[1]), opt$par[seq_len(J - 1)])))
}
