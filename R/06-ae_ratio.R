# Private utilization-state ratio response. Positive, measured supply only.
# References are fixed numeric units, not estimated scale parameters.
.ratio_problem <- function(substrate, b = 1, fit_b = FALSE, eps = 1e-6,
                           supply_ref = 1, ratio_ref = 1) {
  .chck_nonnegative_scalar(b, "b")
  .chck_positive_scalar(eps, "eps")
  .chck_positive_scalar(supply_ref, "supply_ref")
  .chck_positive_scalar(ratio_ref, "ratio_ref")
  if (!is.logical(fit_b) || length(fit_b) != 1L || is.na(fit_b)) {
    .stop("fit_b must be TRUE or FALSE")
  }
  .refuse_grouped_substrate("the ratio response", substrate)
  S <- substrate$S
  D <- substrate$D_active
  distance <- substrate$distance_active
  if (!is.matrix(S) || ncol(S) != 1L || !nrow(S) ||
      any(!is.finite(S) | S <= 0)) .stop("ratio response requires positive measured supply")
  S <- as.numeric(S)
  if (!is.numeric(D) || !length(D) || any(!is.finite(D) | D <= 0) ||
      !is.matrix(distance) || !identical(dim(distance), c(length(D), length(S))) ||
      length(substrate$facility_ids) != length(S) ||
      anyNA(substrate$facility_ids) || anyDuplicated(substrate$facility_ids)) {
    .stop("invalid prepared ratio substrate or axes")
  }
  theta <- list(names = c("sigma", "v0", "feedback_scale"),
                lower = c(0, 0, 1), upper = rep(Inf, 3))
  if (fit_b) {
    theta$names <- c(theta$names, "supply_scale")
    theta$lower <- c(theta$lower, 1)
    theta$upper <- c(theta$upper, Inf)
  }
  spec <- list(model = "ratio", family = "gaussian", allocation = "clm",
               deterministic = TRUE, b = b, fit_b = fit_b, eps = eps,
               supply_ref = supply_ref, ratio_ref = ratio_ref, theta = theta,
               response = "capacity_per_contact_ratio",
               output_axes = c(target = "facility", utilization = "facility",
                 attractiveness = "facility", access = "origin",
                 outside_share = "origin", allocation = "origin_facility",
                 flow = "origin_facility", opportunity = "origin_facility"))
  logS <- log(S / supply_ref)
  D2 <- distance^2
  D2[!is.finite(D2)] <- 0
  bind <- function(par) {
    par <- .coerce_problem_theta(par, theta)
    if (par[["sigma"]] <= 0 || par[["v0"]] <= 0) .stop("sigma and v0 must be positive")
    gamma <- par[["feedback_scale"]] - 1
    baseline <- if (fit_b) par[["supply_scale"]] - 1 else b
    K <- calc_decay(distance, method = "gaussian", sigma = par[["sigma"]], snap = TRUE)
    K[!is.finite(K)] <- 0
    base <- baseline * logS
    constant <- NULL
    eval <- function(U, full = FALSE) {
      if (length(U) != length(S) || any(!is.finite(U) | U < 0)) .stop("invalid load state")
      if (!is.null(constant)) return(if (full) constant else constant$target)
      loga <- base
      if (gamma != 0) loga <- loga + gamma * log(S / ((U + eps) * ratio_ref))
      a <- exp(loga)
      if (any(!is.finite(a))) .stop("nonfinite ratio attractiveness")
      phi <- sweep(K, 2, a, `*`)
      denom <- rowSums(phi) + par[["v0"]]
      P <- phi / denom
      T <- as.numeric(crossprod(D, P))
      if (!full) return(T)
      list(target = T, utilization = T, attractiveness = a, allocation = P,
           access = rowSums(P), outside_share = par[["v0"]] / denom,
           opportunity = phi, flow = D * P)
    }
    if (gamma == 0) constant <- eval(rep(0, length(S)), TRUE)
    # A derivative evaluation reuses its exact-state intermediates locally.
    # No equilibrium or outputs are cached across binds/parameter values.
    last <- NULL
    deriv <- function(U) {
      if (!is.null(last) && identical(U, last$U)) return(last$value)
      st <- eval(U, TRUE)
      P <- st$allocation
      T <- st$utilization
      B <- diag(T, nrow = length(S)) - crossprod(P, D * P)
      Ju <- -gamma * sweep(B, 2, 1 / (U + eps), `*`)
      score <- D2 / par[["sigma"]]^3
      dsigma <- colSums(D * P * (score - rowSums(P * score)))
      dv0 <- -colSums(D * P * st$outside_share) / par[["v0"]]
      signal <- log(S / ((U + eps) * ratio_ref))
      dg <- T * signal - colSums(D * P * as.numeric(P %*% signal))
      Jp <- cbind(dsigma, dv0, dg)
      if (fit_b) Jp <- cbind(Jp, T * logS - colSums(D * P * as.numeric(P %*% logS)))
      colnames(Jp) <- theta$names
      value <- list(jac_state = Ju, jac_utilization = Ju, jac_param = Jp)
      last <<- list(U = U, value = value)
      value
    }
    list(map = function(U) eval(U), outputs = function(U) eval(U, TRUE),
         jac_state = function(U) deriv(U), jac_param = function(U) deriv(U)$jac_param,
         jac_output_param = function(U, output) {
           if (output %in% c("utilization", "target")) deriv(U)$jac_param else NULL
         }, theta = par)
  }
  .new_problem("ratio", substrate,
    state = list(name = "U", axis = "J", init = rep(sum(D) / (2 * length(S)), length(S)),
                 lower = 0, upper = sum(D)), theta = theta, bind = bind,
    metadata = list(spec = spec))
}
