# Private fixed-system CLM prediction --------------------------------------

.clm_prediction_spec <- function(spec) {
  spec[c("family", "allocation", "kappa", "beta", "v0", "fit_beta", "fit_v0")]
}

.new_clm_prediction_snapshot <- function(problem, step, evaluated) {
  declaration <- step[["clm_prediction", exact = TRUE]]
  if (is.null(declaration) || !isTRUE(evaluated$converged)) return(NULL)
  s <- problem$substrate
  # Plain O(J) values only: no provider closure, raster, origin registry or P.
  list(schema = "spax_clm_prediction_v1", kind = "builtin_static_clm",
    spec = declaration$spec, theta = declaration$theta,
    facility_ids = as.character(s$facility_ids), supply = as.numeric(s$S[, 1]),
    utilization = as.numeric(evaluated$outputs$utilization),
    input_metadata = s$input_metadata, input_policy = s$input_policy,
    provenance = list(converged = TRUE, zero_denominator = "zero_mass_undefined",
                       zero_load_ratio = "unsupported_capacity"))
}

.validate_clm_prediction_snapshot <- function(x) {
  fields <- c("schema", "kind", "spec", "theta", "facility_ids", "supply",
              "utilization", "input_metadata", "input_policy", "provenance")
  if (!is.list(x) || !identical(names(x), fields) ||
      !identical(x$schema, "spax_clm_prediction_v1") ||
      !identical(x$kind, "builtin_static_clm")) {
    stop("unsupported prediction snapshot; regenerate the evaluated CLM source")
  }
  spec <- x$spec
  expected <- c("family", "allocation", "kappa", "beta", "v0", "fit_beta", "fit_v0")
  scalar_choice <- function(value, choices) {
    is.character(value) && length(value) == 1L && !is.na(value) && value %in% choices
  }
  if (!is.list(spec) || !identical(names(spec), expected) ||
      !scalar_choice(spec$family, c("gaussian", "exponential", "power")) ||
      !identical(spec$allocation, "clm")) stop("invalid CLM snapshot specification")
  for (key in c("kappa", "beta", "v0")) {
    z <- spec[[key]]
    if (!is.numeric(z) || length(z) != 1L || !is.finite(z) ||
        z < 0 || (key != "v0" && z == 0)) stop("invalid CLM snapshot ", key)
  }
  for (key in c("fit_beta", "fit_v0")) {
    if (!is.logical(spec[[key]]) || length(spec[[key]]) != 1L || is.na(spec[[key]])) {
      stop("invalid CLM snapshot parameter flags")
    }
  }
  parameters <- .huff_theta_contract(spec$fit_v0, spec$fit_beta)$names
  if (!is.numeric(x$theta) || !is.null(dim(x$theta)) ||
      !identical(names(x$theta), parameters) ||
      any(!is.finite(x$theta) | x$theta < 0)) stop("invalid CLM snapshot theta")
  if (spec$fit_beta && x$theta[["beta"]] <= 0) stop("snapshot beta must be positive")
  ids <- .allocation_ids(x$facility_ids, "snapshot facility IDs")
  if (!identical(ids, x$facility_ids)) stop("snapshot facility IDs must be strings")
  for (key in c("supply", "utilization")) {
    value <- x[[key]]
    if (!is.numeric(value) || !is.null(dim(value)) || length(value) != length(ids) ||
        any(!is.finite(value) | value < 0)) stop("invalid snapshot ", key)
  }
  if (!identical(x$input_metadata, .allocation_metadata(x$input_metadata))) {
    stop("invalid snapshot input metadata")
  }
  p <- x$input_policy
  if (!is.list(p) || !identical(names(p),
      c("source", "missing_demand", "missing_travel", "kernel_domain")) ||
      !scalar_choice(p$source, c("numeric", "raster")) ||
      !scalar_choice(p$missing_demand, c("error", "exclude", "legacy_positive_fit")) ||
      !scalar_choice(p$missing_travel, c("error", "legacy_zero")) ||
      !scalar_choice(p$kernel_domain, c("strict", "legacy_zero"))) {
    stop("invalid snapshot input policy")
  }
  if (spec$family == "power" && x$theta[["sigma"]] == 0 && p$kernel_domain == "legacy_zero") {
    stop("legacy power sigma=0 treats +Inf differently; regenerate with checked numeric preparation or positive sigma")
  }
  if (!identical(x$provenance, list(converged = TRUE,
      zero_denominator = "zero_mass_undefined", zero_load_ratio = "unsupported_capacity"))) {
    stop("snapshot requires a successful fixed-state evaluation and known guard policy")
  }
  x
}

.clm_prediction_source <- function(source) {
  if (!inherits(source, "ae_equilibrium") && !inherits(source, "ae_problem_nfxp_fit")) {
    stop("source must be a solved or fitted built-in corrected CLM")
  }
  snapshot <- source[["prediction_snapshot", exact = TRUE]]
  if (is.null(snapshot)) stop("source has no prediction snapshot; regenerate the solved or fitted CLM")
  s <- .validate_clm_prediction_snapshot(snapshot)
  equal <- function(value, expected, label, numeric = FALSE) {
    if (is.null(value)) return(invisible(NULL))
    ok <- if (numeric) is.numeric(value) &&
      identical(as.numeric(value), as.numeric(expected)) else identical(value, expected)
    if (!ok) stop("source conflicts with its prediction snapshot: ", label)
  }
  equal(source$theta, s$theta, "theta")
  equal(source$theta_hat, s$theta, "theta_hat")
  equal(source$model, "huff", "model")
  equal(source$family, s$spec$family, "family")
  equal(source$allocation_form, "clm", "allocation_form")
  equal(source$utilization, s$utilization, "utilization", TRUE)
  equal(source$outputs$utilization, s$utilization, "output utilization", TRUE)
  equal(source$coverage_meta$facility_ids, s$facility_ids, "facility IDs")
  equal(source$coverage_meta$supply, s$supply, "supply", TRUE)
  equal(source$coverage_meta$input_metadata, s$input_metadata, "input metadata")
  equal(source$coverage_meta$input_policy, s$input_policy, "input policy")
  evaluated <- if (inherits(source, "ae_equilibrium")) source else source$equilibrium
  identity <- evaluated[["evaluated_clm", exact = TRUE]]
  if (!is.list(identity) || !identical(names(identity), c("spec", "theta", "facility_ids", "supply"))) {
    stop("source has no evaluated CLM identity; regenerate the solved or fitted CLM")
  }
  for (key in names(identity)) {
    equal(identity[[key]], s[[key]], paste("evaluated", key))
  }
  if (inherits(source, "ae_equilibrium")) {
    if (!isTRUE(source$converged)) stop("prediction requires a successful fixed-state evaluation")
  } else {
    if (!isTRUE(source$equilibrium$converged)) stop("prediction requires a successful final evaluation")
    equal(source$equilibrium$prediction_snapshot, s, "final evaluation snapshot")
    equal(source$equilibrium$utilization, s$utilization, "final evaluation utilization", TRUE)
    equal(source$equilibrium$outputs$utilization, s$utilization, "final output utilization", TRUE)
  }
  s
}

.clm_prediction_flow <- function(score, denominator, demand, ordinary = FALSE) {
  positive_demand <- demand[!is.na(demand) & demand > 0]
  if (ordinary && min(positive_demand, Inf) >= 1e-100 &&
      max(positive_demand, 0) <= 1e100) {
    return(score * (demand / denominator))
  }
  flow <- score
  for (j in seq_len(ncol(score))) {
    # Normalize before multiplication: D / denominator can overflow even
    # when every expected count D * P is finite. Only one column is temporary.
    probability <- score[, j] / denominator
    value <- probability * demand
    tiny <- probability == 0 & score[, j] > 0 & !is.na(demand) & demand > 0
    if (any(tiny)) {
      value[tiny] <- exp(log(score[tiny, j]) - log(denominator[tiny]) + log(demand[tiny]))
    }
    flow[, j] <- value
  }
  flow
}

#' Predict requested locations against a saved static CLM system
#'
#' Reads the portable prediction snapshot of a fixed solve or fitted object.
#' Requested rows do not change saved facility loads or rerun fitting/solving.
#' Named travel axes and demand are aligned independently to complete IDs.
#' Positive infinity is a known absent edge. Unknown travel makes the whole
#' normalized row unknown unless explicit `legacy_zero` imputation is requested.
#' Legacy finite-kernel guards are used only with that explicit request and a
#' compatible legacy source; their use is flagged. Negative travel always fails.
#'
#' `allocation` contains unconditional inside probabilities and `flow` expected
#' counts in the declared demand unit/period. Unknown demand leaves flow unknown,
#' including absent edges; known zero demand does not repair unknown travel.
#' Neither output is materialized unless requested. A zero choice denominator
#' retains the zero inside/outside/count guard with `probability_valid = FALSE`;
#' it does not define a normalized distribution or imply unmet need.
#'
#' Capacity ratios use saved supply/load for positive-load facilities. Contact
#' with positive-supply, zero-load facilities is reported as unsupported mass;
#' unqualified A/abar are NA there. Explicit A_supported/abar_supported use the
#' zero-contribution convention for these facilities. Unqualified A/abar remain
#' undefined for unsupported contact even when its mass rounds to zero: the
#' `unsupported_capacity` flag preserves positive-score support independently.
#' All abar values are undefined at zero contact. Supply is in its original
#' units, without kappa.
#' Numerically unrepresentable capacity attribution is flagged separately and
#' returned as NA, while valid probabilities and counts remain available.
#' If only conditional intensity overflows, A remains available and abar is NA
#' with a separate flag. Capacity validity refers to A; conditional capacity has
#' its own validity flag. A probability-underflow flag records positive scores
#' that normalize below double range; guarded count/capacity arithmetic can still
#' recover representable values without treating these scores as absent edges.
#' Legacy raster power sources at sigma=0 are incompatible with known +Inf
#' absence; regenerate with checked numeric preparation or positive sigma.
#' Snapshot fields are checked against the saved evaluated-system identity and
#' other redundant small fields. Editing these fields is not a scenario update:
#' prepare and evaluate a changed system. Coordinated manual mutation of every
#' copied field cannot be verified against historical travel and is unsupported.
#'
#' The result holds selected outputs, IDs, separate probability/demand/capacity
#' validity, declared units/policies and compact facility values. Optional spatial
#' metadata follows `.prepare_allocation()` and is saved as plain geometry.
#' Raster reconstruction is deferred to `.prediction_surface()`.
#' @keywords internal
.predict_clm <- function(source, distance, origin_ids = rownames(distance),
                         demand = NULL, outputs = c("rho", "A", "abar"),
                         missing_travel = c("unknown", "legacy_zero"), spatial = NULL) {
  missing_travel <- match.arg(missing_travel)
  s <- .clm_prediction_source(source)
  origins <- .allocation_ids(origin_ids, "origin_ids")
  facilities <- s$facility_ids
  .check_output_names(outputs, "outputs")
  supported_outputs <- c("rho", "outside_share", "A", "abar", "A_supported",
                         "abar_supported", "unsupported_contact", "allocation", "flow")
  if (is.null(outputs) || any(!outputs %in% supported_outputs)) stop("unsupported prediction outputs")
  if (!is.matrix(distance) || !is.numeric(distance) ||
      !identical(dim(distance), c(length(origins), length(facilities)))) {
    stop("distance must be a requested-origin-by-facility numeric matrix")
  }
  if (is.null(colnames(distance))) stop("distance must have complete facility column IDs")
  rows <- .allocation_order(rownames(distance), origins, "distance origin")
  cols <- .allocation_order(colnames(distance), facilities, "distance facility")
  distance <- distance[rows, cols, drop = FALSE]
  dimnames(distance) <- list(origins, facilities)
  minimum_distance <- min(distance, Inf, na.rm = TRUE)
  if (minimum_distance < 0) stop("travel must be nonnegative or positive infinity")
  n <- length(origins)
  if (is.null(demand)) demand <- rep(NA_real_, n)
  if (!is.numeric(demand) || !is.null(dim(demand)) || length(demand) != n ||
      any(!is.na(demand) & (!is.finite(demand) | demand < 0))) {
    stop("demand must be nonnegative finite or missing, matching requested origins")
  }
  demand <- unname(demand[.allocation_order(names(demand), origins, "demand")])
  demand_known <- !is.na(demand)
  geometry <- .allocation_spatial(spatial, origins)
  # Keep sparse edge indices and row flags, not multiple dense logical masks.
  # Original finite distances are unchanged, including zero-domain checks below.
  nonfinite <- if (anyNA(distance) || max(distance, 0, na.rm = TRUE) == Inf)
    which(!is.finite(distance)) else integer()
  unknown <- rep(FALSE, n)
  if (length(nonfinite)) {
    missing <- nonfinite[is.na(distance[nonfinite])]
    unknown[(missing - 1L) %% n + 1L] <- TRUE
    distance[nonfinite] <- 1
  }
  row_unknown <- unknown & missing_travel == "unknown"
  spec <- s$spec; theta <- s$theta
  sigma <- theta[["sigma"]]
  beta <- if (spec$fit_beta) theta[["beta"]] else spec$beta
  v0 <- if (spec$fit_v0) theta[["v0"]] else spec$v0
  legacy <- missing_travel == "legacy_zero" && s$input_policy$kernel_domain == "legacy_zero"
  if (!legacy && spec$family == "power" && minimum_distance == 0) {
    stop("power decay requires strictly positive finite travel distances")
  }
  if (!legacy && spec$family == "gaussian" && sigma <= 0) {
    stop("gaussian decay requires positive sigma")
  }
  kernel <- calc_decay(distance, spec$family, sigma, snap = TRUE)
  rm(distance)
  kernel_min <- min(kernel, Inf, na.rm = TRUE)
  kernel_max <- max(kernel, 0, na.rm = TRUE)
  legacy_kernel <- rep(FALSE, n)
  if (anyNA(kernel) || !is.finite(kernel_max) || kernel_min < 0) {
    invalid <- which(!is.finite(kernel) | kernel < 0)
    if (!legacy) stop("decay kernel is invalid on finite requested travel")
    legacy_kernel[(invalid - 1L) %% n + 1L] <- TRUE
    kernel[invalid] <- 0
  }
  if (length(nonfinite)) kernel[nonfinite] <- 0
  attractiveness <- (spec$kappa * s$supply)^beta
  bad_attractiveness <- !is.finite(attractiveness)
  if (!legacy && any(bad_attractiveness)) stop("requested CLM attractiveness is nonfinite")
  attractiveness[bad_attractiveness] <- 0
  # Bounds precede zeroing known absent/unknown edges. Finite kernel zeros or
  # any extreme positive score disable the certificate; they are never skipped
  # when looking for a positive lower bound. Zero supply contributes zero.
  score_lower <- kernel_min * min(attractiveness[attractiveness > 0], Inf)
  score_upper <- kernel_max * max(attractiveness)
  score <- kernel * rep(attractiveness, each = n)
  rm(kernel)
  inside <- rowSums(score)
  denominator <- inside + v0
  # Scores are nonnegative; a nonfinite entry also makes its row sum nonfinite.
  if (any(!is.finite(denominator))) {
    stop("requested CLM opportunity or denominator overflowed")
  }
  normalizer <- ifelse(denominator > 0, denominator, 1)
  undefined <- denominator == 0 & !row_unknown
  rho <- inside / normalizer
  outside <- if (v0 > 0) v0 / normalizer else rep(0, n)
  ratio <- numeric(length(facilities)); positive_load <- s$utilization > 0
  ratio[positive_load] <- s$supply[positive_load] / s$utilization[positive_load]
  ratio[!is.finite(ratio)] <- NA_real_
  unsupported <- A_supported <- numeric(n)
  unsupported_capacity <- rep(FALSE, n)
  probability_underflow <- rep(FALSE, n)
  # Conservative ordinary-range certificate. Every positive score, denominator
  # and nonzero ratio is in [1e-100, 1e100]. A product is >=1e-200 and <=1e200;
  # summing at most .Machine$integer.max columns stays <2.2e209. Division cannot
  # underflow a positive contribution (>=1e-300), nor can P underflow (>=1e-200).
  # Zeros contribute exactly zero. Extreme inputs keep direct normalization and
  # the guarded log path below, including positive-score unsupported contacts.
  ordinary <- is.finite(score_lower) && score_lower >= 1e-100 &&
    is.finite(score_upper) && score_upper <= 1e100 &&
    min(normalizer) >= 1e-100 && max(normalizer) <= 1e100 && !anyNA(ratio) &&
    min(ratio[ratio > 0], Inf) >= 1e-100 && max(ratio) <= 1e100
  if (ordinary) {
    A_supported <- as.vector(score %*% ratio) / normalizer
    unsupported_facilities <- !positive_load & s$supply > 0
    if (any(unsupported_facilities)) {
      unsupported_score <- as.vector(score %*% as.numeric(unsupported_facilities))
      unsupported_capacity <- unsupported_score > 0
      unsupported <- unsupported_score / normalizer
    }
  } else for (j in seq_along(facilities)) {
    probability <- score[, j] / normalizer
    probability_underflow <- probability_underflow | (probability == 0 & score[, j] > 0)
    if (positive_load[j]) {
      if (is.finite(ratio[j])) {
        contribution <- probability * ratio[j]
        tiny <- probability == 0 & score[, j] > 0 & ratio[j] > 0
      } else {
        contribution <- numeric(n)
        tiny <- rep(TRUE, n)
      }
      # Avoid both score*r overflow and P*S underflow. The exceptional log
      # path preserves representable attribution when r or P is out of range.
      if (any(tiny)) {
        contribution[tiny] <- exp(log(score[tiny, j]) - log(normalizer[tiny]) +
          log(s$supply[j]) - log(s$utilization[j]))
      }
      A_supported <- A_supported + contribution
    } else if (s$supply[j] > 0) {
      unsupported <- unsupported + probability
      unsupported_capacity <- unsupported_capacity | score[, j] > 0
    }
  }
  abar_supported <- rep(NA_real_, n)
  contact <- rho > 0
  abar_supported[contact] <- A_supported[contact] / rho[contact]
  capacity_overflow <- !is.finite(A_supported)
  conditional_overflow <- contact & !is.finite(abar_supported) & !capacity_overflow
  A_supported[capacity_overflow] <- NA_real_
  abar_supported[capacity_overflow | conditional_overflow] <- NA_real_
  A <- A_supported; abar <- abar_supported
  A[unsupported_capacity] <- abar[unsupported_capacity] <- NA_real_
  vectors <- list(rho = rho, outside_share = outside, A = A, abar = abar,
    A_supported = A_supported, abar_supported = abar_supported, unsupported_contact = unsupported)
  for (name in names(vectors)) {
    vectors[[name]][row_unknown] <- NA_real_
    names(vectors[[name]]) <- origins
  }
  result <- vectors[intersect(outputs, names(vectors))]
  if ("allocation" %in% outputs) {
    allocation <- score / normalizer
    allocation[row_unknown, ] <- NA_real_
    result$allocation <- allocation
  }
  if ("flow" %in% outputs) {
    flow <- .clm_prediction_flow(score, normalizer, demand, ordinary = ordinary)
    flow[row_unknown, ] <- NA_real_
    result$flow <- flow
  }
  imputed <- (unknown & missing_travel == "legacy_zero") | legacy_kernel | any(bad_attractiveness)
  probability_valid <- !row_unknown & !undefined
  capacity_valid <- probability_valid & !unsupported_capacity & !capacity_overflow
  status <- rep("valid", n)
  status[inside == 0] <- "known_absence"
  status[imputed] <- "legacy_imputed"
  status[probability_underflow] <- "probability_numerical_underflow"
  status[conditional_overflow] <- "conditional_capacity_numerical_overflow"
  status[unsupported_capacity] <- "unsupported_capacity"
  status[capacity_overflow] <- "capacity_numerical_overflow"
  status[undefined] <- "undefined_choice_denominator"
  status[row_unknown] <- "unknown_travel"
  validity <- data.frame(origin_id = origins, status = status,
    probability_valid = probability_valid, demand_known = demand_known,
    demand_status = ifelse(!demand_known, "unknown", ifelse(demand == 0, "zero", "positive")),
    capacity_valid = capacity_valid, unknown_travel = unknown,
    conditional_capacity_valid = capacity_valid & contact & !conditional_overflow,
    legacy_imputed = imputed, legacy_kernel_imputed = legacy_kernel,
    capacity_overflow = capacity_overflow & !row_unknown,
    conditional_capacity_overflow = conditional_overflow & !row_unknown,
    probability_underflow = probability_underflow & !row_unknown,
    unsupported_capacity = unsupported_capacity & !row_unknown,
    undefined_denominator = undefined, unsupported_contact = unname(vectors$unsupported_contact),
    stringsAsFactors = FALSE)
  structure(list(origin_ids = origins, facility_ids = facilities, outputs = result[outputs],
    validity = validity, units = s$input_metadata,
    policy = list(missing_travel = missing_travel,
      kernel_domain = if (legacy) "legacy_zero" else "strict",
      source_input = s$input_policy,
      legacy_attractiveness_imputed = any(bad_attractiveness)),
    facilities = data.frame(facility_id = facilities, supply = s$supply,
      utilization = s$utilization, ratio = ratio, ratio_representable = !is.na(ratio),
      supported = positive_load),
    spatial = geometry), class = "ae_clm_prediction")
}

#' Reconstruct one saved requested-location prediction vector as a raster
#'
#' Uses only validated requested-cell geometry and stored vector values. Other
#' cells remain NA. Matrix outputs require explicit numeric access and cannot be
#' silently flattened into a surface. This never evaluates the CLM again.
#' @keywords internal
.prediction_surface <- function(x, output = "A") {
  .chck_class(x, "ae_clm_prediction", "x")
  if (!is.character(output) || length(output) != 1L || is.na(output) || !nzchar(output)) {
    stop("output must be one nonmissing name")
  }
  .rewrap_problem_surface(x$outputs, output, x$spatial$template, x$spatial$cell_index)
}
