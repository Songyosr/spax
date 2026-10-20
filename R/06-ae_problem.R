# Private AE problem / map compiler scaffold ---------------------------------
#
# Ring-1 design object: a theta-free inner problem whose `bind` closure performs
# theta-only work once, then returns a hot-loop step function of state only.
# This is intentionally private and thin; SAE/HAAE remain the extraction oracles.

#' Canonical theta contract for single decay-parameter AE models
#'
#' Single source of truth for the sigma validity bounds. Both the bind closure
#' and the problem object read this, so the contract cannot drift between them.
#' @keywords internal
#' @md
.decay_theta_contract <- function() {
  list(names = "sigma", lower = 0, upper = Inf)
}

#' Require an AE outputs list to carry the contracted keys
#'
#' Every bound step's `outputs(x)` must expose `target` and `utilization` so
#' runners and loss functions can bind to known quantities without defensive
#' NULL checks.
#' @keywords internal
#' @md
.validate_ae_outputs <- function(outputs) {
  if (!is.list(outputs)) {
    stop("AE outputs must be a list")
  }
  missing <- setdiff(c("target", "utilization"), names(outputs))
  if (length(missing) > 0) {
    stop("AE outputs must include: ", paste(missing, collapse = ", "))
  }
  outputs
}

#' Warn when a calibrated parameter lands on its search-box bound
#'
#' A boundary optimum usually means the search box, not the model, picked the
#' answer. `par` is the optimized value on the log scale (matching the optimizer).
#' @keywords internal
#' @md
.warn_search_boundary <- function(par, lower, upper, name = "parameter") {
  par <- as.numeric(par)
  lower <- as.numeric(lower)
  upper <- as.numeric(upper)
  name <- rep(name, length.out = length(par))
  for (i in seq_along(par)) {
    span <- log(upper[i]) - log(lower[i])
    tol <- 1e-6 + 1e-3 * span
    if (par[i] <= log(lower[i]) + tol) {
      warning("fitted ", name[i], " reached the lower search bound (",
              format(lower[i]), "); widen `lower` or revisit the model.",
              call. = FALSE)
    } else if (par[i] >= log(upper[i]) - tol) {
      warning("fitted ", name[i], " reached the upper search bound (",
              format(upper[i]), "); widen `upper` or revisit the model.",
              call. = FALSE)
    }
  }
  invisible(NULL)
}

#' Compile checked raster inputs into a compact interaction substrate
#'
#' Select finite positive-demand cells before extracting distances, avoiding a
#' full distance matrix solely to discard inactive rows. All-active inputs use
#' the direct matrix reader. Raw cell extraction preserves numeric travel-time
#' values, missing edges, layer order and the original cell index map. This does
#' not change the fitting support policy or evaluate omitted prediction cells.
#' The shared finalizer records this raster route's legacy missing-edge policy
#' and stores plain template geometry plus cell mapping for safe serialization.
#' @keywords internal
#' @md
.interaction_substrate <- function(demand, supply, distance,
                                   id_col = NULL, supply_cols = NULL) {
  .chck_raster_alignment(demand, distance[[1]], "demand", "distance")

  ids <- .allocation_ids(names(distance), "facility_ids")
  processed <- .help_process_supply(supply, id_col = id_col,
                                    supply_cols = supply_cols,
                                    weight_ids = ids)
  D <- terra::values(demand, mat = TRUE)
  if (ncol(D) != 1L) {
    stop("AE problems currently support one demand layer")
  }
  keep <- which(is.finite(D[, 1]) & D[, 1] > 0)
  Dist0 <- if (!length(keep)) {
    matrix(numeric(), nrow = 0L, ncol = length(ids), dimnames = list(NULL, ids))
  } else if (length(keep) == terra::ncell(distance)) {
    terra::values(distance, mat = TRUE)
  } else {
    terra::extract(distance, keep, raw = TRUE)
  }

  .finalize_allocation(D[, 1], rep(TRUE, nrow(D)), as.character(seq_len(nrow(D))),
    as.matrix(processed$values), ids, processed$cols, Dist0, keep,
    list(source = "raster", missing_demand = "legacy_positive_fit",
         missing_travel = "legacy_zero", kernel_domain = "legacy_zero"),
    spatial = list(template = demand, cell_index = seq_len(nrow(D))))
}

#' Construct an SAE model spec
#' @keywords internal
#' @md
.sae_spec <- function(family = c("gaussian", "exponential", "power"),
                      kappa = 1, beta = 20, eps = 1e-8) {
  family <- match.arg(family)
  .chck_positive_scalar(kappa, "kappa")
  .chck_positive_scalar(beta, "beta")
  .chck_positive_scalar(eps, "eps")
  list(
    model = "sae",
    family = family,
    deterministic = TRUE,
    output_axes = c(target = "facility", utilization = "facility",
                    serviceability = "facility", adequacy = "facility",
                    access = "origin", allocation = "origin_facility",
                    opportunity = "origin_facility"),
    kappa = as.numeric(kappa),
    beta = as.numeric(beta),
    eps = as.numeric(eps),
    theta = .decay_theta_contract()
  )
}

#' Construct an HAAE model spec
#' @keywords internal
#' @md
.haae_spec <- function(family = c("gaussian", "exponential", "power"),
                       kappa = 1, eps = 1e-8, a_min = 1e-6,
                       a_max = Inf) {
  family <- match.arg(family)
  .chck_positive_scalar(kappa, "kappa")
  .chck_positive_scalar(eps, "eps")
  .chck_positive_scalar(a_min, "a_min")
  list(
    model = "haae",
    family = family,
    deterministic = TRUE,
    output_axes = c(target = "facility", utilization = "facility",
                    attractiveness = "facility", ratio = "facility",
                    access = "origin", pooled = "origin",
                    allocation = "origin_facility", huff_share = "origin_facility",
                    opportunity = "origin_facility"),
    kappa = as.numeric(kappa),
    eps = as.numeric(eps),
    a_min = as.numeric(a_min),
    a_max = a_max,
    theta = .decay_theta_contract()
  )
}

#' Theta contract for the static Huff/CLM spec
#'
#' `fit_v0 = TRUE` promotes the outside-option mass `v0`, and `fit_beta = TRUE`
#' the supply elasticity `beta` (attractiveness `a_j = (kappa S_j)^beta`), from
#' constants to fitted parameters, estimated jointly in `(sigma, [v0], [beta])`.
#' The default `clm` allocation is the conditional logit (DEC-013).
#' Appended order is `sigma`, then
#' `v0`, then `beta`; all bounded `[0, Inf)` with positive search bounds from the
#' caller. A declared decay kernel replaces the bounds of `sigma` with its own.
#' @param fit_v0,fit_beta Whether `v0` and `beta` are fitted parameters.
#' @param sigma_lower,sigma_upper Inclusive bounds of `sigma`; the declaration's
#'   `lower` and `upper` for a declared decay kernel.
#' @keywords internal
#' @md
.huff_theta_contract <- function(fit_v0 = FALSE, fit_beta = FALSE,
                                 sigma_lower = 0, sigma_upper = Inf) {
  contract <- .decay_theta_contract()
  contract$lower <- sigma_lower
  contract$upper <- sigma_upper
  append_param <- function(contract, name) {
    list(
      names = c(contract$names, name),
      lower = c(contract$lower, 0),
      upper = c(contract$upper, Inf)
    )
  }
  if (isTRUE(fit_v0)) {
    contract <- append_param(contract, "v0")
  }
  if (isTRUE(fit_beta)) {
    contract <- append_param(contract, "beta")
  }
  contract
}

#' Construct a static Huff / aggregate-CLM model spec
#'
#' The no-state member of the ladder: attractiveness is exogenous
#' (`a_j = (kappa * S_j)^beta`), allocation is one Huff pass, and `v0` is the
#' optional outside-option mass (`0` = pure Huff). With `fit_v0` / `fit_beta`,
#' the outside option and/or the supply elasticity join the fitted theta;
#' otherwise they (and `kappa`) are constants. `allocation = "clm"` is the
#' conditional-logit share (DEC-013); `"huff_decay"` preserves the historical
#' extra decay factor used for one-pass HAAE comparisons.
#' @param kernel `NULL` for a built-in `family`, or a custom `spax_decay_kernel`
#'   declaration (see [decay_kernel()]). The declaration then names the family,
#'   its bounds become the bounds of `sigma`, and the spec carries it as
#'   `spec$kernel`; built-in specs have no `kernel` element.
#' @keywords internal
#' @md
.huff_spec <- function(family = c("gaussian", "exponential", "power"),
                       kappa = 1, beta = 1, v0 = 0,
                       fit_v0 = FALSE, fit_beta = FALSE,
                       allocation = c("clm", "huff_decay"),
                       kernel = NULL, grouped = NULL) {
  if (!is.null(grouped) && isTRUE(grouped$per_group_family)) {
    family <- "grouped"
  } else if (is.null(kernel)) {
    family <- match.arg(family)
  } else {
    family <- kernel$name
  }
  allocation <- match.arg(allocation)
  .chck_positive_scalar(kappa, "kappa")
  .chck_positive_scalar(beta, "beta")
  # Constants per group replace the scalar v0 of a grouped model.
  if (is.null(grouped) || is.null(grouped$v0_groups)) {
    .chck_nonnegative_scalar(v0, "v0")
  }
  if (!is.logical(fit_v0) || length(fit_v0) != 1L || is.na(fit_v0) ||
      !is.logical(fit_beta) || length(fit_beta) != 1L || is.na(fit_beta)) {
    stop("`fit_v0` and `fit_beta` must be TRUE or FALSE")
  }
  spec <- list(
    model = "huff",
    family = family,
    deterministic = TRUE,
    output_axes = c(target = "facility", utilization = "facility",
                    attractiveness = "facility", access = "origin",
                    outside_share = "origin", allocation = "origin_facility",
                    huff_share = "origin_facility", opportunity = "origin_facility",
                    if (allocation == "clm") c(flow = "origin_facility")),
    output_meanings = if (allocation == "clm") c(
      allocation = "inside_choice_probability",
      huff_share = "inside_choice_probability",
      access = "inside_probability_mass",
      outside_share = "outside_choice_probability",
      utilization = "expected_count", flow = "expected_count"
    ) else NULL,
    allocation = allocation,
    kappa = as.numeric(kappa),
    beta = as.numeric(beta),
    v0 = as.numeric(v0),
    fit_v0 = isTRUE(fit_v0),
    fit_beta = isTRUE(fit_beta),
    theta = if (is.null(grouped)) .huff_theta_contract(
      fit_v0, fit_beta,
      sigma_lower = if (is.null(kernel)) 0 else kernel$lower,
      sigma_upper = if (is.null(kernel)) Inf else kernel$upper
    ) else list()
  )
  # Appended only for a declared kernel: a NULL element inside list() would
  # change the built-in specs.
  if (!is.null(kernel)) spec$kernel <- kernel
  if (!is.null(grouped)) spec <- .huff_group_spec(spec, grouped)
  spec
}

# Grouped CLM models (SPAX-057) ------------------------------------------------

# The legacy routes (SAE, HAAE, huff_decay, the ratio response) have no grouped
# form: refuse a prepared substrate with demand groups.
.refuse_grouped_substrate <- function(what, substrate) {
  if (!is.null(substrate$group_ids)) {
    stop(what, " is not available for grouped models yet", call. = FALSE)
  }
  invisible(TRUE)
}

# The grouped part of a spec, appended after today's fields (and `kernel`):
# groups, by_group, families, fixed, v0_groups. Outputs: the pooled
# `opportunity` is not consistent with the pooled allocation and is omitted; the
# per-group outputs are added with their own axes. `grouped` is the list built
# by .clm_group_args().
.huff_group_spec <- function(spec, grouped) {
  axes <- spec$output_axes[names(spec$output_axes) != "opportunity"]
  spec$output_axes <- c(axes, utilization_by_group = "facility_group",
                        access_by_group = "origin_group",
                        outside_share_by_group = "origin_group")
  if (!is.null(spec$output_meanings)) {
    keep <- names(spec$output_meanings) != "opportunity"
    spec$output_meanings <- c(spec$output_meanings[keep],
      utilization_by_group = "expected_count",
      access_by_group = "inside_probability_mass",
      outside_share_by_group = "outside_choice_probability")
  }
  # c() keeps a NULL `v0_groups`, which `$<-` would drop.
  spec <- c(spec, list(groups = grouped$groups, by_group = grouped$by_group,
                       families = grouped$families, fixed = grouped$fixed,
                       v0_groups = grouped$v0_groups))
  spec$theta <- .huff_group_theta_contract(spec)
  spec
}

#' Theta contract of a grouped static CLM spec
#'
#' Names in order: `sigma` or `sigma.<g>` for every group (when `"sigma"` is in
#' `by_group`), then `v0` or `v0.<g>` when `v0` is fitted, then `beta` when it is
#' fitted; the parameters in `spec$fixed` are removed. The bounds of a sigma are
#' those of the declared kernel of its group (`[0, Inf)` for a built-in one); the
#' extra `group` field gives the group of each parameter (`NA` when shared).
#' @param spec A grouped spec with `groups`, `by_group`, `families`, `fixed`,
#'   `fit_v0` and `fit_beta`.
#' @keywords internal
#' @md
.huff_group_theta_contract <- function(spec) {
  groups <- spec$groups
  bounds <- function(f) {
    if (inherits(f, "spax_decay_kernel")) c(f$lower, f$upper) else c(0, Inf)
  }
  nms <- character()
  lower <- upper <- numeric()
  group <- character()
  add <- function(nm, lo, up, g) {
    nms <<- c(nms, nm)
    lower <<- c(lower, lo)
    upper <<- c(upper, up)
    group <<- c(group, g)
  }
  if ("sigma" %in% spec$by_group) {
    for (g in groups) {
      b <- bounds(spec$families[[g]])
      add(paste0("sigma.", g), b[1L], b[2L], g)
    }
  } else {
    b <- bounds(spec$families[[1L]])
    add("sigma", b[1L], b[2L], NA_character_)
  }
  if (isTRUE(spec$fit_v0)) {
    if ("v0" %in% spec$by_group) {
      for (g in groups) add(paste0("v0.", g), 0, Inf, g)
    } else {
      add("v0", 0, Inf, NA_character_)
    }
  }
  if (isTRUE(spec$fit_beta)) add("beta", 0, Inf, NA_character_)
  free <- !nms %in% names(spec$fixed)
  list(names = nms[free], lower = lower[free], upper = upper[free],
       group = group[free])
}

# The grouped arguments of clm_allocation(), checked once. `groups` are the
# group IDs of the prepared inputs. Returns the family string and declared
# kernel as for an ungrouped model (the family is "grouped" when it is given by
# group), `v0` for the spec and the `grouped` list for .huff_spec().
.clm_group_args <- function(groups, family, v0, fit_v0, fit_beta, by_group, fixed) {
  if (!is.character(by_group) || anyNA(by_group) || anyDuplicated(by_group) ||
      !all(by_group %in% c("sigma", "v0"))) {
    stop("`by_group` must be a subset of \"sigma\" and \"v0\"", call. = FALSE)
  }
  if ("v0" %in% by_group && !isTRUE(fit_v0)) {
    stop("a group-specific v0 (by_group = \"v0\") must be fitted: outside = \"fit\" ",
         "(fit_v0 = TRUE in clm_allocation())", call. = FALSE)
  }
  is_family <- function(e) {
    (is.character(e) && length(e) == 1L && !is.na(e)) || inherits(e, "spax_decay_kernel")
  }
  per <- .fca_per_group(family, groups, "family", element = is_family)
  if (is.null(per) && is.list(family) && !inherits(family, "spax_decay_kernel")) {
    stop("family: give one element per group: ", paste(groups, collapse = ", "),
         call. = FALSE)
  }
  kernel <- NULL
  if (is.null(per)) {
    decay <- .resolve_decay_family(family)
    kernel <- decay$kernel
    one <- if (is.null(kernel)) decay$family else kernel
    families <- stats::setNames(rep(list(one), length(groups)), groups)
    family_out <- decay$family
  } else {
    families <- lapply(per, function(e) {
      decay <- .resolve_decay_family(e)
      if (is.null(decay$kernel)) decay$family else decay$kernel
    })
    by_group <- union("sigma", by_group)
    family_out <- "grouped"
  }
  by_group <- c("sigma", "v0")[c("sigma", "v0") %in% by_group]
  v0_groups <- NULL
  per_v0 <- .fca_per_group(v0, groups, "v0", atomic = TRUE)
  if (!is.null(per_v0)) {
    if (isTRUE(fit_v0)) {
      stop("v0 given by group is held: use fit_v0 = FALSE, or fit_v0 = TRUE with ",
           "by_group = \"v0\" to fit one per group", call. = FALSE)
    }
    for (g in groups) {
      .chck_nonnegative_scalar(per_v0[[g]], paste0("v0 (group '", g, "')"))
    }
    v0_groups <- stats::setNames(as.numeric(per_v0), groups)
    v0 <- NA_real_
  }
  grouped <- list(groups = groups, by_group = by_group, families = families,
                  fixed = stats::setNames(numeric(), character()),
                  v0_groups = v0_groups,
                  per_group_family = !is.null(per))
  if (!is.null(fixed)) {
    grouped$fixed <- .clm_group_fixed(fixed, grouped, fit_v0, fit_beta)
  }
  list(family = family_out, kernel = kernel, v0 = v0, grouped = grouped)
}

# The `fixed` values of a grouped model, checked against the parameters it would
# otherwise fit, in the order of the theta contract.
.clm_group_fixed <- function(fixed, grouped, fit_v0, fit_beta) {
  if (!is.numeric(fixed) || !length(fixed) || is.null(names(fixed)) ||
      anyNA(names(fixed)) || any(!nzchar(names(fixed))) ||
      anyDuplicated(names(fixed)) || any(!is.finite(fixed))) {
    stop("`fixed` must be a named finite numeric vector with unique names", call. = FALSE)
  }
  all_params <- .huff_group_theta_contract(c(grouped, list(
    fit_v0 = fit_v0, fit_beta = fit_beta)))
  unknown <- setdiff(names(fixed), all_params$names)
  if (length(unknown)) {
    stop("`fixed` names must be among the parameters of the model (",
         paste(all_params$names, collapse = ", "), "); not: ",
         paste(unknown, collapse = ", "), call. = FALSE)
  }
  at <- match(names(fixed), all_params$names)
  positive <- startsWith(names(fixed), "sigma") | names(fixed) == "beta"
  if (any(fixed < all_params$lower[at] | fixed > all_params$upper[at]) ||
      any(positive & fixed <= 0)) {
    stop("`fixed` values are outside the parameter bounds (sigma and beta must be ",
         "positive, v0 nonnegative)", call. = FALSE)
  }
  if (length(fixed) == length(all_params$names)) {
    stop("`fixed` leaves no free parameter", call. = FALSE)
  }
  stats::setNames(as.numeric(fixed)[order(at)], all_params$names[sort(at)])
}

# Every declared (custom) kernel of a spec: the shared one, or those of the
# per-group families. Built-in kernels are strings and are not listed.
.allocation_kernels <- function(spec) {
  kernels <- list()
  shared <- spec[["kernel", exact = TRUE]]
  if (!is.null(shared)) kernels <- list(shared)
  for (f in spec[["families", exact = TRUE]]) {
    if (inherits(f, "spax_decay_kernel")) kernels[[length(kernels) + 1L]] <- f
  }
  kernels
}

# The fields that mark a grouped fit or equilibrium (appended only when grouped).
.group_marker_names <- c("groups", "by_group", "families", "fixed")
.group_markers <- function(spec) {
  list(groups = spec$groups, by_group = spec$by_group, families = spec$families,
       fixed = spec$fixed)
}

#' Create an AE inner problem object
#' @keywords internal
#' @md
.new_problem <- function(model, substrate, state, theta, bind, metadata = list()) {
  if (!is.function(bind)) {
    stop("`bind` must be a function")
  }
  structure(
    list(
      model = model,
      substrate = substrate,
      state = state,
      theta = theta,
      bind = bind,
      metadata = metadata
    ),
    class = c("ae_problem", paste0(model, "_problem"))
  )
}

#' Compile a model spec and substrate into problem closures
#' @keywords internal
#' @md
.compile_ae_map <- function(spec, substrate) {
  if (!is.list(spec) || is.null(spec$model)) {
    stop("`spec` must be a model spec")
  }
  switch(spec$model,
    "sae" = .compile_sae_map(spec, substrate),
    "haae" = .compile_haae_map(spec, substrate),
    "huff" = .compile_huff_map(spec, substrate),
    stop("unsupported AE model spec")
  )
}

#' Construct a private SAE problem
#'
#' `demand` can be checked `.prepare_allocation()` output; in that case omit raw
#' `supply`, `distance` and supply selectors. Raster arguments remain compatible.
#' @keywords internal
#' @md
.sae_problem <- function(demand, supply = NULL, distance = NULL,
                         family = c("gaussian", "exponential", "power"),
                         kappa = 1, beta = 20, eps = 1e-8,
                         id_col = NULL, supply_cols = NULL) {
  substrate <- .resolve_allocation(
    demand, supply, distance, id_col = id_col, supply_cols = supply_cols
  )
  if (ncol(substrate$S) != 1L) {
    stop("SAE currently supports one supply measure")
  }
  .refuse_grouped_substrate("SAE", substrate)
  spec <- .sae_spec(family = family, kappa = kappa, beta = beta, eps = eps)
  compiled <- .compile_ae_map(spec, substrate)
  .new_problem(
    model = "sae",
    substrate = substrate,
    state = list(
      name = "c",
      axis = "J",
      init = rep(1, length(substrate$facility_ids)),
      lower = 0,
      upper = 1
    ),
    theta = spec$theta,
    bind = compiled$bind,
    metadata = list(spec = spec)
  )
}

#' Construct a private HAAE problem
#'
#' `demand` can be checked `.prepare_allocation()` output; in that case omit raw
#' `supply`, `distance` and supply selectors. Raster arguments remain compatible.
#' @keywords internal
#' @md
.haae_problem <- function(demand, supply = NULL, distance = NULL,
                          family = c("gaussian", "exponential", "power"),
                          kappa = 1, eps = 1e-8, a_min = 1e-6,
                          a_max = Inf, id_col = NULL,
                          supply_cols = NULL) {
  substrate <- .resolve_allocation(
    demand, supply, distance, id_col = id_col, supply_cols = supply_cols
  )
  if (ncol(substrate$S) != 1L) {
    stop("HAAE currently supports one supply measure")
  }
  .refuse_grouped_substrate("HAAE", substrate)
  spec <- .haae_spec(family = family, kappa = kappa, eps = eps,
                     a_min = a_min, a_max = a_max)
  compiled <- .compile_ae_map(spec, substrate)
  .new_problem(
    model = "haae",
    substrate = substrate,
    state = list(
      name = "a",
      axis = "J",
      init = rep(1, length(substrate$facility_ids)),
      lower = a_min,
      upper = a_max
    ),
    theta = spec$theta,
    bind = compiled$bind,
    metadata = list(spec = spec)
  )
}

#' Construct a private static Huff / aggregate-CLM problem
#'
#' The `dT/dstate = 0` member of the ladder: the map is constant, the
#' equilibrium is reached in one pass, and the decay parameter enters only
#' through the allocation output (not the state target). It calibrates through
#' the shared NFXP runner unchanged.
#' `demand` can be checked `.prepare_allocation()` output; in that case omit raw
#' `supply`, `distance` and supply selectors. Raster arguments remain compatible.
#' @param kernel `NULL` for a built-in `family`, or a custom `spax_decay_kernel`
#'   declaration passed on to `.huff_spec()`.
#' @param grouped `NULL`, or the grouped arguments of `.clm_group_args()` for
#'   prepared inputs with demand groups.
#' @keywords internal
#' @md
.huff_problem <- function(demand, supply = NULL, distance = NULL,
                          family = c("gaussian", "exponential", "power"),
                          kappa = 1, beta = 1, v0 = 0,
                          fit_v0 = FALSE, fit_beta = FALSE,
                          id_col = NULL, supply_cols = NULL,
                          allocation = c("clm", "huff_decay"),
                          kernel = NULL, grouped = NULL) {
  substrate <- .resolve_allocation(
    demand, supply, distance, id_col = id_col, supply_cols = supply_cols
  )
  if (ncol(substrate$S) != 1L) {
    stop("static Huff currently supports one supply measure")
  }
  if (identical(match.arg(allocation), "huff_decay")) {
    .refuse_grouped_substrate("the huff_decay allocation", substrate)
  }
  if (is.null(grouped) && !is.null(substrate$group_ids)) {
    stop("prepared inputs with demand groups need clm_allocation()")
  }
  spec <- .huff_spec(family = family, kappa = kappa, beta = beta, v0 = v0,
                     fit_v0 = fit_v0, fit_beta = fit_beta,
                     allocation = match.arg(allocation), kernel = kernel,
                     grouped = grouped)
  compiled <- .compile_ae_map(spec, substrate)
  a_init <- .allocation_supply(substrate, kappa, beta)
  .new_problem(
    model = "huff",
    substrate = substrate,
    state = list(
      name = "a",
      axis = "J",
      init = a_init,
      lower = 0,
      upper = Inf
    ),
    theta = spec$theta,
    bind = compiled$bind,
    metadata = list(spec = spec)
  )
}

.compile_sae_map <- function(spec, substrate) {
  S <- as.vector(substrate$S[, 1])
  bind <- function(theta) {
    theta <- .coerce_problem_theta(theta, spec$theta)
    K <- .allocation_kernel(substrate, spec$family, theta[[spec$theta$names]])
    plan <- substrate
    plan$Kd_active <- K
    plan$S <- S
    plan$kappa_supply <- .allocation_supply(substrate, spec$kappa)
    class(plan) <- c("sae_compact_plan", "fca_compact_plan")

    eval <- function(x, full = FALSE) {
      state <- .sae_state(x, plan = plan, beta = spec$beta, eps = spec$eps)
      if (!full) {
        return(state$target)
      }
      state
    }
    list(
      map = function(x) eval(x, full = FALSE),
      outputs = function(x) eval(x, full = TRUE),
      jac_state = function(x) {
        .sae_jacobian_state(x, plan = plan, beta = spec$beta, eps = spec$eps)
      },
      theta = theta,
      plan = plan
    )
  }
  list(bind = bind)
}

.compile_haae_map <- function(spec, substrate) {
  S <- as.vector(substrate$S[, 1])
  bind <- function(theta) {
    theta <- .coerce_problem_theta(theta, spec$theta)
    K <- .allocation_kernel(substrate, spec$family, theta[[spec$theta$names]])
    plan <- substrate
    plan$Kd_active <- K
    plan$S <- S
    plan$kappa_supply <- .allocation_supply(substrate, spec$kappa)
    class(plan) <- c("haae_compact_plan", "fca_compact_plan")

    eval <- function(x, full = FALSE) {
      state <- .haae_state(
        x, plan = plan, eps = spec$eps, a_min = spec$a_min,
        a_max = spec$a_max
      )
      if (!full) {
        return(state$target)
      }
      state
    }
    list(
      map = function(x) eval(x, full = FALSE),
      outputs = function(x) eval(x, full = TRUE),
      jac_state = function(x) {
        .haae_jacobian_state(
          x, plan = plan, eps = spec$eps, a_min = spec$a_min,
          a_max = spec$a_max
        )
      },
      theta = theta,
      plan = plan
    )
  }
  list(bind = bind)
}

# The travel matrix of a group for the kernel: on rows where the group has no
# demand, cells that would fail the domain checks (missing, negative, and zero
# where the kernel is undefined at zero) carry no flow and become absent edges
# (Inf). Rows with positive group demand are left to the strict checks.
.group_clean_distance <- function(distance, positive, zero_ok) {
  rows <- which(!positive)
  if (!length(rows)) return(distance)
  sub <- distance[rows, , drop = FALSE]
  bad <- is.na(sub) | (!is.na(sub) & sub < 0) | (!zero_ok & !is.na(sub) & sub == 0)
  if (!any(bad)) return(distance)
  sub[bad] <- Inf
  distance[rows, ] <- sub
  # The rows whose travel was not usable: the group's own access and outside
  # share there are unknown, not "no access" (reported NA by group).
  attr(distance, "blanked_rows") <- rows[rowSums(bad) > 0]
  distance
}

# Grouped static CLM (SPAX-057): one kernel per group (its own family, sigma and
# travel), pooled outputs as the ungrouped ones on the total demand, per-group
# outputs on request. Everything that does not depend on theta is prepared here,
# once per model: the travel matrices cleaned for zero-demand rows, one kernel
# plan per distinct matrix, the rebuilt kernel functions.
.compile_huff_group_map <- function(spec, substrate) {
  S <- as.vector(substrate$S[, 1])
  n <- length(S)
  n_theta <- length(spec$theta$names)
  zero_state <- matrix(0, n, n)
  groups <- spec$groups
  G <- length(groups)
  D_groups <- substrate$D_groups
  if (is.null(D_groups) || !identical(colnames(D_groups), groups) ||
      !identical(names(substrate$distance_groups), groups)) {
    stop("a grouped model needs prepared inputs with the same demand groups")
  }
  info <- lapply(groups, function(g) {
    f <- spec$families[[g]]
    kernel <- if (inherits(f, "spax_decay_kernel")) f
    list(family = if (is.null(kernel)) f else kernel$name, kernel = kernel,
         fun = if (!is.null(kernel)) .decay_kernel_function(kernel))
  })
  clean <- lapply(seq_len(G), function(i) {
    zero_ok <- !(identical(info[[i]]$family, "power") ||
                   (!is.null(info[[i]]$kernel) && !isTRUE(info[[i]]$kernel$zero_distance)))
    .group_clean_distance(substrate$distance_groups[[i]], D_groups[, i] > 0, zero_ok)
  })
  # The group of the first travel matrix equal to the matrix of each group (a
  # shared matrix stays one object), and likewise for family and kernel.
  first_same <- function(same) {
    first <- seq_len(G)
    for (i in seq_len(G)[-1L]) {
      for (h in seq_len(i - 1L)) {
        if (first[h] == h && same(h, i)) {
          first[i] <- h
          break
        }
      }
    }
    first
  }
  blanked <- lapply(clean, function(d) attr(d, "blanked_rows"))
  clean <- lapply(clean, function(d) {
    attr(d, "blanked_rows") <- NULL
    d
  })
  dist_id <- first_same(function(h, i) identical(clean[[h]], clean[[i]]))
  family_id <- first_same(function(h, i) {
    identical(info[[h]]$family, info[[i]]$family) &&
      identical(info[[h]]$kernel, info[[i]]$kernel)
  })
  plans <- list()
  for (i in seq_len(G)) {
    if (!is.null(info[[i]]$kernel) && is.null(plans[[as.character(dist_id[i])]])) {
      plans[[as.character(dist_id[i])]] <- .decay_kernel_plan(clean[[i]])
    }
  }
  inputs <- lapply(seq_len(G), function(i) {
    list(distance_active = clean[[i]], input_policy = substrate$input_policy)
  })
  sigma_by <- "sigma" %in% spec$by_group
  v0_by <- "v0" %in% spec$by_group
  per_group <- c("utilization_by_group", "access_by_group", "outside_share_by_group")
  bind <- function(theta) {
    theta <- .coerce_problem_theta(theta, spec$theta)
    full <- c(theta, spec$fixed)
    pick <- function(base, by) {
      vapply(groups, function(g) {
        if (by) full[[paste0(base, ".", g)]] else full[[base]]
      }, numeric(1))
    }
    sigma <- pick("sigma", sigma_by)
    v0 <- if (isTRUE(spec$fit_v0)) {
      pick("v0", v0_by)
    } else if (!is.null(spec$v0_groups)) {
      spec$v0_groups[groups]
    } else {
      rep(spec$v0, G)
    }
    beta <- if (isTRUE(spec$fit_beta)) full[["beta"]] else spec$beta
    a <- .allocation_supply(substrate, spec$kappa, beta)
    kernels <- vector("list", G)
    for (i in seq_len(G)) {
      # An earlier group with the same travel, family and sigma has the same kernel.
      same <- which(dist_id[seq_len(i - 1L)] == dist_id[i] &
                      family_id[seq_len(i - 1L)] == family_id[i] &
                      sigma[seq_len(i - 1L)] == sigma[i])
      kernels[[i]] <- if (length(same)) kernels[[same[1L]]] else {
        .allocation_kernel(inputs[[i]], info[[i]]$family, sigma[[i]],
          kernel = info[[i]]$kernel, fun = info[[i]]$fun,
          plan = plans[[as.character(dist_id[i])]])
      }
    }
    state <- .clm_group_state(a, kernels, D_groups, v0)
    by <- state[per_group]
    state[per_group] <- NULL
    for (i in seq_len(G)) {
      rows <- blanked[[i]]
      if (length(rows)) {
        by$access_by_group[rows, i] <- NA_real_
        by$outside_share_by_group[rows, i] <- NA_real_
      }
    }
    plan <- substrate
    plan$S <- S
    plan$kappa_supply <- .allocation_supply(substrate, spec$kappa)
    class(plan) <- c("huff_compact_plan", "fca_compact_plan")
    list(
      map = function(x) state$target,
      outputs = function(x) state,
      outputs_requested = function(x, requested_outputs) {
        out <- state
        if ("flow" %in% requested_outputs) {
          out$flow <- .clm_flow(state$allocation, substrate$D_active)
        }
        for (name in intersect(per_group, requested_outputs)) out[[name]] <- by[[name]]
        out
      },
      state_independent_outputs = c("flow", per_group),
      clm_prediction = NULL,
      jac_state = function(x) {
        list(
          jac_state = zero_state,
          jac_utilization = zero_state,
          utilization = state$utilization,
          target = state$target
        )
      },
      jac_param = function(x) matrix(0, n, n_theta),
      theta = theta,
      plan = plan
    )
  }
  list(bind = bind)
}

.compile_huff_map <- function(spec, substrate) {
  if (!is.null(spec[["groups", exact = TRUE]])) {
    return(.compile_huff_group_map(spec, substrate))
  }
  S <- as.vector(substrate$S[, 1])
  n <- length(S)
  n_theta <- length(spec$theta$names)
  zero_state <- matrix(0, n, n)
  # A declared kernel is rebuilt from its stored code once per model, outside
  # the bind closure that runs inside the optimizer.
  kernel <- spec[["kernel", exact = TRUE]]
  kernel_fun <- if (!is.null(kernel)) .decay_kernel_function(kernel)
  # Which travel entries are finite does not change with the parameters: record it once.
  kernel_plan <- if (!is.null(kernel)) .decay_kernel_plan(substrate$distance_active)
  # Corrected CLM models declare a prediction snapshot for the built-in kernels
  # and for a declared kernel (kept in the snapshot specification as plain data);
  # the internal no-decay family never has one.
  has_snapshot <- spec$allocation == "clm" && !identical(spec$family, "none")
  bind <- function(theta) {
    theta <- .coerce_problem_theta(theta, spec$theta)
    K <- .allocation_kernel(substrate, spec$family, theta[["sigma"]],
                            kernel = kernel, fun = kernel_fun, plan = kernel_plan)
    v0 <- if (isTRUE(spec$fit_v0)) theta[["v0"]] else spec$v0
    beta <- if (isTRUE(spec$fit_beta)) theta[["beta"]] else spec$beta
    a <- .allocation_supply(substrate, spec$kappa, beta)
    plan <- substrate
    plan$Kd_active <- K
    plan$S <- S
    plan$kappa_supply <- .allocation_supply(substrate, spec$kappa)
    class(plan) <- c("huff_compact_plan", "fca_compact_plan")

    # State-independent: the full state is fixed by theta (kernel, outside
    # option, supply elasticity) and the exogenous attractiveness, so compute
    # it once at bind time. Optional flow is materialized only when requested.
    state <- .huff_state(a, plan = plan, v0 = v0, allocation = spec$allocation)
    list(
      map = function(x) state$target,
      outputs = function(x) state,
      outputs_requested = if (spec$allocation == "clm") function(x, requested_outputs) {
        out <- state
        if ("flow" %in% requested_outputs) {
          out$flow <- .clm_flow(state$allocation, substrate$D_active)
        }
        out
      } else NULL,
      state_independent_outputs = if (spec$allocation == "clm") "flow" else NULL,
      clm_prediction = if (has_snapshot) list(
        spec = .clm_prediction_spec(spec), theta = theta
      ) else NULL,
      jac_state = function(x) {
        list(
          jac_state = zero_state,
          jac_utilization = zero_state,
          utilization = state$utilization,
          target = state$target
        )
      },
      jac_param = function(x) matrix(0, n, n_theta),
      theta = theta,
      plan = plan
    )
  }
  list(bind = bind)
}

#' Bind theta once and return the state-only inner-loop step
#'
#' Providers keep the required `outputs(x)` callback. An optional
#' `outputs_requested(x, requested_outputs)` callback adds requested outputs
#' while retaining `target` and `utilization`. Without it, requests read the
#' legacy outputs. `state_independent_outputs` explicitly names outputs whose
#' partial derivative with respect to state is zero for every valid state and
#' theta. Only these outputs bypass implicit state differentiation; the promise
#' is the provider's responsibility and is never inferred from a model name.
#' @keywords internal
#' @md
.bind_theta <- function(problem, theta) {
  .chck_class(problem, "ae_problem", "problem")
  bound <- problem$bind(theta)
  if (!is.function(bound$map)) {
    stop("bound AE step must provide a `map` function")
  }
  if (!is.function(bound[["outputs", exact = TRUE]])) {
    stop("bound AE step must provide an `outputs` function")
  }
  .validate_output_capabilities(bound, problem$metadata$spec$output_axes)
  structure(bound, class = c("ae_bound_step", paste0(problem$model, "_bound_step")))
}

#' Evaluate rich outputs for a problem at arbitrary theta and state
#' @param requested_outputs Optional output names to materialize. `NULL` keeps
#'   the provider's existing default outputs.
#' @keywords internal
#' @md
.problem_outputs_at <- function(problem, theta, state, requested_outputs = NULL) {
  .chck_class(problem, "ae_problem", "problem")
  .validate_problem_state(problem, state)
  .validate_ae_outputs(.problem_step_outputs(
    .bind_theta(problem, theta), state, requested_outputs
  ))
}

#' List model outputs that can be rewrapped onto the demand-cell surface
#' @keywords internal
#' @md
.problem_available_surfaces <- function(problem, outputs) {
  .chck_class(problem, "ae_problem", "problem")
  if (!is.list(outputs)) {
    stop("`outputs` must be a list")
  }
  active_n <- length(problem$substrate$demand_kept_index)
  .surface_output_names(outputs, active_n, problem$metadata$spec$output_axes)
}

#' Rewrap one origin-side problem output onto the demand raster template
#' @keywords internal
#' @md
.problem_output_surface <- function(problem, theta, state, output) {
  .chck_class(problem, "ae_problem", "problem")
  if (!is.character(output) || length(output) != 1L || is.na(output) || output == "") {
    stop("`output` must be a length-one character value")
  }
  outputs <- .problem_outputs_at(problem, theta = theta, state = state,
                                 requested_outputs = output)
  meta <- .problem_surface_meta(problem)
  .rewrap_problem_surface(
    outputs = outputs,
    output = output,
    template = meta$template,
    kept_cell_index = .allocation_cell_index(meta),
    output_axes = problem$metadata$spec$output_axes
  )
}

#' Solve an AE problem at fixed theta
#'
#' Successful built-in corrected CLM evaluations attach a plain O(J) prediction
#' snapshot containing the actual specification, theta, aligned supply/load and
#' declared input policy/units. Requested-location prediction uses this record
#' after serialization without retaining the problem or original travel matrix.
#' A model with a declared decay kernel (see [decay_kernel()]) attaches the same
#' record with the declaration, which is plain data, appended to the
#' specification; the record then has a separate schema version.
#' @param requested_outputs Optional output names to materialize in addition to
#'   the provider's default outputs. `NULL` retains the legacy output callback.
#' @keywords internal
#' @md
.solve_problem <- function(problem, theta, x0 = NULL, lambda = 1,
                           tol = 1e-8, max_iter = 1000,
                           norm = c("max", "l2"),
                           keep_history = TRUE, warn = TRUE,
                           check = TRUE, diagnostics = TRUE,
                           keep_state_history = FALSE, requested_outputs = NULL) {
  .chck_class(problem, "ae_problem", "problem")
  step <- .bind_theta(problem, theta)
  if (is.null(x0)) {
    x0 <- problem$state$init
  }
  .validate_problem_state(problem, x0)
  fit <- solve_equilibrium(
    map = step$map,
    x0 = x0,
    lambda = lambda,
    tol = tol,
    max_iter = max_iter,
    norm = norm,
    keep_history = keep_history,
    warn = warn,
    check = check,
    keep_state_history = keep_state_history
  )
  fit$outputs <- .validate_ae_outputs(.problem_step_outputs(
    step, fit$x_star, requested_outputs
  ))
  fit$utilization <- fit$outputs$utilization
  fit$prediction_snapshot <- .new_clm_prediction_snapshot(problem, step, fit)
  if (!is.null(fit$prediction_snapshot)) {
    fit$evaluated_clm <- fit$prediction_snapshot[c("spec", "theta", "facility_ids", "supply")]
  }
  if (!is.null(problem$metadata$spec[["groups", exact = TRUE]])) {
    # A grouped equilibrium has no prediction snapshot; the markers make the
    # objects that need one refuse it.
    fit[c(.group_marker_names, "prediction_snapshot")] <- c(
      .group_markers(problem$metadata$spec), list(NULL))
  }
  if (isTRUE(diagnostics) && is.function(step$jac_state)) {
    rho <- tryCatch({
      jac <- step$jac_state(fit$x_star)
      if (is.list(jac)) {
        jac <- jac$jac_state
      }
      spectral_radius(jac)
    }, error = function(e) NA_real_)
    fit$spectral_radius <- rho
    fit$contraction <- if (is.na(rho)) NA else rho < 1
  }
  fit
}

#' Fit a private AE problem by nested fixed point
#'
#' `observed` may be a numeric vector (e.g. facility marginals) or a numeric
#' matrix (e.g. origin-by-facility flows) matching the shape of the requested
#' `output`; the state axis stays whatever the problem declares. `NA` cells in
#' `observed` are treated as unobserved (e.g. suppressed flow cells) and are
#' excluded from the loss and its gradient.
#' Named observation axes are matched to declared provider IDs; unnamed axes
#' retain positional behavior. Callbacks receive canonical target order, so
#' observation-indexed `loss_args` must already use that order. Saved
#' `target_alignment` records the canonical IDs and the input permutations.
#' Unannotated custom providers keep unnamed positional fitting, without
#' assuming that every fitted vector has facility identity.
#' Corrected CLM supports requested `flow = D_active * allocation`, interpreted
#' as expected counts in the caller's demand unit and period. Default fits do
#' not save flow; accessors retrieve only outputs saved by the fit. Requesting
#' flow does not select a loss: Poisson loss remains an explicit caller choice,
#' and fractional expected counts are allowed.
#' Probability and expected-count meanings require a valid choice denominator.
#' When `v0 = 0` and an origin has zero opportunity at every facility, the
#' existing convention returns zero inside allocation, outside share and flow.
#' This row is not a normalized choice distribution; its missing mass is not
#' automatically outside participation or unmet need.
#'
#' With `reuse = TRUE`, providers declaring `spec$deterministic = TRUE` may
#' reuse a successful objective's equilibrium for the next gradient request
#' at exactly the same log parameters. This single-use entry belongs only to
#' this fitting call; every new objective invalidates it. The gradient is
#' evaluated at the objective's accepted state rather than refining that state
#' by another solve. Loss and gradient callbacks still run on every request.
#' Final history/diagnostics always use a fresh solve. Built-in providers declare
#' determinism; custom providers remain uncached unless they explicitly do so.
#' Inputs, provider behavior and controls must remain fixed during fitting;
#' use `reuse = FALSE` for stateful/mutating providers or callbacks that alter
#' model behavior. This is not a persistent cache across targets or scenarios.
#' @keywords internal
#' @md
.fit_problem_nfxp <- function(problem, observed, init, lower, upper,
                              output = "utilization",
                              loss = .weighted_sse_loss,
                              loss_grad = .weighted_sse_gradient,
                              loss_args = list(eta = 1),
                              lambda = 1, tol = 1e-8, max_iter = 1000,
                              norm = c("max", "l2"),
                              control = list(maxit = 25),
                              check = FALSE, penalty = 1e12,
                              gradient = FALSE, fd_eps = 1e-6,
                              reuse = TRUE) {
  .chck_class(problem, "ae_problem", "problem")
  if (!is.function(loss)) {
    stop("`loss` must be a function")
  }
  if (isTRUE(gradient) && !is.function(loss_grad)) {
    stop("`loss_grad` must be a function when `gradient = TRUE`")
  }
  if (!is.list(loss_args)) {
    stop("`loss_args` must be a list")
  }
  if (!is.character(output) || length(output) != 1L || is.na(output) || output == "") {
    stop("`output` must be a length-one character value")
  }
  target <- .bind_problem_target(problem, observed, output)
  .chck_positive_scalar(penalty, "penalty")
  .chck_positive_scalar(fd_eps, "fd_eps")
  norm <- match.arg(norm)
  gradient <- isTRUE(gradient)
  if (!is.logical(reuse) || length(reuse) != 1L || is.na(reuse)) {
    stop("`reuse` must be TRUE or FALSE")
  }
  reuse <- reuse && gradient && isTRUE(problem$metadata$spec$deterministic)

  expected <- problem$theta$names
  init <- .coerce_fit_theta(init, expected, "init")
  lower <- .coerce_fit_theta(lower, expected, "lower")
  upper <- .coerce_fit_theta(upper, expected, "upper")
  .coerce_problem_theta(init, problem$theta)
  .coerce_problem_theta(lower, problem$theta)
  .coerce_problem_theta(upper, problem$theta)
  if (any(init <= 0 | lower <= 0 | upper <= 0)) {
    stop("`init`, `lower`, and `upper` must be positive for log-scale optimization")
  }
  if (any(lower >= upper)) {
    stop("`lower` must be less than `upper`")
  }
  if (any(init < lower | init > upper)) {
    stop("`init` must be inside [`lower`, `upper`]")
  }

  observed_values <- target$values
  warm <- NULL
  paired <- NULL
  reused_solves <- 0L
  evaluate <- function(log_theta, keep_history = FALSE, warn = FALSE,
                       diagnostics = FALSE, update_warm = TRUE, solved = NULL) {
    # exp(log(x)) can round just outside x: keep trial values inside the
    # parameter contract, so a finite declared bound can be reached.
    theta <- stats::setNames(pmin(pmax(exp(log_theta), problem$theta$lower),
                                  problem$theta$upper), expected)
    x0 <- if (.state_within_problem_bounds(problem, warm)) warm else NULL
    fit <- if (!is.null(solved)) solved else .solve_problem(
      problem,
      theta = theta,
      x0 = x0,
      lambda = lambda,
      tol = tol,
      max_iter = max_iter,
      norm = norm,
      keep_history = keep_history,
      warn = warn,
      check = check,
      diagnostics = diagnostics,
      requested_outputs = output
    )
    if (!isTRUE(fit$converged)) {
      return(NULL)
    }
    predicted_full <- .problem_fit_output(fit, output, required = FALSE)
    if (is.null(predicted_full) || !.fit_target_shape_ok(predicted_full, target)) {
      return(NULL)
    }
    predicted <- as.numeric(predicted_full)[target$mask]
    value <- do.call(
      loss,
      c(list(predicted = predicted, observed = observed_values), loss_args)
    )
    if (!is.numeric(value) || length(value) != 1L || !is.finite(value)) {
      return(NULL)
    }
    if (isTRUE(update_warm)) {
      warm <<- fit$x_star
    }
    list(theta = theta, fit = fit, predicted = predicted, loss = as.numeric(value))
  }

  objective <- function(log_theta) {
    # Release the old rich outputs before allocating a new parameter point.
    paired <<- NULL
    tryCatch({
      evaluated <- evaluate(log_theta, keep_history = FALSE, warn = FALSE,
                            diagnostics = FALSE, update_warm = TRUE)
      if (is.null(evaluated)) {
        return(penalty)
      }
      if (reuse) paired <<- list(log_theta = as.numeric(log_theta), fit = evaluated$fit)
      evaluated$loss
    }, error = function(e) penalty)
  }

  objective_grad <- NULL
  if (gradient) {
    objective_grad <- function(log_theta) {
      solved <- NULL
      if (!is.null(paired) && identical(as.numeric(log_theta), paired$log_theta) &&
          identical(warm, paired$fit$x_star)) {
        solved <- paired$fit
        reused_solves <<- reused_solves + 1L
      }
      # Consume or invalidate the entry even when the gradient falls back.
      paired <<- NULL
      tryCatch({
        evaluated <- evaluate(log_theta, keep_history = FALSE, warn = FALSE,
                              diagnostics = FALSE, update_warm = FALSE, solved = solved)
        if (is.null(evaluated)) {
          return(.nfxp_fd_log_gradient(objective, log_theta, log(lower), log(upper),
                                       fd_eps = fd_eps))
        }
        sensitivity <- .problem_output_sensitivity(
          problem = problem,
          theta = evaluated$theta,
          x = evaluated$fit$x_star,
          output = output,
          fd_eps = fd_eps
        )
        if (nrow(sensitivity) != target$length) {
          return(.nfxp_fd_log_gradient(objective, log_theta, log(lower), log(upper),
                                       fd_eps = fd_eps))
        }
        sensitivity <- sensitivity[target$mask, , drop = FALSE]
        grad <- do.call(
          loss_grad,
          c(
            list(
              predicted = evaluated$predicted,
              observed = observed_values,
              sensitivity = sensitivity
            ),
            loss_args
          )
        )
        if (!is.numeric(grad) || length(grad) != length(log_theta) ||
            any(!is.finite(grad))) {
          return(.nfxp_fd_log_gradient(objective, log_theta, log(lower), log(upper),
                                       fd_eps = fd_eps))
        }
        as.numeric(grad) * exp(log_theta)
      }, error = function(e) {
        .nfxp_fd_log_gradient(objective, log_theta, log(lower), log(upper),
                              fd_eps = fd_eps)
      })
    }
  }

  # proc.time(), not system.time(): its default gcFirst = TRUE would force a
  # full garbage collection before every fit (SPAX-037 timer rule).
  started <- proc.time()[["elapsed"]]
  opt <- stats::optim(
    par = log(init),
    fn = objective,
    gr = objective_grad,
    method = "L-BFGS-B",
    lower = log(lower),
    upper = log(upper),
    control = control
  )
  elapsed <- proc.time()[["elapsed"]] - started

  # The estimate stays inside the search bounds (and so inside the contract),
  # also when exp(log(bound)) rounds outside them.
  theta_hat <- stats::setNames(pmin(pmax(exp(opt$par), lower), upper), expected)
  .warn_search_boundary(opt$par, lower, upper, expected)
  paired <- NULL
  final <- .solve_problem(
    problem,
    theta = theta_hat,
    x0 = if (.state_within_problem_bounds(problem, warm)) warm else NULL,
    lambda = lambda,
    tol = tol,
    max_iter = max_iter,
    norm = norm,
    keep_history = TRUE,
    warn = TRUE,
    check = check,
    requested_outputs = output
  )
  predicted_full <- .problem_fit_output(final, output, required = TRUE)
  .check_fit_target_shape(predicted_full, target, output)
  predicted <- as.numeric(predicted_full)[target$mask]
  loss_value <- do.call(
    loss,
    c(list(predicted = predicted, observed = observed_values), loss_args)
  )
  if (!is.numeric(loss_value) || length(loss_value) != 1L || !is.finite(loss_value)) {
    stop("final loss must be a finite numeric scalar")
  }
  predicted_store <- .name_bound_target(predicted_full, target)
  observed_store <- .name_bound_target(target$observed, target)
  state_name <- problem$state$name
  state <- .name_problem_vector(final$x_star, problem$substrate$facility_ids)

  fit <- structure(
    list(
      model = problem$model,
      family = problem$metadata$spec$family,
      allocation_form = problem$metadata$spec$allocation,
      theta_hat = theta_hat,
      theta = theta_hat,
      loss = as.numeric(loss_value),
      output = output,
      loss_fn = loss,
      loss_args = loss_args,
      gradient = gradient,
      evaluation_reuse = list(enabled = reuse, reused_solves = reused_solves),
      convergence = opt$convergence,
      message = opt$message,
      seconds = unname(elapsed),
      observed = observed_store,
      predicted = predicted_store,
      target_dim = target$dim,
      n_observed = target$n_observed,
      target_mask = if (all(target$mask)) NULL else target$mask,
      target_alignment = target$alignment,
      state = state,
      state_name = state_name,
      outputs = final$outputs,
      output_axes = problem$metadata$spec$output_axes,
      output_meanings = problem$metadata$spec$output_meanings,
      prediction_snapshot = final$prediction_snapshot,
      coverage_meta = list(
        demand = problem$substrate$D_active,
        supply = problem$substrate$S,
        facility_ids = problem$substrate$facility_ids,
        input_metadata = problem$substrate$input_metadata,
        input_policy = problem$substrate$input_policy
      ),
      surface_meta = .problem_surface_meta(problem),
      equilibrium = final,
      spectral_radius = final$spectral_radius,
      optim = opt
    ),
    class = c("ae_problem_nfxp_fit", paste0(problem$model, "_problem_nfxp_fit"))
  )
  if (!is.null(problem$metadata$spec[["groups", exact = TRUE]])) {
    # prediction_snapshot is already NULL: a grouped evaluation has none.
    fit[.group_marker_names] <- .group_markers(problem$metadata$spec)
  }
  fit
}

#' Compare calibrated AE problem fits on an identical target and loss
#'
#' Builds a tidy comparison table over fits of *different models calibrated
#' against the same data*: same output name, same observed values and
#' observation mask, same loss arguments (and the same loss function when the
#' fits carry one). Reports fit quality and stability diagnostics side by
#' side; it does not pick a winner.
#' Canonical observation identities must also agree. This helper does not
#' reorder independently prepared fits. Legacy fits without identity metadata
#' can still be compared to each other, but not to fits with declared identity.
#' @keywords internal
#' @md
.compare_problem_fits <- function(fits) {
  if (!is.list(fits) || length(fits) < 2L) {
    stop("`fits` must be a list of at least two AE problem fits")
  }
  if (is.null(names(fits)) || any(!nzchar(names(fits)))) {
    names(fits) <- vapply(fits, function(f) as.character(f$model), character(1))
  }
  if (anyDuplicated(names(fits)) > 0L) {
    stop("`fits` must have unique labels (name the list entries explicitly)")
  }
  for (f in fits) {
    .chck_class(f, "ae_problem_nfxp_fit", "each element of `fits`")
  }

  ref <- fits[[1L]]
  ref_obs <- as.numeric(ref$observed)
  ref_label <- names(fits)[[1L]]
  for (i in seq_along(fits)[-1L]) {
    f <- fits[[i]]
    label <- names(fits)[[i]]
    if (!identical(f$output, ref$output)) {
      stop("fits `", ref_label, "` and `", label,
           "` target different outputs (`", ref$output, "` vs `", f$output, "`)")
    }
    if (!identical(.fit_observation_identity(f), .fit_observation_identity(ref))) {
      stop("fits `", ref_label, "` and `", label,
           "` have different or unavailable canonical observation identities")
    }
    obs <- as.numeric(f$observed)
    if (!identical(dim(f$observed), dim(ref$observed)) ||
        length(obs) != length(ref_obs)) {
      stop("fits `", ref_label, "` and `", label,
           "` were calibrated against different target shapes")
    }
    if (!identical(is.na(obs), is.na(ref_obs))) {
      stop("fits `", ref_label, "` and `", label,
           "` use different observation masks")
    }
    if (!isTRUE(all.equal(obs[!is.na(obs)], ref_obs[!is.na(ref_obs)]))) {
      stop("fits `", ref_label, "` and `", label,
           "` were calibrated against different observed values")
    }
    if (!identical(f$loss_args, ref$loss_args)) {
      stop("fits `", ref_label, "` and `", label,
           "` use different `loss_args`")
    }
    if (is.function(f$loss_fn) && is.function(ref$loss_fn) &&
        !.same_function(f$loss_fn, ref$loss_fn)) {
      stop("fits `", ref_label, "` and `", label,
           "` use different loss functions")
    }
  }

  rows <- lapply(names(fits), function(label) {
    f <- fits[[label]]
    predicted <- as.numeric(f$predicted)
    observed <- as.numeric(f$observed)
    keep <- !is.na(observed)
    data.frame(
      label = label,
      model = f$model,
      family = if (is.null(f$family)) NA_character_ else f$family,
      allocation_form = if (is.null(f$allocation_form)) NA_character_ else f$allocation_form,
      n_params = length(f$theta),
      theta = paste(
        sprintf("%s=%.6g", names(f$theta), as.numeric(f$theta)),
        collapse = "; "
      ),
      loss = f$loss,
      correlation = stats::cor(predicted[keep], observed[keep]),
      rmse = sqrt(mean((predicted[keep] - observed[keep])^2)),
      n_observed = sum(keep),
      spectral_radius = if (is.null(f$spectral_radius)) NA_real_ else f$spectral_radius,
      solver_iters = f$equilibrium$iters,
      solver_converged = isTRUE(f$equilibrium$converged),
      optim_convergence = f$convergence,
      gradient = isTRUE(f$gradient),
      seconds = f$seconds,
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

#' Fit several AE problems against one shared target and compare them
#'
#' The nested static-vs-feedback question (DEC-012): fit every problem in the
#' ladder against the *same* observed target with the *same* loss, then put
#' the legs side by side. Comparability is guaranteed by construction; the
#' result reports, it does not decide. `init`/`lower`/`upper` are shared
#' across problems, or a named list keyed by problem label for per-problem
#' theta contracts. Solver/runner options in `...` are shared by all legs.
#' @keywords internal
#' @md
.fit_problem_ladder <- function(problems, observed, init, lower, upper,
                                output = "utilization",
                                loss = .weighted_sse_loss,
                                loss_grad = .weighted_sse_gradient,
                                loss_args = list(eta = 1), ...) {
  if (!is.list(problems) || length(problems) < 2L) {
    stop("`problems` must be a list of at least two AE problems")
  }
  if (is.null(names(problems)) || any(!nzchar(names(problems)))) {
    names(problems) <- vapply(
      problems,
      function(p) as.character(p$model),
      character(1)
    )
  }
  if (anyDuplicated(names(problems)) > 0L) {
    stop("`problems` must have unique labels (name the list entries explicitly)")
  }
  for (p in problems) {
    .chck_class(p, "ae_problem", "each element of `problems`")
  }
  pick <- function(arg, label, what) {
    if (is.list(arg)) {
      if (is.null(names(arg)) || !label %in% names(arg)) {
        stop("per-problem `", what, "` must be a named list covering problem `",
             label, "`")
      }
      return(arg[[label]])
    }
    arg
  }

  fits <- lapply(names(problems), function(label) {
    .fit_problem_nfxp(
      problem = problems[[label]],
      observed = observed,
      init = pick(init, label, "init"),
      lower = pick(lower, label, "lower"),
      upper = pick(upper, label, "upper"),
      output = output,
      loss = loss,
      loss_grad = loss_grad,
      loss_args = loss_args,
      ...
    )
  })
  names(fits) <- names(problems)
  list(fits = fits, table = .compare_problem_fits(fits))
}

#' Rewrap one final-fit origin-side output onto the demand raster template
#'
#' Declared axes prevent facility vectors being interpreted as surfaces when
#' their lengths happen to equal the origin count. Older/custom fits without
#' axis metadata retain legacy shape-based discovery; see `.fit_available_surfaces`.
#' @keywords internal
#' @md
.fit_output_surface <- function(fit, output) {
  .chck_class(fit, "ae_problem_nfxp_fit", "fit")
  if (!is.character(output) || length(output) != 1L || is.na(output) || output == "") {
    stop("`output` must be a length-one character value")
  }
  .rewrap_problem_surface(
    outputs = fit$outputs,
    output = output,
    template = fit$surface_meta$template,
    kept_cell_index = .allocation_cell_index(fit$surface_meta),
    output_axes = fit$output_axes
  )
}

#' Facility-level table for an AE calibration fit
#'
#' Always reports predicted facility utilization. Observations/residuals are
#' included only for utilization-vector targets, not origin or matrix targets.
#' @keywords internal
#' @md
.ae_fit_facility_table <- function(fit) {
  .chck_class(fit, "ae_problem_nfxp_fit", "fit")
  if (is.null(fit$target_dim) && identical(fit$output, "utilization")) {
    predicted <- fit$predicted
    observed <- fit$observed
  } else {
    # Other targets may have origin/edge axes; never relabel them as facilities.
    predicted <- .name_problem_vector(
      .coerce_numeric_vector(fit$outputs$utilization),
      names(fit$state)
    )
    observed <- NULL
  }
  n <- length(predicted)
  facility_id <- names(predicted)
  if (is.null(facility_id)) {
    facility_id <- as.character(seq_len(n))
  }
  out <- data.frame(
    facility_id = facility_id,
    predicted = as.numeric(predicted),
    state = as.numeric(fit$state),
    stringsAsFactors = FALSE
  )
  if (!is.null(observed) && length(observed) == n) {
    out$observed <- as.numeric(observed)
    out$residual <- out$predicted - out$observed
  }
  out
}

.problem_jac_state_matrix <- function(jac) {
  if (is.list(jac)) {
    jac <- jac$jac_state
  }
  .chck_numeric_matrix(jac, "jac_state")
  .coerce_numeric_matrix(jac)
}

.nfxp_fd_log_gradient <- function(objective, log_theta, lower, upper,
                                  fd_eps = 1e-6) {
  .chck_positive_scalar(fd_eps, "fd_eps")
  log_theta <- .coerce_numeric_vector(log_theta)
  lower <- .coerce_numeric_vector(lower)
  upper <- .coerce_numeric_vector(upper)
  grad <- numeric(length(log_theta))
  for (i in seq_along(log_theta)) {
    h <- as.numeric(fd_eps) * max(abs(log_theta[i]), 1)
    hi <- log_theta
    lo <- log_theta
    hi[i] <- min(log_theta[i] + h, upper[i])
    lo[i] <- max(log_theta[i] - h, lower[i])
    if (hi[i] > log_theta[i] && lo[i] < log_theta[i]) {
      grad[i] <- (objective(hi) - objective(lo)) / (hi[i] - lo[i])
    } else if (hi[i] > log_theta[i]) {
      grad[i] <- (objective(hi) - objective(log_theta)) / (hi[i] - log_theta[i])
    } else if (lo[i] < log_theta[i]) {
      grad[i] <- (objective(log_theta) - objective(lo)) / (log_theta[i] - lo[i])
    } else {
      grad[i] <- 0
    }
  }
  grad[!is.finite(grad)] <- 0
  grad
}

.problem_map_jac_param <- function(problem, theta, x, step = NULL,
                                   fd_eps = 1e-6) {
  if (is.null(step)) {
    step <- .bind_theta(problem, theta)
  }
  if (is.function(step$jac_param)) {
    jac <- step$jac_param(x)
    if (is.list(jac)) {
      jac <- jac$jac_param
    }
    .chck_numeric_matrix(jac, "jac_param")
    return(.coerce_numeric_matrix(jac))
  }
  fn <- function(theta_value) {
    .bind_theta(problem, theta_value)$map(x)
  }
  bounds <- .problem_theta_bounds(problem, theta)
  fd_jacobian(fn, theta, eps = fd_eps, check = FALSE,
              lower = bounds$lower, upper = bounds$upper)
}

# The inclusive bounds of the theta contract in the order of the names of
# `theta` (positional when it has none), so that a finite-difference step never
# leaves them. Unbounded when the contract has none or cannot be aligned.
.problem_theta_bounds <- function(problem, theta) {
  contract <- problem$theta
  n <- length(contract$names)
  bound <- function(value, default) {
    if (is.null(value)) rep(default, n) else rep_len(as.numeric(value), n)
  }
  lower <- bound(contract$lower, -Inf)
  upper <- bound(contract$upper, Inf)
  at <- if (!is.null(names(theta)) && !anyNA(match(names(theta), contract$names))) {
    match(names(theta), contract$names)
  } else {
    seq_along(theta)
  }
  if (!length(theta) || length(at) != length(theta) || anyNA(at) || any(at > n)) {
    return(list(lower = -Inf, upper = Inf))
  }
  list(lower = lower[at], upper = upper[at])
}

.problem_output_jac_state <- function(step, x, output, fd_eps = 1e-6,
                                      jac_state_result = NULL) {
  if (is.function(step$jac_output_state)) {
    jac <- step$jac_output_state(x, output = output)
    .chck_numeric_matrix(jac, "jac_output_state")
    return(.coerce_numeric_matrix(jac))
  }
  if (is.null(jac_state_result) && is.function(step$jac_state)) {
    jac_state_result <- step$jac_state(x)
  }
  jac_name <- paste0("jac_", output)
  if (is.list(jac_state_result) && jac_name %in% names(jac_state_result)) {
    jac <- jac_state_result[[jac_name]]
    .chck_numeric_matrix(jac, jac_name)
    return(.coerce_numeric_matrix(jac))
  }
  fn <- function(x_value) {
    .problem_output_from_step(step, x_value, output, required = TRUE)
  }
  fd_jacobian(fn, x, eps = fd_eps, check = FALSE)
}

.problem_output_jac_param <- function(problem, theta, x, output,
                                      fd_eps = 1e-6, step = NULL) {
  if (is.null(step)) step <- .bind_theta(problem, theta)
  if (is.function(step$jac_output_param)) {
    jac <- step$jac_output_param(x, output = output)
    if (!is.null(jac)) {
      .chck_numeric_matrix(jac, "jac_output_param")
      if (ncol(jac) != length(theta)) stop("output parameter Jacobian has wrong columns")
      return(.coerce_numeric_matrix(jac))
    }
  }
  fn <- function(theta_value) {
    step <- .bind_theta(problem, theta_value)
    .problem_output_from_step(step, x, output, required = TRUE)
  }
  bounds <- .problem_theta_bounds(problem, theta)
  fd_jacobian(fn, theta, eps = fd_eps, check = FALSE,
              lower = bounds$lower, upper = bounds$upper)
}

.problem_output_sensitivity <- function(problem, theta, x,
                                        output = "utilization",
                                        fd_eps = 1e-6) {
  step <- .bind_theta(problem, theta)
  if (output %in% step[["state_independent_outputs", exact = TRUE]]) {
    # The provider explicitly promises no dependence on the solved state.
    # Avoid constructing an output-by-state matrix of structural zeros.
    return(.problem_output_jac_param(problem, theta, x, output, fd_eps))
  }
  jac_state_result <- if (is.function(step$jac_state)) step$jac_state(x) else NULL
  jac_state <- if (is.null(jac_state_result)) {
    fd_jacobian_state(step$map, x, eps = fd_eps, check = FALSE)
  } else {
    .problem_jac_state_matrix(jac_state_result)
  }
  jac_param <- .problem_map_jac_param(problem, theta, x, step = step,
                                      fd_eps = fd_eps)
  state_sensitivity <- implicit_gradient(jac_state, jac_param)
  output_jac_state <- .problem_output_jac_state(
    step, x, output = output, fd_eps = fd_eps,
    jac_state_result = jac_state_result
  )
  output_jac_param <- .problem_output_jac_param(
    problem, theta, x, output = output, fd_eps = fd_eps, step = step
  )
  output_jac_state %*% state_sensitivity + output_jac_param
}

.problem_output_from_step <- function(step, x, output, required = TRUE) {
  outputs <- .problem_step_outputs(step, x, requested_outputs = output,
                                   required = required)
  if (!is.list(outputs) || !output %in% names(outputs)) {
    if (required) {
      stop("problem outputs do not include requested output `", output, "`")
    }
    return(NULL)
  }
  value <- outputs[[output]]
  ok <- tryCatch({
    .chck_target_numeric(value, output)
    TRUE
  }, error = function(e) {
    if (required) {
      stop(e$message, call. = FALSE)
    }
    FALSE
  })
  if (!ok) {
    return(NULL)
  }
  # Flattened (column-major) so FD Jacobians stack matrix outputs as rows.
  as.numeric(value)
}

.validate_problem_state <- function(problem, x) {
  .chck_numeric_vector(x, "x0")
  x <- .coerce_numeric_vector(x)
  if (length(x) != length(problem$state$init)) {
    stop("`x0` must match the problem state length")
  }
  if (any(x < problem$state$lower | x > problem$state$upper)) {
    stop("`x0` is outside the problem state bounds")
  }
  invisible(TRUE)
}

.state_within_problem_bounds <- function(problem, x) {
  if (is.null(x)) {
    return(FALSE)
  }
  x <- .coerce_numeric_vector(x)
  length(x) == length(problem$state$init) &&
    all(is.finite(x)) &&
    all(x >= problem$state$lower & x <= problem$state$upper)
}

.coerce_fit_theta <- function(x, expected, name) {
  .chck_numeric_vector(x, name)
  x <- .coerce_numeric_vector(x)
  if (is.null(names(x)) || any(names(x) == "")) {
    if (length(x) != length(expected)) {
      stop("unnamed `", name, "` must match the theta contract length")
    }
    names(x) <- expected
  }
  missing <- setdiff(expected, names(x))
  if (length(missing) > 0) {
    stop("`", name, "` is missing required parameter(s): ",
         paste(missing, collapse = ", "))
  }
  x[expected]
}

#' Validate a calibration value as a finite numeric vector or matrix
#' @keywords internal
#' @md
.chck_target_numeric <- function(x, name = "Input") {
  .chck_is_numeric(x, name = name)
  if (!is.null(dim(x)) && length(dim(x)) != 2L) {
    stop(sprintf("%s must be a numeric vector or matrix", name))
  }
  if (any(!is.finite(x))) {
    stop(sprintf("%s must contain only finite values", name))
  }
  invisible(TRUE)
}

#' Describe a calibration target: shape, observation mask, observed values
#'
#' `NA` cells mark unobserved targets (e.g. HSAF flow suppression below 11
#' cases) and are excluded from the loss; predicted/observed comparison is
#' over flattened (column-major) values at the observed cells only.
#' @keywords internal
#' @md
.fit_target_meta <- function(observed, name = "observed") {
  .chck_is_numeric(observed, name = name)
  if (!is.null(dim(observed)) && length(dim(observed)) != 2L) {
    stop(sprintf("%s must be a numeric vector or matrix", name))
  }
  if (length(observed) == 0L) {
    stop(sprintf("%s must not be empty", name))
  }
  flat <- as.numeric(observed)
  mask <- !is.na(flat)
  if (!any(mask)) {
    stop(sprintf("%s must contain at least one non-missing value", name))
  }
  if (any(!is.finite(flat[mask]))) {
    stop(sprintf("%s non-missing values must be finite", name))
  }
  list(
    dim = dim(observed),
    length = length(flat),
    mask = mask,
    values = flat[mask],
    n_observed = sum(mask)
  )
}

#' Does a predicted output have the same shape as the calibration target?
#' @keywords internal
#' @md
.fit_target_shape_ok <- function(predicted, target) {
  if (!is.numeric(predicted)) {
    return(FALSE)
  }
  pdim <- dim(predicted)
  if (is.null(target$dim)) {
    return(is.null(pdim) && length(predicted) == target$length)
  }
  !is.null(pdim) && length(pdim) == 2L && all(pdim == target$dim)
}

#' Require shape agreement between a predicted output and the target
#' @keywords internal
#' @md
.check_fit_target_shape <- function(predicted, target, output) {
  if (.fit_target_shape_ok(predicted, target)) {
    return(invisible(TRUE))
  }
  describe <- function(d, n) {
    if (is.null(d)) paste0("length ", n) else paste0("dim ", paste(d, collapse = " x "))
  }
  stop("output `", output, "` (", describe(dim(predicted), length(predicted)),
       ") does not match `observed` (", describe(target$dim, target$length), ")")
}

.problem_fit_output <- function(fit, output, required = TRUE) {
  if (!is.list(fit$outputs) || !output %in% names(fit$outputs)) {
    if (required) {
      stop("problem outputs do not include requested output `", output, "`")
    }
    return(NULL)
  }
  value <- fit$outputs[[output]]
  ok <- tryCatch({
    .chck_target_numeric(value, output)
    TRUE
  }, error = function(e) {
    if (required) {
      stop(e$message, call. = FALSE)
    }
    FALSE
  })
  if (!ok) {
    return(NULL)
  }
  if (is.null(dim(value))) {
    .coerce_numeric_vector(value)
  } else {
    .coerce_numeric_matrix(value)
  }
}

.name_problem_vector <- function(x, ids) {
  if (!is.null(ids) && length(x) == length(ids)) {
    names(x) <- ids
  }
  x
}

.problem_surface_meta <- function(problem) {
  s <- problem$substrate
  list(
    template = s$template,
    demand_kept_index = s$demand_kept_index,
    origin_ids = if (!is.null(s$origin_ids)) s$origin_ids else as.character(s$demand_kept_index),
    cell_index = if (!is.null(s$cell_index)) s$cell_index[s$demand_kept_index] else NULL
  )
}

.rewrap_problem_surface <- function(outputs, output, template, kept_cell_index,
                                     output_axes = NULL) {
  if (is.null(template) || is.null(kept_cell_index)) {
    stop("surface metadata is not available for this AE object")
  }
  if (!is.list(outputs) || !output %in% names(outputs)) {
    stop("problem outputs do not include requested output `", output, "`")
  }

  active_n <- length(kept_cell_index)
  value <- outputs[[output]]
  surface_names <- .surface_output_names(outputs, active_n, output_axes)
  if (!output %in% surface_names) {
    msg <- paste0("output `", output, "` is not an origin-side surface")
    if (length(surface_names) > 0) {
      msg <- paste0(msg, "; available surfaces: ",
                    paste(surface_names, collapse = ", "))
    }
    stop(msg)
  }
  .chck_numeric_vector(value, output, finite = FALSE)
  if (any(!is.finite(value) & !is.na(value))) {
    stop(output, " must contain only finite or missing values")
  }
  out <- .rewrap_cells(.coerce_numeric_vector(value), .allocation_template(template), kept_cell_index)
  names(out) <- output
  out
}

.surface_output_names <- function(outputs, active_n, output_axes = NULL) {
  candidates <- names(Filter(
    function(value) {
      is.numeric(value) &&
        is.null(dim(value)) &&
        length(value) == active_n &&
        all(is.finite(value) | is.na(value))
    },
    outputs
  ))
  if (is.null(output_axes)) return(candidates)
  intersect(candidates, names(output_axes)[output_axes == "origin"])
}

.coerce_problem_theta <- function(theta, contract) {
  .chck_numeric_vector(theta, "theta")
  theta <- .coerce_numeric_vector(theta)
  expected <- contract$names
  if (is.null(names(theta)) || any(names(theta) == "")) {
    if (length(theta) != length(expected)) {
      stop("unnamed `theta` must match the theta contract length")
    }
    names(theta) <- expected
  }
  missing <- setdiff(expected, names(theta))
  if (length(missing) > 0) {
    stop("`theta` is missing required parameter(s): ",
         paste(missing, collapse = ", "))
  }
  theta <- theta[expected]
  if (any(theta < contract$lower | theta > contract$upper)) {
    stop("`theta` is outside the theta bounds")
  }
  theta
}
