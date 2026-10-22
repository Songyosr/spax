# Binding a model specification to prepared data -------------------------------

#' Bind a model specification to prepared inputs
#'
#' The private step inside `spax()` that turns a data-free `spax_allocation_spec` (see
#' [spec_clm()]) and checked `ae_prepared_inputs` into the existing `ae_problem`,
#' built through [clm_allocation()]. It checks the demand groups, translates the
#' specification into the arguments of `clm_allocation()`, decides whether the
#' model is fitted or evaluated and refuses a decay that is undefined at a zero
#' travel time.
#'
#' **Mode.** The mode is `"fit"` when any parameter is free (`"fit"` in its slot)
#' and `"evaluate"` when every parameter has a value. In fit mode the held values
#' of a model with demand groups go through `fixed`; an ungrouped model cannot
#' hold its sigma while something else is fitted. In evaluate mode `sigma` is
#' bound as a free parameter and its value is returned as `theta_given`, for
#' `evaluate_allocation()`; nothing goes into `fixed`.
#'
#' @param spec A `spax_allocation_spec` from [spec_clm()] or [spec_huff()].
#' @param data An `ae_prepared_inputs`, from [prep_data()].
#' @return A list: `problem` (the `ae_problem`), `mode` (`"fit"` or
#'   `"evaluate"`), `theta_given` (`NULL` in fit mode; in evaluate mode the
#'   named values of the free parameters, in the order of `problem$theta$names`)
#'   and `held`, a named numeric vector of the values that are neither fitted nor
#'   given: held `sigma` or `sigma.<group>` (fit mode only, since in evaluate mode
#'   sigma is in `theta_given`), `v0` or `v0.<group>` (`v0 = 0` for the Huff
#'   model), `beta` when it is a number and `kappa`.
#' @noRd
#' @md
.bind_spax_spec <- function(spec, data) {
  groups <- data$group_ids
  grouped <- !is.null(groups)
  decay <- spec$decay
  outside <- spec$outside
  clm <- identical(spec$kind, "clm")
  decay_list <- !inherits(decay, "spax_decay")
  outside_list <- is.list(outside)
  outside_by_group <- outside_list || (is.numeric(outside) && !is.null(names(outside)))

  # 1. Groups ------------------------------------------------------------------
  .bind_check_groups(spec, groups, decay_list, outside_by_group)

  # 2. Family: a built-in kernel as its name, a declared one as itself ----------
  kernel_family <- function(kernel) if (isTRUE(kernel$builtin)) kernel$name else kernel
  family <- if (decay_list) {
    lapply(decay[groups], function(d) kernel_family(d$kernel))
  } else {
    kernel_family(decay$kernel)
  }

  # 3. Status of each parameter: a number is held, "fit" is free. The held values
  # are named like the parameters; NA marks a free one.
  held_sigma <- if (decay_list) {
    stats::setNames(vapply(decay[groups], function(d) {
      if (is.numeric(d$sigma)) d$sigma else NA_real_
    }, numeric(1)), paste0("sigma.", groups))
  } else {
    c(sigma = if (is.numeric(decay$sigma)) decay$sigma else NA_real_)
  }
  # NULL (fitted v0), a number, or the values named by group in the order of the groups
  # (NA marks a group whose v0 is fitted).
  held_v0 <- if (!clm) {
    0
  } else if (identical(outside, "fit")) {
    NULL
  } else if (outside_by_group) {
    stats::setNames(vapply(outside[groups], function(v) {
      if (is.numeric(v)) as.numeric(v) else NA_real_
    }, numeric(1)), groups)
  } else {
    outside
  }
  # The held values by group as named parameters: v0.<group>.
  v0_named <- function(v) {
    keep <- !is.na(v)
    if (!any(keep)) return(numeric())
    stats::setNames(unname(v[keep]), paste0("v0.", names(v)[keep]))
  }
  sigma_free <- anyNA(held_sigma)
  v0_free <- clm && (is.null(held_v0) || anyNA(held_v0))
  beta_free <- identical(spec$beta, "fit")

  # 4. Mode --------------------------------------------------------------------
  mode <- if (sigma_free || v0_free || beta_free) "fit" else "evaluate"

  # 5. and 6. Arguments of clm_allocation() --------------------------------------
  by_group <- spec$by_group
  # A decay list gives each group its sigma; an outside list with a free weight, its v0.
  if (decay_list) by_group <- union(by_group, "sigma")
  fixed <- numeric()
  if (identical(mode, "fit")) {
    if (outside_list && v0_free) by_group <- union(by_group, "v0")
    if (!grouped && !sigma_free) {
      stop("in this version sigma can be held only when nothing else is fitted, ",
           "or with demand groups; give the decay sigma = \"fit\", or give beta and ",
           "outside values to evaluate at given parameters", call. = FALSE)
    }
    if (grouped) {
      fixed <- c(held_sigma[!is.na(held_sigma)],
                 if (outside_list && v0_free) v0_named(held_v0))
    }
  }
  # In evaluate mode sigma is bound free (its value is `theta_given`) and nothing is
  # fixed. `v0_free` is a fitted v0, also in a mixed list, whose numbers go in `fixed`.
  v0_arg <- if (v0_free) 0 else held_v0
  beta_arg <- if (beta_free) 1 else spec$beta

  # 7. D8: a kernel that is undefined at zero refuses a zero travel time ---------
  .bind_check_zero_travel(data, if (decay_list) decay[groups] else list(decay))

  # 8. Bind --------------------------------------------------------------------
  problem <- clm_allocation(data, family = family, kappa = spec$kappa, beta = beta_arg,
                            v0 = v0_arg, fit_v0 = v0_free, fit_beta = beta_free,
                            by_group = as.character(by_group),
                            fixed = if (length(fixed)) fixed else NULL)

  # 9. Result -------------------------------------------------------------------
  # `held` lists the values that are neither fitted nor given as theta.
  held <- c(if (identical(mode, "fit")) held_sigma[!is.na(held_sigma)],
            if (!is.null(held_v0)) {
              if (length(held_v0) == 1L && is.null(names(held_v0))) {
                c(v0 = held_v0)
              } else {
                v0_named(held_v0)
              }
            },
            if (!beta_free) c(beta = spec$beta),
            c(kappa = spec$kappa))
  theta_given <- if (identical(mode, "evaluate")) held_sigma[problem$theta$names]
  list(problem = problem, mode = mode, theta_given = theta_given, held = held)
}

# Step 1: demand groups. A decay list, an outside vector or list and a nonempty
# `by_group` need data with demand groups; with groups, the names of a decay list or
# an outside vector or list must be the groups of the data.
.bind_check_groups <- function(spec, groups, decay_list, outside_by_group) {
  if (is.null(groups)) {
    needs <- c(decay = decay_list, outside = outside_by_group,
               by_group = length(spec$by_group) > 0L)
    if (any(needs)) {
      stop(names(needs)[needs][1L], " needs data with demand groups (prep_data() ",
           "with demand as a named list by group)", call. = FALSE)
    }
    return(invisible(TRUE))
  }
  given <- list(decay = if (decay_list) names(spec$decay),
                outside = if (outside_by_group) names(spec$outside))
  for (arg in names(given)) {
    if (!is.null(given[[arg]]) && !setequal(given[[arg]], groups)) {
      stop(arg, " is given for groups ", paste(given[[arg]], collapse = ", "),
           " but the data has groups ", paste(groups, collapse = ", "), call. = FALSE)
    }
  }
  invisible(TRUE)
}

# Step 7 (D8): a decay with `zero_distance = FALSE` (power, or as declared) is
# undefined at a travel time of 0. Ungrouped data are checked on the active travel
# matrix. With demand groups each group's kernel is checked on the travel of that
# group, on the rows where the group has positive demand: the engine blanks the other
# rows (.group_clean_distance()). `decays` is the decay of every group in the order of
# the groups, or the one decay that all groups share.
.bind_check_zero_travel <- function(data, decays) {
  refuse <- function(kernel, n) {
    stop(kernel$name, " decay needs positive travel times, and ", n,
         if (n == 1) " origin-facility pair has" else " origin-facility pairs have",
         " travel 0; use decay_exponential() or ",
         "decay_gaussian(), or set a minimum travel time (for example ",
         "pmax(travel, 1))", call. = FALSE)
  }
  groups <- data$group_ids
  if (is.null(groups)) {
    kernel <- decays[[1L]]$kernel
    n <- if (isTRUE(kernel$zero_distance)) 0L else sum(data$distance_active == 0, na.rm = TRUE)
    if (n > 0L) refuse(kernel, n)
    return(invisible(TRUE))
  }
  for (i in seq_along(groups)) {
    kernel <- decays[[if (length(decays) == 1L) 1L else i]]$kernel
    if (isTRUE(kernel$zero_distance)) next
    travel <- data$distance_groups[[i]]
    if (is.null(travel)) travel <- data$distance_active
    n <- sum(travel[data$D_groups[, i] > 0, , drop = FALSE] == 0, na.rm = TRUE)
    if (n > 0L) refuse(kernel, n)
  }
  invisible(TRUE)
}
