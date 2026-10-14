# Shared fixtures for the AE kernel tests: problem declaration, solver, NFXP
# calibration, research tools and the SAE/HAAE providers.

# Two facilities `a` and `b` (supply 30 and 45) and four demand cells, as a
# 2 x 2 or a 1 x 4 raster. Travel to `a` rises (1, 2, 4, 5) while travel to
# `b` falls (5, 4, 2, 1), so both facilities have a near and a far side.
ae_toy_inputs <- function(shape = c("2x2", "1x4"), demand = c(10, 20, 30, 40)) {
  shape <- match.arg(shape)
  dims <- if (shape == "2x2") c(2L, 2L) else c(1L, 4L)
  d <- terra::rast(nrows = dims[1], ncols = dims[2])
  terra::values(d) <- demand
  distance <- c(d, d)
  names(distance) <- c("a", "b")
  terra::values(distance) <- cbind(c(1, 2, 4, 5), c(5, 4, 2, 1))
  list(demand = d, distance = distance, supply = c(a = 30, b = 45))
}

# A raster-input provider (`.<model>_problem`) on the toy inputs. `...` goes
# to the model constructor.
ae_toy_problem <- function(model = "huff", shape = "2x2",
                           demand = c(10, 20, 30, 40), ...) {
  f <- ae_toy_inputs(shape, demand)
  get(paste0(".", model, "_problem"))(f$demand, f$supply, f$distance, ...)
}

# Truth for the retained ratio-feedback model (DEC-026) on the toy inputs.
# feedback_scale = 1.5 gives a genuine contraction (spectral radius ~0.34).
ae_ratio_truth <- function() c(sigma = 2, v0 = 30, feedback_scale = 1.5)

# The ratio-feedback model on the toy inputs. `fix` names parameters held at
# their truth through `.fix_problem_parameter()`, e.g.
# fix = c("v0", "feedback_scale") leaves a sigma-only fixed-point problem.
ae_ratio_problem <- function(shape = "1x4", fix = character(), ...) {
  f <- ae_toy_inputs(shape)
  p <- .ratio_problem(.resolve_allocation(f$demand, f$supply, f$distance), ...)
  truth <- ae_ratio_truth()
  for (name in fix) p <- .fix_problem_parameter(p, name, truth[[name]])
  p
}

# Generate a target from `problem` at `truth`, blank the cells in `missing`,
# and refit from `init`. With the defaults this is the recurring recipe
# "solve at sigma = 2, fit from 1.5 within [0.5, 4]".
ae_recovery_fit <- function(problem, output = "utilization", missing = integer(),
                            truth = c(sigma = 2), init = c(sigma = 1.5),
                            lower = c(sigma = 0.5), upper = c(sigma = 4),
                            control = list(maxit = 20), ...) {
  observed <- .solve_problem(problem, truth, lambda = 0.7, tol = 1e-10,
                             max_iter = 500)$outputs[[output]]
  observed[missing] <- NA
  .fit_problem_nfxp(problem, observed, init, lower, upper, output = output,
                    lambda = 0.7, tol = 1e-10, max_iter = 500,
                    control = control, ...)
}

# Three facilities whose IDs are not in sorted order (b, a, c), on a 1 x n
# raster (n = 5 or 6) with a zero-demand and an unknown-demand cell, so the
# retained origin keys are nonconsecutive.
ae_identity_inputs <- function(n = 5) {
  rows <- seq_len(n)
  d <- terra::rast(nrows = 1, ncols = n)
  terra::values(d) <- c(8, 0, 13, NA, 21, 5)[rows]
  distance <- c(d, d, d)
  names(distance) <- c("b", "a", "c")
  terra::values(distance) <- cbind(c(1, 2, 4, 2, 5, 3), c(4, 2, 1, 2, 3, 2),
                                   c(2, 3, 5, 2, 1, 4))[rows, ]
  list(demand = d, distance = distance, supply = c(b = 30, a = 20, c = 25))
}

# A custom provider whose map returns theta, so the fixed point is x* = theta
# and every fitted value is known exactly. `outputs(theta)` adds outputs;
# `step` adds or replaces bound-step entries.
ae_identity_problem <- function(names = c("alpha", "beta"),
                                facility_ids = c("facility1", "facility2")[seq_along(names)],
                                outputs = function(theta) list(), step = list(),
                                substrate = list(), spec = list()) {
  n <- length(names)
  contract <- list(names = names, lower = rep(0, n), upper = rep(10, n))
  .new_problem(
    model = "toy",
    substrate = c(list(facility_ids = facility_ids), substrate),
    state = list(name = "x", axis = "J", init = rep(1, n), lower = 0, upper = 10),
    theta = contract,
    bind = function(theta) {
      theta <- .coerce_problem_theta(theta, contract)
      utils::modifyList(list(
        map = function(x) theta,
        outputs = function(x) c(list(target = theta, utilization = x),
                                outputs(theta)),
        theta = theta
      ), step)
    },
    metadata = list(spec = c(list(family = "toy"), spec))
  )
}
