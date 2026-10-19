# Grouped preparation for the allocation model (SPAX-057, T1): demand as a named
# list by group, travel shared or per group, the active-row rule (D-B1), the
# appended group fields and the shared group-ID naming rule. Ungrouped objects
# are tested in test-06-ae_preparation.R and must stay identical.

# Six origins, three facilities (not alphabetical), two groups. Origin o2 has
# no demand, o3 has demand in group b only, o4 only in group a, o5 is unknown in
# group b and o6 is active in both.
.group_prep_fixture <- function() {
  ids <- paste0("o", 1:6)
  Da <- stats::setNames(c(10, 0, 0, 7, 4, 6), ids)
  Db <- stats::setNames(c(5, 0, 8, 0, NA, 9), ids)
  M <- matrix(c(1, 2, 3, 4, 5, 6,  2, 3, 4, 5, 6, 7,  3, 4, 5, 6, 7, 8), 6, 3,
              dimnames = list(ids, c("f2", "f1", "f3")))
  list(ids = ids, demand = list(a = Da, b = Db), M = M,
       supply = c(f1 = 20, f2 = 15, f3 = 5))
}

.group_prepare <- function(f, ...) {
  prepare_allocation(f$demand, f$supply, f$M, missing_demand = "exclude", ...)
}

test_that("group IDs follow one naming rule shared with the FCA wrappers", {
  expect_identical(.check_group_ids(c("a", "b"), "x"), c("a", "b"))
  msg <- "^group names must be unique, non-empty, not NA, not \"all\" and without \"\\.\"$"
  for (bad in list(c("a", "a"), c("a", "all"), c("a", "b.c"), c("a", ""),
                   c("a", NA), NULL, character())) {
    expect_error(.check_group_ids(bad, "group names"), msg)
  }
  f <- .group_prep_fixture()
  for (bad in list(c("a", "a"), c("a", "all"), c("a", "b.c"), c("a", ""))) {
    d <- f$demand
    names(d) <- bad
    expect_error(prepare_allocation(d, f$supply, f$M, missing_demand = "exclude"),
                 "demand group names must be unique, non-empty")
  }
  expect_error(prepare_allocation(unname(f$demand), f$supply, f$M, origin_ids = f$ids),
               "demand group names must be unique, non-empty")
})

test_that("ungrouped preparation keeps its field names and order", {
  f <- .group_prep_fixture()
  p <- prepare_allocation(f$demand$a, f$supply, f$M)
  expect_identical(names(p), c("D_active", "distance_active", "S", "facility_ids",
    "origin_ids", "supply_cols", "demand_kept_index", "template", "cell_index",
    "all_origin_ids", "origin_status", "active_unknown_travel",
    "active_travel_status", "input_policy", "input_metadata"))
  expect_null(p$group_ids)
  expect_identical(p$D_active, unname(f$demand$a[c(1, 4, 5, 6)]))
  expect_identical(p$origin_ids, c("o1", "o4", "o5", "o6"))
  # a one-element list is grouped, a plain vector is not
  q <- prepare_allocation(list(a = f$demand$a), f$supply, f$M)
  expect_identical(names(q)[seq_along(names(p))], names(p))
  expect_identical(q$D_active, p$D_active)
})

test_that("grouped preparation with shared travel appends the group fields", {
  f <- .group_prep_fixture()
  p <- .group_prepare(f)
  expect_s3_class(p, "ae_prepared_inputs")
  ungrouped <- names(prepare_allocation(f$demand$a, f$supply, f$M))
  expect_identical(names(p), c(ungrouped, "group_ids", "D_groups", "distance_groups",
    "active_travel_status_groups", "active_unknown_travel_groups"))
  expect_identical(p$group_ids, c("a", "b"))
  # o5 has an unknown group-b demand: unknown as a whole; o2 is zero in both
  expect_identical(p$origin_status,
    c("active", "zero", "active", "active", "unknown", "active"))
  expect_identical(p$demand_kept_index, c(1L, 3L, 4L, 6L))
  expect_identical(p$origin_ids, c("o1", "o3", "o4", "o6"))
  expect_identical(p$D_active, c(15, 8, 7, 15))
  expect_identical(p$D_groups,
    matrix(c(10, 0, 7, 6,  5, 8, 0, 9), 4, 2,
           dimnames = list(c("o1", "o3", "o4", "o6"), c("a", "b"))))
  expect_equal(rowSums(p$D_groups), stats::setNames(p$D_active, p$origin_ids))
  # facilities follow the supply and travel column order f2, f1, f3 -> canonical
  # order is the travel column order
  expect_identical(p$facility_ids, c("f2", "f1", "f3"))
  expect_identical(as.vector(p$S), c(15, 20, 5))
  expect_identical(p$distance_active, f$M[c(1, 3, 4, 6), ])
  expect_identical(names(p$distance_groups), c("a", "b"))
  expect_identical(p$distance_groups$a, p$distance_active)
  expect_identical(p$distance_groups$b, p$distance_active)
  expect_identical(dimnames(p$distance_active), list(p$origin_ids, p$facility_ids))
  expect_identical(rownames(p$D_groups), rownames(p$distance_active))
  expect_identical(p$active_travel_status, rep("known", 4))
  expect_identical(p$active_unknown_travel,
    stats::setNames(rep(FALSE, 4), p$origin_ids))
  expect_identical(p$active_travel_status_groups,
    list(a = rep("known", 4), b = rep("known", 4)))
  expect_identical(p$active_unknown_travel_groups,
    list(a = p$active_unknown_travel, b = p$active_unknown_travel))
})

test_that("group demand order is free and unnamed included follows the first group", {
  f <- .group_prep_fixture()
  p <- .group_prepare(f)
  ord <- c(4, 6, 1, 2, 5, 3)
  h <- f
  h$demand$b <- f$demand$b[ord]
  expect_identical(.group_prepare(h), p)
  # the defaults come from the first group: reordering it reorders the origins
  g <- f
  g$demand$a <- f$demand$a[ord]
  q <- .group_prepare(g)
  expect_identical(q$all_origin_ids, f$ids[ord])
  # an explicit origin_ids fixes the order for both
  r <- .group_prepare(g, origin_ids = f$ids)
  expect_identical(r, p)
  # an unnamed mask follows the first group's order
  incl <- rep(TRUE, 6)
  incl[match("o1", names(g$demand$a))] <- FALSE
  s <- .group_prepare(g, origin_ids = f$ids, included = incl)
  expect_identical(s$origin_status[1], "excluded")
  expect_identical(s$origin_ids, c("o3", "o4", "o6"))
  # a named mask matches the origin IDs
  t <- .group_prepare(f, included = stats::setNames(c(FALSE, rep(TRUE, 5)), f$ids))
  expect_identical(t$origin_status[1], "excluded")
})

test_that("per-group travel is aligned by IDs and distance_active is NULL", {
  f <- .group_prep_fixture()
  Mb <- f$M * 10
  # group b: rows reversed and columns in another order
  Mb_in <- Mb[6:1, c("f3", "f2", "f1")]
  p <- prepare_allocation(f$demand, f$supply, list(a = f$M, b = Mb_in),
                          missing_demand = "exclude")
  keep <- c("o1", "o3", "o4", "o6")
  expect_null(p$distance_active)
  expect_true("distance_active" %in% names(p))
  expect_identical(p$facility_ids, c("f2", "f1", "f3"))
  expect_identical(p$distance_groups$a, f$M[keep, ])
  expect_identical(p$distance_groups$b, Mb[keep, c("f2", "f1", "f3")])
  expect_identical(dimnames(p$distance_groups$b), list(keep, c("f2", "f1", "f3")))
  # the order of the distance list is free (the facility default follows the
  # first matrix, so give it explicitly)
  q <- prepare_allocation(f$demand, f$supply, list(b = Mb_in, a = f$M),
                          facility_ids = c("f2", "f1", "f3"), missing_demand = "exclude")
  expect_identical(q, p)
  # D_active is still the total
  expect_identical(p$D_active, c(15, 8, 7, 15))
  # a facility-ID default comes from the first matrix; explicit IDs also work
  r <- prepare_allocation(f$demand, f$supply, list(a = f$M, b = Mb_in),
                          facility_ids = c("f3", "f1", "f2"), missing_demand = "exclude")
  expect_identical(r$facility_ids, c("f3", "f1", "f2"))
  expect_identical(r$distance_groups$b, Mb[keep, c("f3", "f1", "f2")])
  # a one-element group list is accepted
  one <- prepare_allocation(f$demand["a"], f$supply, list(a = f$M),
                            missing_demand = "exclude")
  expect_identical(one$group_ids, "a")
  expect_null(one$distance_active)
  expect_identical(one$D_active, unname(f$demand$a[c(1, 4, 5, 6)]))
  expect_identical(one$D_groups, matrix(c(10, 7, 4, 6), 4, 1,
    dimnames = list(c("o1", "o4", "o5", "o6"), "a")))
  # a shared single matrix with a one-element demand list is also grouped
  two <- prepare_allocation(f$demand["a"], f$supply, f$M)
  expect_identical(two$distance_active, f$M[c(1, 4, 5, 6), ])
})

test_that("a group list for distance needs exactly one matrix per group", {
  f <- .group_prep_fixture()
  expect_error(prepare_allocation(f$demand, f$supply, list(a = f$M),
                                  missing_demand = "exclude"),
               "give one element per group: a, b")
  expect_error(prepare_allocation(f$demand, f$supply, list(a = f$M, b = f$M, c = f$M),
                                  missing_demand = "exclude"),
               "give one element per group: a, b")
  expect_error(prepare_allocation(f$demand, f$supply, list(a = f$M, c = f$M),
                                  missing_demand = "exclude"),
               "give one element per group: a, b")
  # a wrong shape or labelling is refused per group
  expect_error(prepare_allocation(f$demand, f$supply, list(a = f$M, b = f$M[-1, ]),
                                  missing_demand = "exclude"),
               "distance for group 'b' must be an origin-by-facility numeric matrix")
  bad <- f$M
  rownames(bad)[1] <- "zz"
  expect_error(prepare_allocation(f$demand, f$supply, list(a = f$M, b = bad),
                                  missing_demand = "exclude"),
               "distance\\[\\['b'\\]\\] origin IDs must exactly match")
  # an ungrouped demand does not take a list of matrices
  expect_error(prepare_allocation(f$demand$a, f$supply, list(a = f$M)),
               "distance must be an origin-by-facility numeric matrix")
})

test_that("group demand vectors must name the same origins; matrices stay refused", {
  f <- .group_prep_fixture()
  d <- f$demand
  names(d$b)[2] <- "zz"
  expect_error(prepare_allocation(d, f$supply, f$M, missing_demand = "exclude"),
               "demand group 'b' IDs must exactly match canonical IDs")
  d <- f$demand
  d$b <- d$b[-1]
  expect_error(prepare_allocation(d, f$supply, f$M, missing_demand = "exclude"),
               "demand group 'b' IDs must exactly match canonical IDs")
  d <- f$demand
  d$b <- unname(d$b)
  expect_error(prepare_allocation(d, f$supply, f$M, missing_demand = "exclude"),
               "demand group 'b' must be a named numeric vector")
  d <- f$demand
  d$b <- as.character(d$b)
  expect_error(prepare_allocation(d, f$supply, f$M, missing_demand = "exclude"),
               "demand group 'b' must be a named numeric vector")
  expect_error(prepare_allocation(cbind(a = f$demand$a, b = f$demand$b), f$supply,
                                  f$M, origin_ids = f$ids),
               "demand must be a numeric vector")
  for (bad in c(-1, Inf)) {
    d <- f$demand
    d$b[1] <- bad
    expect_error(prepare_allocation(d, f$supply, f$M, missing_demand = "exclude"),
                 "demand must be finite and nonnegative")
  }
})

test_that("an origin is active when every group demand is known and the total is positive (D-B1)", {
  f <- .group_prep_fixture()
  # default "error" refuses a missing group demand and names the group
  expect_error(prepare_allocation(f$demand, f$supply, f$M),
               "included missing demand requires missing_demand = 'exclude' \\(missing in group: b\\)")
  # excluding the origin, or declaring it not included, makes the default pass
  incl <- stats::setNames(f$ids != "o5", f$ids)
  p <- prepare_allocation(f$demand, f$supply, f$M, included = incl)
  expect_identical(p$origin_status[5], "excluded")
  # "exclude": the unknown origin is dropped as a whole, its known demand too
  q <- .group_prepare(f)
  expect_identical(q$origin_status[5], "unknown")
  expect_false("o5" %in% q$origin_ids)
  expect_identical(sum(q$D_active), 45)
  # missing in both groups at one origin: still one unknown origin
  g <- f
  g$demand$a[5] <- NA
  expect_identical(.group_prepare(g)$origin_status[5], "unknown")
  expect_error(prepare_allocation(g$demand, g$supply, g$M), "missing in group: a, b")
  # zero in a group at an active origin is fine (o3, o4); zero in all is "zero"
  expect_identical(q$origin_status[c(2, 3, 4)], c("zero", "active", "active"))
  expect_identical(q$D_groups["o3", ], c(a = 0, b = 8))
  expect_identical(q$D_groups["o4", ], c(a = 7, b = 0))
  # an excluded origin keeps its status whatever its demand
  h <- .group_prepare(f, included = stats::setNames(f$ids != "o6", f$ids))
  expect_identical(h$origin_status[6], "excluded")
  expect_false("o6" %in% h$origin_ids)
})

test_that("travel statuses are the union over groups and per group", {
  f <- .group_prep_fixture()
  # shared travel with an unknown cell at o4 (group a only demands there) and
  # at o3 (group b only demands there) and all-absent travel at o6
  M <- f$M
  M["o4", "f1"] <- NA
  M["o6", ] <- Inf
  q <- prepare_allocation(f$demand, f$supply, M, missing_demand = "exclude",
                          missing_travel = "legacy_zero")
  # active rows o1, o3, o4, o6
  expect_identical(q$active_travel_status, c("known", "known", "unknown", "all_absent"))
  expect_identical(unname(q$active_unknown_travel), c(FALSE, FALSE, TRUE, FALSE))
  expect_identical(names(q$active_unknown_travel), q$origin_ids)
  expect_identical(q$active_travel_status_groups$a,
                   c("known", "known", "unknown", "all_absent"))
  # group b has no demand at o4: its NA travel is ignored there
  expect_identical(q$active_travel_status_groups$b,
                   c("known", "known", "known", "all_absent"))
  expect_identical(unname(q$active_unknown_travel_groups$a), c(FALSE, FALSE, TRUE, FALSE))
  expect_identical(unname(q$active_unknown_travel_groups$b), rep(FALSE, 4))
  expect_identical(names(q$active_unknown_travel_groups$b), q$origin_ids)
  # the strict default refuses the unknown cell, naming the group
  expect_error(prepare_allocation(f$demand, f$supply, M, missing_demand = "exclude"),
               "active travel is missing.*\\(group 'a'\\)")
  # a row is all absent in the union only if every group with demand has no edge
  Mb <- f$M
  Mb["o6", ] <- Inf
  Ma <- f$M
  r <- prepare_allocation(f$demand, f$supply, list(a = Ma, b = Mb),
                          missing_demand = "exclude")
  expect_identical(r$active_travel_status, rep("known", 4))
  expect_identical(r$active_travel_status_groups$b[4], "all_absent")
  expect_identical(r$active_travel_status_groups$a[4], "known")
  # unknown travel in one group's matrix flows into the union
  Mb2 <- f$M
  Mb2["o3", "f2"] <- NA
  s <- prepare_allocation(f$demand, f$supply, list(a = f$M, b = Mb2),
                          missing_demand = "exclude", missing_travel = "legacy_zero")
  expect_identical(s$active_travel_status, c("known", "unknown", "known", "known"))
  expect_identical(s$active_travel_status_groups$a, rep("known", 4))
  expect_identical(s$active_travel_status_groups$b, c("known", "unknown", "known", "known"))
  expect_identical(unname(s$active_unknown_travel), c(FALSE, TRUE, FALSE, FALSE))
  expect_error(prepare_allocation(f$demand, f$supply, list(a = f$M, b = Mb2),
                                  missing_demand = "exclude"),
               "active travel is missing.*\\(group 'b'\\)")
})

test_that("missing or invalid travel for a zero-demand group is ignored", {
  f <- .group_prep_fixture()
  # group b has zero demand at o4 and o1 .. only o4: NA and negative travel there
  # carry no flow, so the strict default accepts them
  Mb <- f$M
  Mb["o4", ] <- c(NA, -1, NA)
  p <- prepare_allocation(f$demand, f$supply, list(a = f$M, b = Mb),
                          missing_demand = "exclude")
  expect_identical(p$active_travel_status, rep("known", 4))
  expect_identical(p$active_travel_status_groups$b, rep("known", 4))
  # the matrix is still retained for the group
  expect_true(anyNA(p$distance_groups$b))
  # the same cells matter once the group has demand there
  d <- f$demand
  d$b["o4"] <- 3
  expect_error(prepare_allocation(d, f$supply, list(a = f$M, b = Mb),
                                  missing_demand = "exclude"),
               "nonnegative or positive infinity \\(group 'b'\\)")
  Mb[Mb < 0 & !is.na(Mb)] <- 1
  expect_error(prepare_allocation(d, f$supply, list(a = f$M, b = Mb),
                                  missing_demand = "exclude"),
               "active travel is missing")
  # omitted (zero, unknown or excluded) origins are never read
  Mc <- f$M
  Mc[c("o2", "o5"), ] <- NA
  expect_no_error(prepare_allocation(f$demand, f$supply, list(a = Mc, b = Mc),
                                     missing_demand = "exclude"))
  # negative travel in a group with demand is refused, also for shared travel
  Ms <- f$M
  Ms["o1", "f1"] <- -2
  expect_error(prepare_allocation(f$demand, f$supply, Ms, missing_demand = "exclude"),
               "nonnegative or positive infinity")
})

test_that("grouped preparation carries spatial and metadata like the ungrouped one", {
  f <- .group_prep_fixture()
  p <- .group_prepare(f, metadata = list(demand_units = "persons", travel_units = "min"))
  expect_identical(p$input_metadata$demand_units, "persons")
  expect_identical(p$input_metadata$travel_units, "min")
  expect_identical(p$input_policy,
    list(source = "numeric", missing_demand = "exclude", missing_travel = "error",
         kernel_domain = "strict"))
  expect_null(p$template)
  expect_error(prepare_allocation(f$demand, c(f1 = 1, f2 = -1, f3 = 2), f$M,
                                  missing_demand = "exclude"),
               "supply must be finite and nonnegative")
  expect_error(prepare_allocation(f$demand, c(1, 2), f$M, missing_demand = "exclude"),
               "supply must be a numeric vector")
})

test_that("shared travel in the grouped form equals the ungrouped preparation on the total", {
  f <- .group_prep_fixture()
  # same total demand and the same travel: the ungrouped fields agree
  total <- f$demand$a + f$demand$b
  p <- .group_prepare(f)
  u <- prepare_allocation(total, f$supply, f$M, missing_demand = "exclude")
  common <- names(u)
  expect_identical(unclass(p)[common], unclass(u)[common])
  # one group: identical to the ungrouped object on that group, plus fields
  one <- prepare_allocation(f$demand["a"], f$supply, f$M, missing_demand = "exclude")
  v <- prepare_allocation(f$demand$a, f$supply, f$M, missing_demand = "exclude")
  expect_identical(unclass(one)[names(v)], unclass(v))
})

# Grouped CLM model and evaluation (SPAX-057, T2) ------------------------------
#
# The grouped spec and theta contract, the group core in the bind, the pooled and
# per-group outputs, the fixed-name rule and the equivalence with the ungrouped
# model. Expected values come from clm_group_oracle() (helper-clm.R).

.group_model_inputs <- function(f = .group_prep_fixture(), distance = f$M, ...) {
  prepare_allocation(f$demand, f$supply, distance, missing_demand = "exclude", ...)
}

# Compare an evaluation with the oracle: pooled outputs, loads, requested
# per-group outputs and the coverage cells.
.expect_group_oracle <- function(model, theta, oracle) {
  p <- model$substrate
  e <- evaluate_allocation(model, theta,
    requested_outputs = c("flow", "utilization_by_group", "access_by_group",
                          "outside_share_by_group"))
  o <- e$outputs
  expect_equal(unname(o$allocation), unname(oracle$allocation), tolerance = 1e-12)
  expect_identical(o$huff_share, o$allocation)
  expect_equal(as.numeric(o$access), unname(oracle$rho), tolerance = 1e-12)
  expect_equal(as.numeric(o$outside_share), unname(oracle$outside_share), tolerance = 1e-12)
  expect_equal(o$utilization, oracle$utilization, tolerance = 1e-12)
  expect_equal(unname(o$flow), unname(oracle$flow), tolerance = 1e-12)
  expect_equal(unname(o$utilization_by_group), unname(oracle$utilization_by_group),
               tolerance = 1e-12)
  expect_equal(unname(o$access_by_group), unname(oracle$access_by_group), tolerance = 1e-12)
  expect_equal(unname(o$outside_share_by_group), unname(oracle$outside_share_by_group),
               tolerance = 1e-12)
  expect_null(o$opportunity)
  expect_identical(dimnames(o$utilization_by_group), list(p$facility_ids, p$group_ids))
  expect_identical(dimnames(o$access_by_group), list(p$origin_ids, p$group_ids))
  expect_identical(dimnames(o$outside_share_by_group), list(p$origin_ids, p$group_ids))
  expect_equal(e$coverage$cells$rho, unname(oracle$rho), tolerance = 1e-12)
  expect_equal(e$coverage$cells$A, unname(oracle$A), tolerance = 1e-12)
  expect_equal(e$coverage$cells$demand, unname(oracle$D))
  expect_true(e$coverage$conservation$passed)
  invisible(e)
}

test_that("a grouped model equals the oracle with shared travel", {
  f <- .group_prep_fixture()
  p <- .group_model_inputs(f)
  m <- clm_allocation(p, by_group = "sigma", fit_v0 = TRUE)
  expect_identical(m$theta$names, c("sigma.a", "sigma.b", "v0"))
  theta <- c(sigma.a = 2, sigma.b = 3.5, v0 = 4)
  oracle <- clm_group_oracle(p$distance_groups, p$D_groups, as.vector(p$S),
                             sigma = c(a = 2, b = 3.5), v0 = 4)
  e <- .expect_group_oracle(m, theta, oracle)
  # the group quantities are not the pooled ones
  expect_false(isTRUE(all.equal(e$outputs$utilization_by_group[, "a"],
                                e$outputs$utilization_by_group[, "b"])))
  expect_equal(unname(rowSums(e$outputs$utilization_by_group)), e$outputs$utilization)
  # pooled access and outside probability add to one on every origin
  expect_equal(unname(e$outputs$access + e$outputs$outside_share), rep(1, 4))
  # theta can be given in any order
  expect_equal(evaluate_allocation(m, theta[c(3, 1, 2)])$outputs$utilization,
               e$outputs$utilization)
  # a v0 for each group as well
  m2 <- clm_allocation(p, by_group = c("v0", "sigma"), fit_v0 = TRUE)
  expect_identical(m2$theta$names, c("sigma.a", "sigma.b", "v0.a", "v0.b"))
  oracle2 <- clm_group_oracle(p$distance_groups, p$D_groups, as.vector(p$S),
                              sigma = c(a = 2, b = 3.5), v0 = c(a = 1, b = 9))
  .expect_group_oracle(m2, c(sigma.a = 2, sigma.b = 3.5, v0.a = 1, v0.b = 9), oracle2)
  # a shared sigma with a per-group v0
  m3 <- clm_allocation(p, by_group = "v0", fit_v0 = TRUE)
  expect_identical(m3$theta$names, c("sigma", "v0.a", "v0.b"))
  oracle3 <- clm_group_oracle(p$distance_groups, p$D_groups, as.vector(p$S),
                              sigma = 3, v0 = c(a = 1, b = 9))
  .expect_group_oracle(m3, c(sigma = 3, v0.a = 1, v0.b = 9), oracle3)
})

test_that("a grouped model equals the oracle with per-group travel", {
  f <- .group_prep_fixture()
  Mb <- f$M * 1.7 + 0.5
  Mb["o3", "f1"] <- Inf
  p <- .group_model_inputs(f, distance = list(a = f$M, b = Mb[6:1, c("f3", "f2", "f1")]))
  expect_null(p$distance_active)
  m <- clm_allocation(p, by_group = "sigma", fit_v0 = TRUE, fit_beta = TRUE)
  expect_identical(m$theta$names, c("sigma.a", "sigma.b", "v0", "beta"))
  oracle <- clm_group_oracle(p$distance_groups, p$D_groups, as.vector(p$S),
                             sigma = c(a = 2.5, b = 4), v0 = 3, beta = 0.8)
  .expect_group_oracle(m, c(sigma.a = 2.5, sigma.b = 4, v0 = 3, beta = 0.8), oracle)
  # the exponential family, shared by the groups, with the travel of each
  m2 <- clm_allocation(p, family = "exponential", fit_v0 = TRUE, by_group = "sigma")
  oracle2 <- clm_group_oracle(p$distance_groups, p$D_groups, as.vector(p$S),
                              sigma = c(a = 0.4, b = 0.2), v0 = 3, family = "exponential")
  .expect_group_oracle(m2, c(sigma.a = 0.4, sigma.b = 0.2, v0 = 3), oracle2)
})

test_that("a grouped model takes a family by group, including a declared kernel", {
  f <- .group_prep_fixture()
  p <- .group_model_inputs(f)
  kern <- function(distance, sigma) exp(-distance / sigma)
  declared <- decay_kernel(kern, name = "scaled-exp", parameter = "scale", lower = 0.5,
                           upper = 40)
  m <- clm_allocation(p, family = list(b = declared, a = "gaussian"), fit_v0 = TRUE)
  # a family by group implies sigma by group; the order follows the groups
  expect_identical(m$metadata$spec$by_group, "sigma")
  expect_identical(m$metadata$spec$family, "grouped")
  expect_identical(m$theta$names, c("sigma.a", "sigma.b", "v0"))
  expect_identical(m$theta$lower, c(0, 0.5, 0))
  expect_identical(m$theta$upper, c(Inf, 40, Inf))
  expect_identical(names(m$metadata$spec$families), c("a", "b"))
  expect_identical(m$metadata$spec$families$a, "gaussian")
  expect_s3_class(m$metadata$spec$families$b, "spax_decay_kernel")
  expect_null(m$metadata$spec$kernel)
  oracle <- clm_group_oracle(p$distance_groups, p$D_groups, as.vector(p$S),
    sigma = c(a = 3, b = 6), v0 = 2, family = c(a = "gaussian", b = "other"),
    kernels = list(a = NULL, b = kern))
  e <- .expect_group_oracle(m, c(sigma.a = 3, sigma.b = 6, v0 = 2), oracle)
  # sigma of the declared kernel must be inside its bounds
  expect_error(evaluate_allocation(m, c(sigma.a = 3, sigma.b = 0.1, v0 = 2)),
               "outside the theta bounds")
  # built-in declarations are the strings; a shared declared kernel is spec$kernel
  m2 <- clm_allocation(p, family = list(a = decay_kernel("gaussian"), b = "power"),
                       v0 = 1)
  expect_identical(m2$metadata$spec$families, list(a = "gaussian", b = "power"))
  expect_identical(m2$theta$names, c("sigma.a", "sigma.b"))
  m3 <- clm_allocation(p, family = declared, by_group = "sigma", fit_v0 = TRUE)
  expect_s3_class(m3$metadata$spec$kernel, "spax_decay_kernel")
  expect_identical(m3$metadata$spec$family, "scaled-exp")
  expect_identical(m3$theta$lower, c(0.5, 0.5, 0))
  expect_identical(names(m3$metadata$spec)[12:18],
    c("theta", "kernel", "groups", "by_group", "families", "fixed", "v0_groups"))
  oracle3 <- clm_group_oracle(p$distance_groups, p$D_groups, as.vector(p$S),
    sigma = c(a = 3, b = 6), v0 = 2, kernels = kern)
  .expect_group_oracle(m3, c(sigma.a = 3, sigma.b = 6, v0 = 2), oracle3)
  # a shared declared kernel with a shared sigma
  m4 <- clm_allocation(p, family = declared, fit_v0 = TRUE)
  expect_identical(m4$theta$names, c("sigma", "v0"))
  oracle4 <- clm_group_oracle(p$distance_groups, p$D_groups, as.vector(p$S),
    sigma = 4, v0 = 2, kernels = kern)
  .expect_group_oracle(m4, c(sigma = 4, v0 = 2), oracle4)
  # a mixed family by group with different travel
  q <- .group_model_inputs(f, distance = list(a = f$M, b = f$M * 2))
  m5 <- clm_allocation(q, family = list(a = "exponential", b = declared), v0 = 1)
  oracle5 <- clm_group_oracle(q$distance_groups, q$D_groups, as.vector(q$S),
    sigma = c(a = 0.3, b = 5), v0 = 1, family = c(a = "exponential", b = "x"),
    kernels = list(a = NULL, b = kern))
  .expect_group_oracle(m5, c(sigma.a = 0.3, sigma.b = 5), oracle5)
})

test_that("groups with zero demand, missing travel and one group are handled", {
  f <- .group_prep_fixture()
  # group b has zero demand at o4: its travel there is never validated; NA cells
  # and (for the power kernel) zero distances are absent edges
  Mb <- f$M
  Mb["o4", ] <- c(NA, -1, NA)
  p <- prepare_allocation(f$demand, f$supply, list(a = f$M, b = Mb),
                          missing_demand = "exclude")
  m <- clm_allocation(p, by_group = "sigma", v0 = 2)
  e <- evaluate_allocation(m, c(sigma.a = 3, sigma.b = 4),
                           requested_outputs = c("utilization_by_group", "access_by_group",
                                                 "outside_share_by_group"))
  # the pooled outputs and the loads do not depend on the blanked row of group b
  rows <- p$origin_ids
  oracle <- clm_group_oracle(list(a = f$M[rows, ], b = f$M[rows, ]), p$D_groups, as.vector(p$S),
                             sigma = c(a = 3, b = 4), v0 = 2)
  expect_equal(unname(e$outputs$allocation), unname(oracle$allocation), tolerance = 1e-12)
  expect_equal(e$outputs$utilization, oracle$utilization, tolerance = 1e-12)
  expect_equal(unname(e$outputs$utilization_by_group), unname(oracle$utilization_by_group),
               tolerance = 1e-12)
  # group b has no demand and no usable travel at o4: its own access and outside
  # share there are unknown (NA), not "no access"; everything else is finite
  expect_true(is.na(e$outputs$access_by_group["o4", "b"]))
  expect_true(is.na(e$outputs$outside_share_by_group["o4", "b"]))
  expect_true(all(is.finite(e$outputs$access_by_group[-match("o4", rownames(e$outputs$access_by_group)), ])))
  # zero distances matter for the power kernel only where the group has demand
  Mz <- f$M
  Mz["o4", "f1"] <- 0
  dz <- f$demand
  dz$b["o4"] <- 0
  pz <- prepare_allocation(dz, f$supply, list(a = f$M + 1, b = Mz), missing_demand = "exclude")
  mz <- clm_allocation(pz, family = "power", by_group = "sigma")
  expect_no_error(evaluate_allocation(mz, c(sigma.a = 1.5, sigma.b = 2)))
  dz$b["o4"] <- 3
  pz2 <- prepare_allocation(dz, f$supply, list(a = f$M + 1, b = Mz), missing_demand = "exclude")
  mz2 <- clm_allocation(pz2, family = "power", by_group = "sigma")
  expect_error(evaluate_allocation(mz2, c(sigma.a = 1.5, sigma.b = 2)),
               "power decay requires strictly positive finite travel distances")
  # a group with zero demand everywhere contributes nothing
  d0 <- f$demand
  d0$b[] <- 0
  p0 <- prepare_allocation(d0, f$supply, f$M, missing_demand = "exclude")
  m0 <- clm_allocation(p0, by_group = "sigma", fit_v0 = TRUE)
  e0 <- evaluate_allocation(m0, c(sigma.a = 3, sigma.b = 5, v0 = 2),
                            requested_outputs = "utilization_by_group")
  expect_identical(unname(e0$outputs$utilization_by_group[, "b"]), rep(0, 3))
  u <- prepare_allocation(d0$a, f$supply, f$M, missing_demand = "exclude")
  eu <- evaluate_allocation(clm_allocation(u, fit_v0 = TRUE), c(sigma = 3, v0 = 2))
  expect_equal(e0$outputs$allocation, eu$outputs$allocation, tolerance = 1e-12)
  # G = 1 grouped equals the ungrouped model on that group
  one <- prepare_allocation(f$demand["a"], f$supply, f$M, missing_demand = "exclude")
  mo <- clm_allocation(one, by_group = "sigma", fit_v0 = TRUE)
  expect_identical(mo$theta$names, c("sigma.a", "v0"))
  v <- prepare_allocation(f$demand$a, f$supply, f$M, missing_demand = "exclude")
  eo <- evaluate_allocation(mo, c(sigma.a = 3, v0 = 2),
                            requested_outputs = c("flow", "utilization_by_group"))
  ev <- evaluate_allocation(clm_allocation(v, fit_v0 = TRUE), c(sigma = 3, v0 = 2),
                            requested_outputs = "flow")
  expect_equal(eo$outputs$allocation, ev$outputs$allocation, tolerance = 1e-12)
  expect_equal(eo$outputs$flow, ev$outputs$flow, tolerance = 1e-12)
  expect_equal(eo$outputs$utilization, ev$outputs$utilization, tolerance = 1e-12)
  expect_equal(unname(eo$outputs$utilization_by_group[, "a"]), ev$outputs$utilization,
               tolerance = 1e-12)
})

test_that("exclude with a missing group demand drops the whole origin", {
  f <- .group_prep_fixture()
  p <- .group_model_inputs(f)
  # o5 has a missing group-b demand and is unknown as a whole
  expect_false("o5" %in% p$origin_ids)
  m <- clm_allocation(p, by_group = "sigma", v0 = 1)
  e <- evaluate_allocation(m, c(sigma.a = 3, sigma.b = 4))
  expect_identical(rownames(e$outputs$allocation), c("o1", "o3", "o4", "o6"))
  expect_identical(e$coverage$cells$origin_id, c("o1", "o3", "o4", "o6"))
  expect_equal(sum(e$outputs$utilization) + sum(p$D_active * e$outputs$outside_share),
               sum(p$D_active), tolerance = 1e-12)
})

test_that("fixed parameters leave the free names and the spec keeps them", {
  f <- .group_prep_fixture()
  p <- .group_model_inputs(f)
  m <- clm_allocation(p, by_group = c("sigma", "v0"), fit_v0 = TRUE,
                      fixed = c(v0.b = 0, sigma.a = 2.5))
  expect_identical(m$theta$names, c("sigma.b", "v0.a"))
  expect_identical(m$theta$group, c("b", "a"))
  expect_identical(m$metadata$spec$fixed, c(sigma.a = 2.5, v0.b = 0))
  expect_identical(m$metadata$spec$theta, m$theta)
  oracle <- clm_group_oracle(p$distance_groups, p$D_groups, as.vector(p$S),
                             sigma = c(a = 2.5, b = 4), v0 = c(a = 3, b = 0))
  e <- .expect_group_oracle(m, c(sigma.b = 4, v0.a = 3), oracle)
  # a fixed name may be given with its fixed value, in any order
  e2 <- evaluate_allocation(m, c(v0.a = 3, v0.b = 0, sigma.b = 4, sigma.a = 2.5))
  expect_identical(e2$outputs, evaluate_allocation(m, c(sigma.b = 4, v0.a = 3))$outputs)
  # but not with another value, and an unknown name is an error
  expect_error(evaluate_allocation(m, c(sigma.b = 4, v0.a = 3, v0.b = 1)),
               "parameter v0.b is fixed at 0")
  expect_error(evaluate_allocation(m, c(sigma.b = 4, v0.a = 3, sigma.a = 2)),
               "parameter sigma.a is fixed at 2.5")
  expect_error(evaluate_allocation(m, c(sigma.b = 4, v0.a = 3, nope = 1)),
               "unknown parameter\\(s\\) in `theta`: nope")
  expect_error(evaluate_allocation(m, c(sigma.b = 4, v0.c = 3)), "unknown parameter")
  expect_error(evaluate_allocation(m, c(sigma.b = 4)), "missing required parameter")
  # unnamed values follow the free names
  expect_identical(evaluate_allocation(m, c(4, 3))$outputs$utilization, e$outputs$utilization)
  # a shared fixed sigma and beta
  m2 <- clm_allocation(p, fit_v0 = TRUE, fit_beta = TRUE, by_group = "v0",
                       fixed = c(sigma = 3, beta = 1))
  expect_identical(m2$theta$names, c("v0.a", "v0.b"))
  expect_identical(m2$metadata$spec$fixed, c(sigma = 3, beta = 1))
  # the contract names, shared and per-group, in the documented order
  full <- clm_allocation(p, by_group = c("sigma", "v0"), fit_v0 = TRUE, fit_beta = TRUE)
  expect_identical(full$theta$names,
                   c("sigma.a", "sigma.b", "v0.a", "v0.b", "beta"))
  expect_identical(full$theta$group, c("a", "b", "a", "b", NA))
  expect_identical(.huff_group_theta_contract(full$metadata$spec), full$theta)
})

test_that("v0 constants by group with fit_v0 = FALSE", {
  f <- .group_prep_fixture()
  p <- .group_model_inputs(f)
  m <- clm_allocation(p, by_group = "sigma", v0 = c(b = 6, a = 1))
  expect_identical(m$theta$names, c("sigma.a", "sigma.b"))
  expect_identical(m$metadata$spec$v0_groups, c(a = 1, b = 6))
  oracle <- clm_group_oracle(p$distance_groups, p$D_groups, as.vector(p$S),
                             sigma = c(a = 2, b = 3), v0 = c(a = 1, b = 6))
  .expect_group_oracle(m, c(sigma.a = 2, sigma.b = 3), oracle)
  # a scalar v0 is shared and keeps its value in the spec
  s <- clm_allocation(p, by_group = "sigma", v0 = 2)
  expect_identical(s$metadata$spec$v0, 2)
  expect_null(s$metadata$spec$v0_groups)
  expect_error(clm_allocation(p, v0 = c(a = 1, b = 2), fit_v0 = TRUE),
               "constants")
  expect_error(clm_allocation(p, v0 = c(a = 1)), "v0: give one element per group: a, b")
  expect_error(clm_allocation(p, v0 = c(a = 1, b = -2)), "nonnegative")
})

test_that("a grouped model with everything shared equals the ungrouped model on the total", {
  f <- .group_prep_fixture()
  p <- .group_model_inputs(f)
  total <- f$demand$a + f$demand$b
  u <- prepare_allocation(total, f$supply, f$M, missing_demand = "exclude")
  expect_identical(u$D_active, p$D_active)
  req <- c("flow", "utilization_by_group")
  for (spec in list(
    list(args = list(fit_v0 = TRUE), theta = c(sigma = 3, v0 = 2)),
    list(args = list(fit_v0 = TRUE, fit_beta = TRUE, family = "exponential"),
         theta = c(sigma = 0.3, v0 = 2, beta = 0.7)),
    list(args = list(v0 = 1.5), theta = c(sigma = 2.5)))) {
    um <- do.call(clm_allocation, c(list(u), spec$args))
    gm <- do.call(clm_allocation, c(list(p), spec$args))
    eu <- evaluate_allocation(um, spec$theta, requested_outputs = "flow")
    eg <- evaluate_allocation(gm, spec$theta, requested_outputs = req)
    expect_identical(gm$theta$names, um$theta$names)
    for (nm in c("target", "attractiveness", "utilization", "access", "outside_share",
                 "allocation", "huff_share", "flow")) {
      expect_equal(eg$outputs[[nm]], eu$outputs[[nm]], tolerance = 1e-12)
    }
    expect_equal(eg$coverage$cells, eu$coverage$cells, tolerance = 1e-10)
    expect_equal(eg$coverage$regional, eu$coverage$regional, tolerance = 1e-10)
    expect_equal(eg$coverage$facilities, eu$coverage$facilities, tolerance = 1e-10)
    expect_equal(eg$coverage$conservation, eu$coverage$conservation, tolerance = 1e-10)
    # every group has the pooled quantities
    shares <- eg$outputs$utilization_by_group
    expect_equal(unname(rowSums(shares)), eg$outputs$utilization, tolerance = 1e-12)
  }
  # per-group parameters set equal give the same as the shared model
  shared <- clm_allocation(p, fit_v0 = TRUE)
  split <- clm_allocation(p, by_group = c("sigma", "v0"), fit_v0 = TRUE)
  a <- evaluate_allocation(shared, c(sigma = 3, v0 = 2))
  b <- evaluate_allocation(split, c(sigma.a = 3, sigma.b = 3, v0.a = 2, v0.b = 2))
  expect_equal(a$outputs, b$outputs, tolerance = 1e-12)
  expect_equal(a$coverage$cells, b$coverage$cells, tolerance = 1e-12)
  # also with a family by group of the same kernel
  fam <- clm_allocation(p, family = list(a = "gaussian", b = "gaussian"), fit_v0 = TRUE)
  expect_equal(evaluate_allocation(fam, c(sigma.a = 3, sigma.b = 3, v0 = 2))$outputs,
               a$outputs, tolerance = 1e-12)
})

test_that("the group core is the ungrouped state on shared kernels and checks its inputs", {
  K <- matrix(c(1, .5, .2, .8, .4, .1), 2, 3, dimnames = list(c("o1", "o2"), c("f1", "f2", "f3")))
  D <- cbind(a = c(3, 0), b = c(2, 5))
  a <- c(4, 2, 1)
  g <- .clm_group_state(a, list(K, K), D, c(1, 1))
  u <- .huff_state(a, structure(list(Kd_active = K, D_active = rowSums(D)),
                                class = "huff_compact_plan"), v0 = 1)
  expect_equal(g$allocation, u$allocation)
  expect_equal(g$utilization, u$utilization)
  expect_equal(g$access, u$access)
  expect_equal(g$outside_share, u$outside_share)
  expect_identical(g$allocation, g$huff_share)
  expect_false("opportunity" %in% names(g))
  expect_identical(dim(g$utilization_by_group), c(3L, 2L))
  expect_identical(dim(g$access_by_group), c(2L, 2L))
  expect_error(.clm_group_state(a, list(K), D, c(1, 1)), "one element per group")
  expect_error(.clm_group_state(a, list(K, K), D, c(1, -1)), "nonnegative")
  expect_error(.clm_group_state(a[-1], list(K, K), D, c(1, 1)), "must match")
  # without an outside option the outside share is zero and unnamed
  z <- .clm_group_state(a, list(K, K), D, c(0, 0))
  expect_identical(z$outside_share, c(0, 0))
  expect_equal(unname(z$access), c(1, 1))
})

test_that("grouped spec, problem and contract names and order", {
  f <- .group_prep_fixture()
  p <- .group_model_inputs(f)
  u <- prepare_allocation(f$demand$a, f$supply, f$M, missing_demand = "exclude")
  # ungrouped: identical to develop
  um <- clm_allocation(u, fit_v0 = TRUE)
  expect_identical(names(um$metadata$spec), c("model", "family", "deterministic",
    "output_axes", "output_meanings", "allocation", "kappa", "beta", "v0", "fit_v0",
    "fit_beta", "theta"))
  expect_identical(names(um), c("model", "substrate", "state", "theta", "bind", "metadata"))
  expect_identical(names(um$theta), c("names", "lower", "upper"))
  expect_identical(names(um$metadata$spec$output_axes), c("target", "utilization",
    "attractiveness", "access", "outside_share", "allocation", "huff_share",
    "opportunity", "flow"))
  expect_identical(names(.huff_theta_contract(TRUE, TRUE)), c("names", "lower", "upper"))
  expect_identical(.huff_theta_contract(TRUE, TRUE)$names, c("sigma", "v0", "beta"))
  # grouped
  gm <- clm_allocation(p, by_group = "sigma", fit_v0 = TRUE)
  spec <- gm$metadata$spec
  expect_identical(names(spec), c("model", "family", "deterministic", "output_axes",
    "output_meanings", "allocation", "kappa", "beta", "v0", "fit_v0", "fit_beta",
    "theta", "groups", "by_group", "families", "fixed", "v0_groups"))
  expect_identical(names(gm), c("model", "substrate", "state", "theta", "bind", "metadata"))
  expect_identical(names(gm$theta), c("names", "lower", "upper", "group"))
  expect_identical(spec$groups, c("a", "b"))
  expect_identical(spec$family, "gaussian")
  expect_identical(spec$families, list(a = "gaussian", b = "gaussian"))
  expect_identical(spec$fixed, stats::setNames(numeric(), character()))
  expect_true("v0_groups" %in% names(spec))
  expect_null(spec$v0_groups)
  expect_identical(names(spec$output_axes), c("target", "utilization", "attractiveness",
    "access", "outside_share", "allocation", "huff_share", "flow",
    "utilization_by_group", "access_by_group", "outside_share_by_group"))
  expect_identical(spec$output_axes[["utilization_by_group"]], "facility_group")
  expect_identical(spec$output_axes[["access_by_group"]], "origin_group")
  expect_identical(spec$output_axes[["outside_share_by_group"]], "origin_group")
  expect_identical(spec$output_meanings[["utilization_by_group"]], "expected_count")
  expect_false("opportunity" %in% names(spec$output_axes))
  expect_false("opportunity" %in% names(spec$output_meanings))
  # a one-element group list is a grouped model
  one <- prepare_allocation(f$demand["a"], f$supply, f$M, missing_demand = "exclude")
  expect_identical(clm_allocation(one, by_group = "sigma")$theta$names, "sigma.a")
  expect_identical(clm_allocation(one)$theta$names, "sigma")
  # the problem passes the output declarations
  step <- .bind_theta(gm, c(sigma.a = 2, sigma.b = 3, v0 = 1))
  expect_identical(step$state_independent_outputs,
    c("flow", "utilization_by_group", "access_by_group", "outside_share_by_group"))
  expect_null(step$clm_prediction)
  # the default outputs do not carry the per-group matrices
  expect_false(any(c("utilization_by_group", "access_by_group", "outside_share_by_group",
                     "flow") %in% names(step$outputs(NULL))))
  expect_identical(step$outputs_requested(NULL, "utilization_by_group")$utilization_by_group,
                   step$outputs_requested(NULL, c("flow", "utilization_by_group"))$utilization_by_group)
})

test_that("grouped outputs have declared axes and bind group-labelled targets", {
  f <- .group_prep_fixture()
  p <- .group_model_inputs(f)
  m <- clm_allocation(p, by_group = "sigma", fit_v0 = TRUE)
  id <- .problem_output_identity(m, "utilization_by_group")
  expect_identical(id$axes, c("facility", "group"))
  expect_identical(id$ids, list(facility = p$facility_ids, group = p$group_ids))
  id2 <- .problem_output_identity(m, "access_by_group")
  expect_identical(id2$axes, c("origin", "group"))
  expect_identical(id2$ids, list(origin = p$origin_ids, group = p$group_ids))
  expect_identical(.problem_output_identity(m, "outside_share_by_group")$axes,
                   c("origin", "group"))
  # a J x G matrix with named axes binds by IDs, in any order
  obs <- matrix(c(5, 6, 7, 8, 9, 10), 3, 2,
                dimnames = list(p$facility_ids, p$group_ids))
  bound <- .bind_problem_target(m, obs, "utilization_by_group")
  expect_identical(bound$observed, obs)
  shuffled <- obs[c(3, 1, 2), c("b", "a")]
  rebound <- .bind_problem_target(m, shuffled, "utilization_by_group")
  expect_identical(rebound$observed, obs)
  expect_equal(rebound$values, as.numeric(obs))
  # the group axis (and the other axis) must be named
  expect_error(.bind_problem_target(m, unname(obs), "utilization_by_group"),
               "axis must be named")
  half <- obs
  colnames(half) <- NULL
  expect_error(.bind_problem_target(m, half, "utilization_by_group"),
               "observed group axis must be named")
  bad <- obs
  colnames(bad) <- c("a", "zz")
  expect_error(.bind_problem_target(m, bad, "utilization_by_group"),
               "observed group IDs must exactly match")
  expect_error(.bind_problem_target(m, obs[, 1], "utilization_by_group"),
               "observed shape does not match")
  # ungrouped identities are unchanged
  expect_identical(.problem_output_identity(m, "utilization")$axes, "facility")
  expect_identical(.problem_output_identity(m, "allocation")$axes, c("origin", "facility"))
})

test_that("clm_allocation refuses invalid grouped arguments", {
  f <- .group_prep_fixture()
  p <- .group_model_inputs(f)
  u <- prepare_allocation(f$demand$a, f$supply, f$M, missing_demand = "exclude")
  expect_error(clm_allocation(u, by_group = "sigma"), "need prepared inputs with demand groups")
  expect_error(clm_allocation(u, fixed = c(sigma = 2)), "need prepared inputs with demand groups")
  expect_error(clm_allocation(p, by_group = "v0"), "requires fit_v0 = TRUE")
  expect_error(clm_allocation(p, by_group = "beta"), "must be a subset")
  expect_error(clm_allocation(p, by_group = c("sigma", "sigma")), "must be a subset")
  expect_error(clm_allocation(p, family = list(a = "gaussian")),
               "family: give one element per group: a, b")
  expect_error(clm_allocation(p, family = list(a = "gaussian", c = "power")),
               "family: give one element per group: a, b")
  expect_error(clm_allocation(p, family = list(a = "gaussian", b = "power", c = "power")),
               "family: give one element per group: a, b")
  expect_error(clm_allocation(p, family = list("gaussian", "power")),
               "family: give one element per group: a, b")
  expect_error(clm_allocation(p, family = list(a = "gaussian", b = "nope")))
  # a name that is not a group is not per group
  expect_error(clm_allocation(p, fixed = c(nope = 1)), "must be among the parameters")
  expect_error(clm_allocation(p, fixed = c(v0 = 1)), "must be among the parameters")
  expect_error(clm_allocation(p, fixed = c(sigma.a = 1), by_group = "v0", fit_v0 = TRUE),
               "must be among the parameters")
  expect_error(clm_allocation(p, fixed = c(sigma = 0)), "outside the parameter bounds")
  expect_error(clm_allocation(p, fixed = c(sigma = -1)), "outside the parameter bounds")
  expect_error(clm_allocation(p, fit_v0 = TRUE, fixed = c(v0 = -1)),
               "outside the parameter bounds")
  expect_error(clm_allocation(p, fixed = c(sigma = 2)), "leaves no free parameter")
  expect_error(clm_allocation(p, fixed = 2), "named finite numeric")
  expect_error(clm_allocation(p, fixed = c(sigma = 2, sigma = 3), fit_v0 = TRUE),
               "named finite numeric")
  expect_error(clm_allocation(p, fixed = c(sigma = NA), fit_v0 = TRUE), "named finite numeric")
  # v0.<g> = 0 is allowed, a fixed sigma must be inside a declared kernel's bounds
  k <- decay_kernel(function(distance, sigma) exp(-distance / sigma), name = "bk",
                    parameter = "scale", lower = 1, upper = 10)
  expect_error(clm_allocation(p, family = k, by_group = "sigma",
                              fixed = c(sigma.a = 0.5)), "outside the parameter bounds")
  expect_no_error(clm_allocation(p, family = k, by_group = "sigma", fixed = c(sigma.a = 2)))
  # grouped inputs cannot go through the legacy and ungrouped constructors
  expect_error(.huff_problem(p, family = "gaussian"), "need clm_allocation")
})

test_that("fit_allocation refuses a smooth = FALSE kernel in any group's family", {
  f <- .group_prep_fixture()
  p <- .group_model_inputs(f)
  step_kernel <- decay_kernel(function(distance, sigma) as.numeric(distance <= sigma),
                              name = "step", parameter = "radius", smooth = FALSE,
                              lower = 0.5, upper = 50)
  m <- clm_allocation(p, family = list(a = "gaussian", b = step_kernel), v0 = 1)
  starts <- cbind(sigma.a = 3, sigma.b = 3)
  expect_error(fit_allocation(m, c(5, 5, 5), starts, c(sigma.a = 0.5, sigma.b = 0.5),
                              c(sigma.a = 20, sigma.b = 20)),
               "smooth = FALSE cannot be fitted")
  # the same kernel evaluates at fixed parameters
  expect_no_error(evaluate_allocation(m, c(sigma.a = 3, sigma.b = 4)))
  # shared declaration refused as before
  ms <- clm_allocation(p, family = step_kernel, v0 = 1)
  expect_error(fit_allocation(ms, c(5, 5, 5), cbind(sigma = 3), c(sigma = 0.5),
                              c(sigma = 20)), "smooth = FALSE cannot be fitted")
  expect_identical(.allocation_kernels(m$metadata$spec), list(step_kernel))
  expect_identical(.allocation_kernels(ms$metadata$spec), list(step_kernel, step_kernel,
                                                              step_kernel))
  u <- prepare_allocation(f$demand$a, f$supply, f$M, missing_demand = "exclude")
  expect_identical(.allocation_kernels(clm_allocation(u)$metadata$spec), list())
})

test_that("a grouped evaluation has no prediction snapshot", {
  f <- .group_prep_fixture()
  p <- .group_model_inputs(f)
  m <- clm_allocation(p, by_group = "sigma", fit_v0 = TRUE)
  e <- evaluate_allocation(m, c(sigma.a = 2, sigma.b = 3, v0 = 1))
  expect_null(e$prediction_snapshot)
  expect_null(e$evaluated_clm)
  expect_s3_class(e, "ae_equilibrium")
  expect_s3_class(e$coverage, "ae_coverage")
  expect_no_error(format(e$coverage))
  # an ungrouped evaluation keeps its snapshot
  u <- prepare_allocation(f$demand$a, f$supply, f$M, missing_demand = "exclude")
  eu <- evaluate_allocation(clm_allocation(u, fit_v0 = TRUE), c(sigma = 2, v0 = 1))
  expect_identical(eu$prediction_snapshot$schema, "spax_clm_prediction_v1")
  expect_false("utilization_by_group" %in% names(eu$outputs))
})

test_that("grouped evaluation reuses kernels of equal groups without changing results", {
  f <- .group_prep_fixture()
  # three groups: a and b share travel and sigma (the kernel is reused), c differs
  d <- f$demand
  d$c <- stats::setNames(c(2, 1, 0, 3, 1, 0), f$ids)
  p <- prepare_allocation(d, f$supply, f$M, missing_demand = "exclude")
  m <- clm_allocation(p, by_group = "sigma", fit_v0 = TRUE)
  theta <- c(sigma.a = 3, sigma.b = 3, sigma.c = 5, v0 = 2)
  oracle <- clm_group_oracle(p$distance_groups, p$D_groups, as.vector(p$S),
                             sigma = c(a = 3, b = 3, c = 5), v0 = 2)
  .expect_group_oracle(m, theta, oracle)
  expect_identical(p$group_ids, c("a", "b", "c"))
})

# Fitting grouped models to group-labelled targets (SPAX-057, T3) ---------------
# The generated case (18 origins, 24 facilities) split into three age bands whose
# shares vary across the origins (the first band is concentrated at one end, the
# last at the other), with group-specific true sigma 5 / 8 / 12 and a shared v0 of
# 25. Group demand is 20 times the generated demand so that the Poisson
# fluctuation is small: over 30 seeds the standard deviation of the fitted sigma
# was 0.11 / 0.28 / 0.51 and of v0 0.92 (about 2 / 3.5 / 4 / 3.7 % of the truth),
# so a 15% tolerance is 3.5 to 7 standard deviations.

.gfit_ids <- c("0-15", "16-64", "65+")
.gfit_truth <- c("sigma.0-15" = 5, "sigma.16-64" = 8, "sigma.65+" = 12, v0 = 25)

.gfit_case <- function() {
  alloc_memo("group_fit_case", {
    base <- alloc_generated_case()
    ids <- rownames(base$distance)
    t <- seq(0, 1, length.out = length(ids))
    share <- cbind(0.15 + 0.5 * t, 0.5 - 0.3 * t, 0.35 - 0.2 * t)
    share <- share / rowSums(share)
    demand <- stats::setNames(lapply(seq_along(.gfit_ids), function(k) {
      stats::setNames(20 * base$demand * share[, k], ids)
    }), .gfit_ids)
    inputs <- prepare_allocation(demand, base$supply, base$distance)
    model <- clm_allocation(inputs, by_group = "sigma", fit_v0 = TRUE)
    truth_eval <- evaluate_allocation(model, .gfit_truth,
      requested_outputs = "utilization_by_group")$outputs$utilization_by_group
    poisson <- function(x, seed) {
      withr::with_seed(seed, {
        x[] <- stats::rpois(length(x), x)
        x
      })
    }
    sigma_cols <- names(.gfit_truth)[1:3]
    list(base = base, inputs = inputs, model = model, expected = truth_eval,
         counts = poisson(truth_eval, 11), poisson = poisson,
         starts = rbind(centre = c(rep(7, 3), 20), spread = c(4, 12, 20, 50)) |>
           `colnames<-`(names(.gfit_truth)),
         lower = stats::setNames(rep(1, 4), names(.gfit_truth)),
         upper = stats::setNames(c(rep(40, 3), 200), names(.gfit_truth)),
         shared = clm_allocation(inputs, fit_v0 = TRUE),
         total_starts = rbind(centre = c(sigma = 7, v0 = 20), spread = c(sigma = 15, v0 = 60)),
         total_lower = c(sigma = 1, v0 = 1), total_upper = c(sigma = 40, v0 = 200))
  })
}

.gfit_control <- list(maxit = 300, factr = 10, pgtol = 1e-8)

.gfit_fit <- function(case, observed = case$counts, model = case$model,
                      starts = case$starts, output = "utilization_by_group",
                      gradient = TRUE, lower = case$lower, upper = case$upper,
                      loss = "poisson") {
  fit_allocation(model, observed, starts, lower, upper, output = output, loss = loss,
                 gradient = gradient, control = .gfit_control)
}

.gfit_fits <- function() {
  alloc_memo("group_fit_fits", {
    case <- .gfit_case()
    seconds <- function(expr) {
      started <- proc.time()[["elapsed"]]
      value <- expr
      list(fit = value, seconds = proc.time()[["elapsed"]] - started)
    }
    list(gradient = seconds(.gfit_fit(case)), finite = seconds(.gfit_fit(case, gradient = FALSE)),
         expected = seconds(.gfit_fit(case, case$expected)))
  })
}

.gfit_msg <- function(names) {
  paste0("group-specific parameters (", paste(names, collapse = ", "), ") need ",
         "group-labelled targets: fit output = \"utilization_by_group\" with observed ",
         "counts for every group, or fix them with clm_allocation(fixed = )")
}

.gfit_error <- function(expr) {
  tryCatch({ expr; NULL }, error = function(e) conditionMessage(e))
}

test_that("per-group totals recover the group sigma from a spread start", {
  case <- .gfit_case()
  fits <- .gfit_fits()
  expect_identical(dim(case$counts), c(24L, 3L))
  expect_identical(dimnames(case$counts), list(case$inputs$facility_ids, .gfit_ids))
  # expected totals: exact recovery from every start, one optimum
  e <- fits$expected$fit
  expect_s3_class(e, "ae_multistart_fit")
  expect_true(all(e$table$eligible))
  expect_false(isTRUE(e$parameter_disagreement))
  for (i in 1:2) {
    expect_equal(e$theta[i, ], .gfit_truth, tolerance = 1e-3)
  }
  expect_lt(abs(e$best$loss - .poisson_loss(as.numeric(case$expected),
                                            as.numeric(case$expected))), 1e-6)
  # Poisson draw: both starts reach the same optimum, near the truth
  for (key in c("gradient", "finite")) {
    f <- fits[[key]]$fit
    expect_true(all(f$table$eligible))
    expect_false(isTRUE(f$loss_disagreement))
    expect_false(isTRUE(f$parameter_disagreement))
    expect_false(any(f$boundary))
    est <- f$best$theta
    expect_identical(names(est), names(.gfit_truth))
    expect_true(all(abs(est / .gfit_truth - 1) < 0.15))
  }
  # the sigma are ordered like the truth: the groups differ
  est <- fits$gradient$fit$best$theta
  expect_true(est[["sigma.0-15"]] < est[["sigma.16-64"]] && est[["sigma.16-64"]] < est[["sigma.65+"]])
  # a fixed seed gives the same fit again (deterministic)
  expect_equal(.gfit_fit(case, starts = case$starts[2, , drop = FALSE])$best$theta, est,
               tolerance = 1e-4)
})

test_that("gradient = TRUE and FALSE agree on a group-labelled target", {
  fits <- .gfit_fits()
  a <- fits$gradient$fit$best
  b <- fits$finite$fit$best
  expect_equal(a$theta, b$theta, tolerance = 1e-4)
  expect_equal(a$loss, b$loss, tolerance = 1e-8)
  expect_true(a$gradient)
  expect_false(b$gradient)
  # the J x G target is bound, masked and predicted on the flattened matrix
  expect_identical(a$output, "utilization_by_group")
  expect_identical(dim(a$observed), c(24L, 3L))
  expect_identical(dim(a$predicted), c(24L, 3L))
  expect_identical(dimnames(a$predicted), list(.gfit_case()$inputs$facility_ids, .gfit_ids))
  expect_identical(a$target_alignment$axes, c("facility", "group"))
  expect_identical(a$target_alignment$ids$group, .gfit_ids)
  expect_identical(a$n_observed, 72L)
  expect_null(a$target_mask)
  # the analytic loss gradient and the finite differences give the same sensitivity
  case <- .gfit_case()
  th <- a$theta
  sens <- .problem_output_sensitivity(case$model, th,
    rep(1, 24), "utilization_by_group")
  expect_identical(dim(sens), c(72L, 4L))
  fd <- vapply(seq_along(th), function(k) {
    h <- 1e-5 * th[[k]]
    up <- down <- th
    up[[k]] <- th[[k]] + h
    down[[k]] <- th[[k]] - h
    f <- function(x) as.numeric(evaluate_allocation(case$model, x,
      requested_outputs = "utilization_by_group")$outputs$utilization_by_group)
    (f(up) - f(down)) / (2 * h)
  }, numeric(72))
  expect_equal(unname(sens), unname(fd), tolerance = 1e-4)
})

test_that("the fitted multistart exposes group-labelled results", {
  case <- .gfit_case()
  f <- .gfit_fits()$gradient$fit
  expect_identical(coef(f), f$best$theta)
  expect_identical(dim(fitted(f)), c(24L, 3L))
  # the Poisson floor and the residual summary read the flattened matrix
  floor <- .poisson_floor(f$best)
  expect_identical(floor[["positive_below"]], 0)
  s <- summary(f$best)
  expect_identical(s$groups, .gfit_ids)
  expect_identical(s$by_group, "sigma")
  expect_s3_class(s$residual, "data.frame")
  # the parameter table keeps the names of group parameters
  tab <- as.data.frame(f)
  expect_identical(tab$parameter, names(.gfit_truth))
  expect_identical(names(tab), c("parameter", "estimate"))
  # plot of observed against fitted counts runs on the J x G matrix
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  expect_no_error(plot(f))
  # the profile of the shared v0 runs on the group-labelled target
  p <- .profile_problem(case$model, case$counts, "v0", c(20, 30),
    init = f$best$theta, lower = case$lower, upper = case$upper,
    output = "utilization_by_group", loss = .poisson_loss, loss_grad = .poisson_gradient,
    loss_args = list(eps = 1e-250), gradient = TRUE, control = .gfit_control)
  expect_true(all(p$table$eligible))
  expect_true("sigma.65+" %in% names(p$table))
})

test_that("a grouped model with shared parameters fits aggregated targets like the ungrouped model", {
  case <- .gfit_case()
  totals <- stats::setNames(rowSums(case$expected), case$inputs$facility_ids)
  grouped <- .gfit_fit(case, totals, case$shared, case$total_starts, "utilization",
                       lower = case$total_lower, upper = case$total_upper)
  expect_true(all(grouped$table$eligible))
  ungrouped_inputs <- prepare_allocation(case$inputs$D_active |>
    stats::setNames(case$inputs$origin_ids), case$base$supply, case$base$distance)
  ungrouped <- .gfit_fit(case, totals, clm_allocation(ungrouped_inputs, fit_v0 = TRUE),
                         case$total_starts, "utilization", lower = case$total_lower,
                         upper = case$total_upper)
  expect_equal(grouped$best$theta, ungrouped$best$theta, tolerance = 1e-5)
  expect_equal(grouped$best$loss, ungrouped$best$loss, tolerance = 1e-8)
  expect_equal(unname(grouped$best$predicted), unname(ungrouped$best$predicted),
               tolerance = 1e-6)
  expect_identical(grouped$best$groups, .gfit_ids)
  expect_identical(grouped$best$by_group, character())
  # a flow target is aggregated as well
  flow <- evaluate_allocation(case$shared, c(sigma = 8, v0 = 25),
                              requested_outputs = "flow")$outputs$flow
  flow_fit <- .gfit_fit(case, round(flow), case$shared, case$total_starts, "flow",
                        lower = case$total_lower, upper = case$total_upper)
  expect_true(all(flow_fit$table$eligible))
  expect_equal(unname(flow_fit$best$theta[["sigma"]]), 8, tolerance = 0.02)
})

test_that("equal true sigma: a by_group fit gains little over the shared fit", {
  case <- .gfit_case()
  equal <- c(rep(8, 3), 25)
  names(equal) <- names(.gfit_truth)
  expected <- evaluate_allocation(case$model, equal,
    requested_outputs = "utilization_by_group")$outputs$utilization_by_group
  counts <- case$poisson(expected, 3)
  by_group <- .gfit_fit(case, counts)$best
  shared_start <- case$total_starts
  shared <- .gfit_fit(case, counts, case$shared, shared_start, "utilization_by_group",
                      lower = case$total_lower, upper = case$total_upper)$best
  # two extra free parameters: a gain of 3 in the Poisson loss would be the 5% chi-square_2 point
  expect_gte(shared$loss - by_group$loss, -1e-6)
  expect_lt(shared$loss - by_group$loss, 3)
  expect_true(all(abs(by_group$theta[1:3] / 8 - 1) < 0.1))
  # with the truth sigma unequal the shared fit loses much
  unequal <- .gfit_fit(case, case$counts, case$shared, shared_start, "utilization_by_group",
                       lower = case$total_lower, upper = case$total_upper)$best
  expect_gt(unequal$loss - .gfit_fits()$gradient$fit$best$loss, 10)
})

test_that("group-specific free parameters need group-labelled targets (D2)", {
  case <- .gfit_case()
  fits <- .gfit_fits()
  agg <- rowSums(case$counts)
  names(agg) <- case$inputs$facility_ids
  msg <- .gfit_msg(c("sigma.0-15", "sigma.16-64", "sigma.65+"))
  expect_identical(.gfit_error(fit_allocation(case$model, agg, case$starts, case$lower,
    case$upper, loss = "poisson")), msg)
  # the aggregated flow target as well
  flow <- evaluate_allocation(case$model, .gfit_truth, requested_outputs = "flow")$outputs$flow
  expect_identical(.gfit_error(fit_allocation(case$model, round(flow), case$starts,
    case$lower, case$upper, output = "flow", loss = "poisson")), msg)
  # profile, nfxp and the already-bound branch refuse as well, before any start
  expect_identical(.gfit_error(.profile_problem(case$model, agg, "v0", c(20, 30),
    init = case$starts[1, ], lower = case$lower, upper = case$upper)), msg)
  expect_identical(.gfit_error(.fit_problem_nfxp(case$model, agg, case$starts[1, ],
    case$lower, case$upper)), msg)
  bound <- .bind_problem_target(case$shared, agg, "utilization")
  expect_identical(.gfit_error(.bind_problem_target(case$model, bound, "utilization")), msg)
  expect_s3_class(.bind_problem_target(case$shared, bound, "utilization"), "ae_bound_target")
  # another per-group output is not a group-labelled count target either
  access <- matrix(0.5, 18, 3, dimnames = list(case$inputs$origin_ids, .gfit_ids))
  expect_identical(.gfit_error(.bind_problem_target(case$model, access, "access_by_group")), msg)
  # an unnamed group axis and an incomplete one are refused by the binder
  expect_match(.gfit_error(fit_allocation(case$model, unname(case$counts), case$starts,
    case$lower, case$upper, output = "utilization_by_group")), "must be named")
  expect_match(.gfit_error(fit_allocation(case$model, case$counts[, 1:2], case$starts,
    case$lower, case$upper, output = "utilization_by_group")), "group dimension")
  # the refusal is raised, not captured as a failed start: no result is returned
  expect_null(tryCatch(fit_allocation(case$model, agg, case$starts, case$lower, case$upper),
                       error = function(e) NULL))
  # a v0 for each group needs labelled targets as well; a shared sigma alone does not
  by_v0 <- clm_allocation(case$inputs, by_group = "v0", fit_v0 = TRUE)
  expect_identical(.gfit_error(.bind_problem_target(by_v0, agg, "utilization")),
                   .gfit_msg(c("v0.0-15", "v0.16-64", "v0.65+")))
  expect_s3_class(.bind_problem_target(case$shared, agg, "utilization"), "ae_bound_target")
  # a group column without observed cells, or a group without demand, is refused
  half <- case$counts
  half[, "16-64"] <- NA
  err <- .gfit_error(fit_allocation(case$model, half, case$starts, case$lower, case$upper,
    output = "utilization_by_group", loss = "poisson"))
  expect_match(err, .gfit_msg(names(.gfit_truth)[1:3]), fixed = TRUE)
  expect_match(err, "no observed cell: 16-64", fixed = TRUE)
  # an NA in some cells only is allowed
  some <- case$counts
  some[c(1, 5, 30)] <- NA
  expect_s3_class(.bind_problem_target(case$model, some, "utilization_by_group"),
                  "ae_bound_target")
  # every group with a free group parameter needs an observed cell, not the fixed ones
  fixed_b <- clm_allocation(case$inputs, by_group = "sigma", fit_v0 = TRUE,
                            fixed = c("sigma.16-64" = 8))
  expect_s3_class(.bind_problem_target(fixed_b, half, "utilization_by_group"), "ae_bound_target")
  demand <- lapply(stats::setNames(.gfit_ids, .gfit_ids), function(g) {
    d <- case$inputs$D_groups[, g]
    if (g == "65+") d[] <- 0
    d
  })
  empty <- clm_allocation(prepare_allocation(demand, case$base$supply, case$base$distance),
                          by_group = "sigma", fit_v0 = TRUE)
  err <- .gfit_error(.bind_problem_target(empty, case$counts, "utilization_by_group"))
  expect_match(err, .gfit_msg(names(.gfit_truth)[1:3]), fixed = TRUE)
  expect_match(err, "no positive active demand: 65+", fixed = TRUE)
  # ungrouped problems return at once
  ungrouped <- clm_allocation(prepare_allocation(case$base$demand, case$base$supply,
    case$base$distance), fit_v0 = TRUE)
  expect_identical(.check_group_targets(ungrouped, NULL), TRUE)
})

test_that("fixed group parameters and shared parameters take aggregated targets", {
  case <- .gfit_case()
  agg <- stats::setNames(rowSums(case$expected), case$inputs$facility_ids)
  fixed <- clm_allocation(case$inputs, by_group = "sigma", fit_v0 = TRUE,
                          fixed = .gfit_truth[1:3])
  expect_identical(fixed$theta$names, "v0")
  f <- fit_allocation(fixed, agg, matrix(c(15, 40), ncol = 1, dimnames = list(NULL, "v0")),
                      c(v0 = 1), c(v0 = 200), loss = "poisson", gradient = TRUE,
                      control = .gfit_control)
  expect_true(all(f$table$eligible))
  expect_equal(f$best$theta[["v0"]], 25, tolerance = 1e-3)
  # the grouped markers say what was held fixed
  expect_identical(f$best$fixed, .gfit_truth[1:3])
  # group-labelled targets are allowed with fixed parameters as well
  g <- fit_allocation(fixed, case$expected, matrix(c(15, 40), ncol = 1, dimnames = list(NULL, "v0")),
                      c(v0 = 1), c(v0 = 200), output = "utilization_by_group", loss = "poisson",
                      gradient = TRUE, control = .gfit_control)
  expect_equal(g$best$theta[["v0"]], 25, tolerance = 1e-3)
  # one free group parameter among fixed ones still needs the labelled target
  one <- clm_allocation(case$inputs, by_group = "sigma", fit_v0 = TRUE,
                        fixed = .gfit_truth[1:2])
  expect_identical(.gfit_error(.bind_problem_target(one, agg, "utilization")),
                   .gfit_msg("sigma.65+"))
})

test_that("grouped fits and equilibria carry the group markers and no snapshot", {
  case <- .gfit_case()
  f <- .gfit_fits()$gradient$fit$best
  markers <- c("groups", "by_group", "families", "fixed")
  expect_identical(utils::tail(names(f), 4L), markers)
  expect_identical(f$groups, .gfit_ids)
  expect_identical(f$by_group, "sigma")
  expect_identical(names(f$families), .gfit_ids)
  expect_identical(f$families[[1]], "gaussian")
  expect_identical(f$fixed, stats::setNames(numeric(), character()))
  expect_true("prediction_snapshot" %in% names(f))
  expect_null(f$prediction_snapshot)
  expect_true(all(c(markers, "prediction_snapshot") %in% names(f$equilibrium)))
  expect_null(f$equilibrium$prediction_snapshot)
  e <- evaluate_allocation(case$model, .gfit_truth)
  expect_true(all(c(markers, "prediction_snapshot") %in% names(e)))
  expect_null(e$prediction_snapshot)
  expect_identical(e$groups, .gfit_ids)
  # ungrouped objects have none of them
  u <- alloc_generated_fit()$best
  expect_false(any(markers %in% names(u)))
  expect_false(any(markers %in% names(u$equilibrium)))
  ue <- evaluate_allocation(alloc_generated_case()$model, c(sigma = 8, v0 = 25))
  expect_false(any(c(markers, "prediction_snapshot") %in% names(ue)[
    vapply(ue, is.null, logical(1))]))
  expect_false(any(markers %in% names(ue)))
  expect_identical(ue$prediction_snapshot$schema, "spax_clm_prediction_v1")
})

test_that("predict, check, bootstrap and inference refuse grouped models", {
  case <- .gfit_case()
  fit <- .gfit_fits()$gradient$fit
  e <- evaluate_allocation(case$model, .gfit_truth)
  yet <- function(fn) paste0(fn, " is not available for grouped models yet")
  distance <- case$base$distance
  expect_error(predict_allocation(fit, distance), yet("predict_allocation()"), fixed = TRUE)
  expect_error(predict_allocation(fit$best, distance), yet("predict_allocation()"), fixed = TRUE)
  expect_error(predict_allocation(e, distance), yet("predict_allocation()"), fixed = TRUE)
  expect_error(check_allocation(case$model, fit), yet("check_allocation()"), fixed = TRUE)
  expect_error(bootstrap_allocation(case$model, fit, alloc_bootstrap_observation(), seed = 1),
               yet("bootstrap_allocation()"), fixed = TRUE)
  expect_error(.allocation_observation(case$model, case$counts, event = "visits",
    exposure_units = "people", period = "year", exposure = "poisson_mean"),
    yet(".allocation_observation()"), fixed = TRUE)
  expect_error(.allocation_inference(case$model, fit,
    structure(list(), class = "allocation_observation")),
    yet(".allocation_inference()"), fixed = TRUE)
  # the legacy and raster routes refuse grouped prepared inputs
  expect_error(.sae_problem(case$inputs), yet("SAE"), fixed = TRUE)
  expect_error(.haae_problem(case$inputs), yet("HAAE"), fixed = TRUE)
  expect_error(.huff_problem(case$inputs, allocation = "huff_decay"),
               yet("the huff_decay allocation"), fixed = TRUE)
  expect_error(.ratio_problem(case$inputs), yet("the ratio response"), fixed = TRUE)
  # a model whose objects are not grouped is not refused
  expect_false(.is_grouped_object(alloc_generated_fit()))
  expect_false(.is_grouped_object(alloc_generated_case()$model))
  expect_true(.is_grouped_object(case$model))
  expect_true(.is_grouped_object(case$inputs))
})

test_that("grouped fits, summaries and evaluations print their groups and parameters", {
  case <- .gfit_case()
  fit <- .gfit_fits()$gradient$fit
  out <- capture.output(print(fit$best))
  expect_true(any(out == "groups: 0-15, 16-64, 65+"))
  expect_true(any(out == "by_group: sigma"))
  expect_true(any(startsWith(out, "  sigma.0-15: ")))
  expect_true(any(startsWith(out, "  sigma.65+: ")))
  expect_true(any(startsWith(out, "  v0: ")))
  expect_true(any(out == "family: gaussian (sigma = travel scale)"))
  expect_false(any(startsWith(out, "fixed:")))
  out <- capture.output(print(fit))
  expect_true(any(out == "groups: 0-15, 16-64, 65+"))
  out <- capture.output(print(summary(fit$best)))
  expect_true(any(out == "groups: 0-15, 16-64, 65+"))
  expect_true(any(grepl("sigma.16-64", out, fixed = TRUE)))
  out <- capture.output(print(summary(fit)))
  expect_true(any(out == "by_group: sigma"))
  # fixed values are listed after theta
  fixed <- clm_allocation(case$inputs, by_group = "sigma", fit_v0 = TRUE,
                          fixed = c("sigma.0-15" = 5, "sigma.65+" = 12))
  free <- fixed$theta$names
  expect_identical(free, c("sigma.16-64", "v0"))
  f <- fit_allocation(fixed, case$expected, matrix(c(8, 25, 9, 40), 2, byrow = TRUE,
        dimnames = list(NULL, free)), stats::setNames(c(1, 1), free),
        stats::setNames(c(40, 200), free), output = "utilization_by_group",
        loss = "poisson", gradient = TRUE, control = .gfit_control)
  expect_equal(f$best$theta[["sigma.16-64"]], 8, tolerance = 1e-3)
  out <- capture.output(print(f$best))
  at <- which(out == "fixed:")
  expect_length(at, 1L)
  expect_identical(out[at + 1:2], c("  sigma.0-15: 5", "  sigma.65+: 12"))
  expect_gt(at, which(out == "theta:"))
  expect_true(any(capture.output(print(summary(f$best))) == "fixed:"))
  expect_identical(summary(f$best)$fixed$parameter, c("sigma.0-15", "sigma.65+"))
  expect_identical(as.data.frame(f)$parameter, c("sigma.16-64", "v0"))
  # evaluations show them through the coverage format
  e <- evaluate_allocation(fixed, c("sigma.16-64" = 8, v0 = 25))
  expect_match(format(e$coverage),
    "[groups 0-15, 16-64, 65+] (sigma.16-64=8, v0=25; fixed sigma.0-15=5, sigma.65+=12)",
    fixed = TRUE)
  expect_true(any(grepl("[groups 0-15, 16-64, 65+]", capture.output(print(e$coverage)),
                        fixed = TRUE)))
  # ungrouped printing is unchanged: no group lines
  u <- capture.output(print(alloc_generated_fit()$best))
  expect_false(any(grepl("^(groups|by_group|fixed)", u)))
  expect_match(format(evaluate_allocation(alloc_generated_case()$model,
    c(sigma = 8, v0 = 25))$coverage), "^<ae_coverage> huff/gaussian \\(sigma=8, v0=25\\)")
})

test_that("a family by group is labelled by group", {
  case <- .gfit_case()
  fam <- clm_allocation(case$inputs, fit_v0 = TRUE,
    family = list("0-15" = "gaussian", "16-64" = "exponential", "65+" = "gaussian"))
  expect_identical(fam$metadata$spec$family, "grouped")
  truth <- c("sigma.0-15" = 5, "sigma.16-64" = 0.15, "sigma.65+" = 12, v0 = 25)
  expected <- evaluate_allocation(fam, truth,
    requested_outputs = "utilization_by_group")$outputs$utilization_by_group
  starts <- matrix(c(6, 0.2, 10, 30), 1, dimnames = list(NULL, names(truth)))
  f <- fit_allocation(fam, expected, starts, c(1, 0.01, 1, 1) |> stats::setNames(names(truth)),
    c(40, 5, 40, 200) |> stats::setNames(names(truth)), output = "utilization_by_group",
    loss = "poisson", gradient = TRUE, control = .gfit_control)
  expect_equal(f$best$theta, truth, tolerance = 1e-2)
  label <- paste0("by group (0-15: gaussian (sigma = travel scale); ",
                  "16-64: exponential (sigma = rate); 65+: gaussian (sigma = travel scale))")
  expect_identical(.fit_family_label(f$best), label)
  out <- capture.output(print(f$best))
  expect_true(any(out == paste0("family: ", label)))
  expect_identical(summary(f$best)$family_label, label)
  expect_true(any(capture.output(print(summary(f$best))) == paste0("family: ", label)))
  # a declared kernel of a group gives its own meaning of sigma
  kernel <- decay_kernel(function(distance, sigma) exp(-distance / sigma), name = "soft",
                         parameter = "decay length")
  expect_identical(.group_family_label(kernel), "soft (sigma = decay length)")
  expect_identical(.group_family_label("power"), "power (sigma = exponent)")
  expect_identical(.fit_family_label(list(family = "soft", families = list(a = kernel))),
                   "soft (sigma = decay length)")
})

# compare_allocations() for grouped fits (SPAX-057 T4) --------------------------

# One start and the same targets keep these fits cheap; they need not be good,
# only comparable or not.
.gcmp_fit <- function(case, model, observed = case$counts, output = "utilization_by_group") {
  .gfit_fit(case, observed, model, case$starts[2L, , drop = FALSE], output)
}

.gcmp_models <- function(case, distance = case$base$distance, demand = NULL) {
  D <- if (is.null(demand)) case$inputs$D_groups else demand
  groups <- stats::setNames(lapply(colnames(D), function(g) D[, g]), colnames(D))
  inputs <- prepare_allocation(groups, case$base$supply, distance)
  clm_allocation(inputs, by_group = "sigma", fit_v0 = TRUE)
}

test_that("a shared and a by_group fit on the same labelled target are comparable", {
  case <- .gfit_case()
  by_group <- .gfit_fits()$gradient$fit
  shared <- .gfit_fit(case, case$counts, case$shared, case$total_starts,
                      "utilization_by_group", lower = case$total_lower,
                      upper = case$total_upper)
  fits <- list(shared = shared, by_group = by_group)
  models <- list(shared = case$shared, by_group = case$model)
  # without models the demand by group and the travel cannot be checked
  none <- as.data.frame(compare_allocations(fits))
  expect_false(any(none$comparable))
  expect_match(none$not_comparable[2], "demand by group and travel not checked")
  cmp <- compare_allocations(fits, models = models)
  tab <- as.data.frame(cmp)
  expect_true(all(tab$comparable))
  expect_identical(tab$not_comparable, c("", ""))
  expect_identical(tab$loss_difference[1], 0)
  expect_equal(tab$loss_difference[2], by_group$best$loss - shared$best$loss)
  expect_lt(tab$loss_difference[2], -10)
  # the free parameters, named by group, and the pooled coverage
  expect_match(tab$parameters[1], "^sigma=[0-9.]+, v0=")
  expect_match(tab$parameters[2], "sigma.0-15=.*sigma.16-64=.*sigma.65\\+=.*v0=")
  expect_equal(tab$contact[2], .ae_coverage(by_group$best)$regional$rho[1])
  printed <- capture.output(print(cmp))
  expect_match(printed[1], "reference: shared")
  expect_true(any(grepl("by_group", printed)))
  expect_false(any(grepl("like-for-like", printed)))
  # the reference may be the by_group fit
  rev <- as.data.frame(compare_allocations(fits, models = models, reference = "by_group"))
  expect_equal(rev$loss_difference[1], shared$best$loss - by_group$best$loss)
  expect_true(all(rev$comparable))
  # a comparison with a withheld difference says so
  expect_output(print(compare_allocations(fits)), "like-for-like")
})

test_that("grouped and ungrouped fits are not comparable", {
  case <- .gfit_case()
  by_group <- .gfit_fits()$gradient$fit
  ungrouped_case <- alloc_generated_case()
  ungrouped <- alloc_generated_fit()
  cmp <- as.data.frame(compare_allocations(list(a = by_group, b = ungrouped),
                                           models = list(a = case$model,
                                                         b = ungrouped_case$model)))
  expect_false(cmp$comparable[2])
  expect_true(is.na(cmp$loss_difference[2]))
  expect_match(cmp$not_comparable[2], "groups")
  # the same with the ungrouped fit as the reference
  cmp <- as.data.frame(compare_allocations(list(b = ungrouped, a = by_group),
                                           models = list(b = ungrouped_case$model,
                                                         a = case$model)))
  expect_false(cmp$comparable[2])
  expect_match(cmp$not_comparable[2], "groups")
  # two ungrouped fits next to a grouped one still compare with each other
  cmp <- as.data.frame(compare_allocations(
    list(u1 = ungrouped, u2 = ungrouped, g = by_group),
    models = list(u1 = ungrouped_case$model, u2 = ungrouped_case$model, g = case$model)))
  expect_identical(cmp$comparable, c(TRUE, TRUE, FALSE))
})

test_that("a different split of the same totals is not comparable", {
  case <- .gfit_case()
  by_group <- .gfit_fits()$gradient$fit
  D <- case$inputs$D_groups
  # the groups' shares rotated: the same totals by origin, other demand by group
  rotated <- D[, c(2L, 3L, 1L)]
  colnames(rotated) <- colnames(D)
  other_model <- .gcmp_models(case, demand = rotated)
  expect_equal(other_model$substrate$D_active, case$model$substrate$D_active)
  expect_false(identical(other_model$substrate$D_groups, case$model$substrate$D_groups))
  other <- .gcmp_fit(case, other_model)
  tab <- as.data.frame(compare_allocations(list(a = by_group, b = other),
                                           models = list(a = case$model, b = other_model)))
  expect_false(tab$comparable[2])
  expect_true(is.na(tab$loss_difference[2]))
  expect_match(tab$not_comparable[2], "demand_groups")
})

test_that("different travel by group is not comparable when models are given", {
  case <- .gfit_case()
  by_group <- .gfit_fits()$gradient$fit
  d <- case$base$distance
  per_group <- stats::setNames(list(d * 1.1, d, d), .gfit_ids)
  inputs <- prepare_allocation(
    stats::setNames(lapply(.gfit_ids, function(g) case$inputs$D_groups[, g]), .gfit_ids),
    case$base$supply, per_group)
  other_model <- clm_allocation(inputs, by_group = "sigma", fit_v0 = TRUE)
  expect_null(other_model$substrate$distance_active)
  other <- .gcmp_fit(case, other_model)
  tab <- as.data.frame(compare_allocations(list(a = by_group, b = other),
                                           models = list(a = case$model, b = other_model)))
  expect_false(tab$comparable[2])
  expect_match(tab$not_comparable[2], "travel")
  # the same travel given for every group separately is the shared travel
  same_inputs <- prepare_allocation(
    stats::setNames(lapply(.gfit_ids, function(g) case$inputs$D_groups[, g]), .gfit_ids),
    case$base$supply, stats::setNames(list(d, d, d), .gfit_ids))
  same_model <- clm_allocation(same_inputs, by_group = "sigma", fit_v0 = TRUE)
  same <- .gcmp_fit(case, same_model)
  tab <- as.data.frame(compare_allocations(list(a = by_group, b = same),
                                           models = list(a = case$model, b = same_model)))
  expect_true(all(tab$comparable))
})

test_that("an aggregated and a group-labelled target are not comparable", {
  case <- .gfit_case()
  by_group <- .gfit_fits()$gradient$fit
  totals <- rowSums(case$counts)
  fixed_model <- clm_allocation(case$inputs, fit_v0 = TRUE)
  aggregated <- fit_allocation(fixed_model, totals, case$total_starts[2, , drop = FALSE],
                               case$total_lower, case$total_upper, output = "utilization",
                               loss = "poisson", control = .gfit_control)
  tab <- as.data.frame(compare_allocations(list(a = by_group, b = aggregated),
                                           models = list(a = case$model, b = fixed_model)))
  expect_false(tab$comparable[2])
  expect_match(tab$not_comparable[2], "observed")
  expect_match(tab$not_comparable[2], "loss")
})

test_that("a grouped fit must belong to its model when models are given", {
  case <- .gfit_case()
  by_group <- .gfit_fits()$gradient$fit
  expect_error(compare_allocations(list(a = by_group, b = by_group),
                                   models = list(a = case$model, b = case$shared)),
               "does not belong")
  expect_error(compare_allocations(list(a = by_group, b = by_group),
                                   models = list(a = case$model,
                                                 b = alloc_generated_case()$model)),
               "does not belong")
  # a model with another family per group does not reproduce the fit
  other <- clm_allocation(case$inputs, by_group = "sigma", fit_v0 = TRUE,
                          family = stats::setNames(list("exponential", "gaussian", "gaussian"),
                                                   .gfit_ids))
  expect_error(compare_allocations(list(a = by_group, b = by_group),
                                   models = list(a = case$model, b = other)),
               "does not belong")
})

test_that("a group's own access is NA where it has no demand and unusable travel", {
  f <- .group_prep_fixture()
  Ma <- f$M
  Ma["o3", "f1"] <- NA              # group a has no demand at o3; group b does
  inputs <- prepare_allocation(f$demand, f$supply, list(a = Ma, b = f$M),
                               missing_demand = "exclude")
  model <- clm_allocation(inputs, family = "gaussian", fit_v0 = TRUE)
  ev <- evaluate_allocation(model, c(sigma = 3, v0 = 2), requested_outputs =
                              c("access_by_group", "outside_share_by_group"))
  acc <- ev$outputs$access_by_group
  out <- ev$outputs$outside_share_by_group
  expect_true(is.na(acc["o3", "a"]) && is.na(out["o3", "a"]))
  expect_false(anyNA(acc[, "b"]) || anyNA(out[, "b"]))
  expect_false(anyNA(acc[setdiff(rownames(acc), "o3"), "a"]))
  # Pooled outputs are unaffected: o3's demand is all in group b.
  expect_false(anyNA(ev$outputs$access))
})

test_that("printing an ungrouped multistart adds no blank line", {
  out <- utils::capture.output(print(alloc_small_fit()))
  at <- grep("^loss disagreement", out)
  expect_false(identical(out[at + 1L], ""))
})

test_that("a data frame of demand is refused with a plain message", {
  f <- .group_prep_fixture()
  expect_error(prepare_allocation(as.data.frame(f$demand), f$supply, f$M),
               "named list of numeric vectors")
})
