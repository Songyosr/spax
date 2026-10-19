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
  # group b has no edge left at o4: it contacts nothing and is all outside
  expect_identical(unname(e$outputs$access_by_group["o4", "b"]), 0)
  expect_equal(unname(e$outputs$outside_share_by_group["o4", "b"]), 1)
  expect_true(all(is.finite(e$outputs$access_by_group)))
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
