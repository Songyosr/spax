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
