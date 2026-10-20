# Observations for the allocation interface (SPAX-058, T4): obs_flows(),
# obs_totals(), the canonical targets that fit_allocation() binds, and the reading
# of raw observed inputs. The synthetic cases always run; the Cornwall cases are
# skipped on CRAN.

# Fixtures ----------------------------------------------------------------------

# Six origins and three facilities (in the order f2, f1, f3 of the data). Origin
# o1, o3 and o5 are active; o2 has zero demand, o4 unknown demand and o6 is
# excluded by `included`.
.obs_case <- function(supply = c(f2 = 18, f1 = 25, f3 = 15), travel = NULL,
                      missing_travel = "error") {
  demand <- c(o1 = 40, o2 = 0, o3 = 55, o4 = NA, o5 = 35, o6 = 20)
  if (is.null(travel)) {
    travel <- matrix(c(1, 5, 3, 4, 6, 2,  6, 2, 2, 3, 4, 3,  4, 6, 1, 2, 5, 4), 6, 3,
                     dimnames = list(names(demand), names(supply)))
  }
  prepare_allocation(demand, supply, travel, missing_demand = "exclude",
                     included = c(TRUE, TRUE, TRUE, TRUE, TRUE, FALSE),
                     missing_travel = missing_travel)
}

# The same with two demand groups (o4 unknown in group b; o2 zero in both).
.obs_case_groups <- function(supply = c(f2 = 18, f1 = 25, f3 = 15)) {
  ids <- paste0("o", 1:6)
  a <- stats::setNames(c(15, 0, 25, 10, 20, 8), ids)
  b <- stats::setNames(c(25, 0, 30, NA, 15, 12), ids)
  travel <- matrix(c(1, 5, 3, 4, 6, 2,  6, 2, 2, 3, 4, 3,  4, 6, 1, 2, 5, 4), 6, 3,
                   dimnames = list(ids, names(supply)))
  prepare_allocation(list(a = a, b = b), supply, travel, missing_demand = "exclude",
                     included = c(TRUE, TRUE, TRUE, TRUE, TRUE, FALSE))
}

# Flows with rows and columns in a different order from the data, an origin and a
# facility outside the data, and every kind of dropped origin.
.obs_flow_matrix <- function() {
  rows <- c("o5", "zz", "o1", "o2", "o4", "o6", "o3")
  cols <- c("f3", "out", "f1", "f2")
  matrix(c(5, 100, 2, 0, 10, 1, 1,    # f3
           1, 7, 0, 0, 0, 2, NA,      # out
           4, 0, 6, 0, 20, 2, 8,      # f1
           3, 5, 7, 0, 30, 3, 9),     # f2
         7, 4, dimnames = list(rows, cols))
}

# What .obs_flow_matrix() becomes for .obs_case(): active origins o1, o3, o5 by
# facilities f2, f1, f3.
.obs_flow_expected <- function() {
  matrix(c(7, 9, 3,  6, 8, 4,  2, 1, 5), 3, 3,
         dimnames = list(c("o1", "o3", "o5"), c("f2", "f1", "f3")))
}

.obs_long <- function(m) {
  data.frame(lsoa = rep(rownames(m), ncol(m)), practice = rep(colnames(m), each = nrow(m)),
             patients = as.vector(m))
}

# Run `expr`, collecting its messages.
.obs_messages <- function(expr) {
  out <- character()
  value <- withCallingHandlers(expr, message = function(m) {
    out <<- c(out, conditionMessage(m))
    invokeRestart("muffleMessage")
  })
  list(value = value, messages = out)
}

# The message of the error of `expr` (NULL when there is none); messages that
# precede the error are muffled.
.obs_error <- function(expr) .obs_error_messages(expr)$error

# The error message of `expr` and the messages that preceded it.
.obs_error_messages <- function(expr) {
  messages <- character()
  error <- tryCatch({
    withCallingHandlers(expr, message = function(m) {
      messages <<- c(messages, conditionMessage(m))
      invokeRestart("muffleMessage")
    })
    NULL
  }, error = function(e) conditionMessage(e))
  list(error = error, messages = messages)
}

# obs_flows() --------------------------------------------------------------------

test_that("obs_flows() stores a matrix as a plain double matrix", {
  m <- .obs_flow_matrix()
  obs <- obs_flows(m)
  expect_s3_class(obs, "spax_obs")
  expect_identical(names(obs), c("type", "x", "loss", "loss_args"))
  expect_identical(obs$type, "flow")
  expect_identical(obs$x, m)
  expect_identical(obs$loss, "poisson")
  expect_identical(obs$loss_args, list())
  im <- m
  storage.mode(im) <- "integer"
  im[is.na(im)] <- 0L
  expect_type(obs_flows(im)$x, "double")
  expect_identical(obs_flows(m, loss = "weighted_sse", loss_args = list(eta = 2))[c("loss", "loss_args")],
                   list(loss = "weighted_sse", loss_args = list(eta = 2)))
  expect_match(.obs_error(obs_flows(m, loss = "mse")), "should be one of", fixed = TRUE)
  expect_identical(.obs_error(obs_flows(m, loss_args = list(1))), "loss_args must be a named list")
  expect_identical(.obs_error(obs_flows(m, loss_args = 2)), "loss_args must be a named list")
})

test_that("obs_flows() reads table and xtabs objects as matrices", {
  d <- data.frame(o = c("o1", "o1", "o2", "o3"), f = c("f1", "f2", "f1", "f2"),
                  n = c(4, 3, 2, 1))
  tab <- table(d$o, d$f)
  from_table <- obs_flows(tab)
  expect_identical(from_table$x, matrix(c(1, 1, 0, 1, 0, 1), 3, 2,
    dimnames = list(c("o1", "o2", "o3"), c("f1", "f2"))))
  xt <- xtabs(n ~ o + f, d)
  from_xtabs <- obs_flows(xt)
  expected <- matrix(c(4, 2, 0, 3, 0, 1), 3, 2,
                     dimnames = list(c("o1", "o2", "o3"), c("f1", "f2")))
  expect_identical(from_xtabs$x, expected)
  expect_type(from_xtabs$x, "double")
  # no class, call or dimnames names are kept
  expect_identical(names(attributes(from_xtabs$x)), c("dim", "dimnames"))
  expect_null(names(dimnames(from_xtabs$x)))
  expect_identical(obs_flows(xt)$x, obs_flows(expected)$x)
})

test_that("obs_flows() reads a data frame by position or by column name", {
  d <- data.frame(o = c("o1", "o1", "o2"), f = c("f1", "f2", "f1"), n = c(4, 3, 2))
  obs <- obs_flows(d)
  expect_identical(obs$x, data.frame(origin = c("o1", "o1", "o2"), facility = c("f1", "f2", "f1"),
                                     count = c(4, 3, 2)))
  expect_identical(obs_flows(d[c("n", "f", "o")], origin = "o", facility = "f", count = "n")$x, obs$x)
  # a frame with more columns is read by the named columns
  wide <- cbind(d, extra = 1:3)
  expect_identical(obs_flows(wide, origin = "o", facility = "f", count = "n")$x, obs$x)
  # factors and integer counts
  fac <- data.frame(o = factor(c("o2", "o1")), f = factor(c("f1", "f1")), n = c(1L, 2L))
  expect_identical(obs_flows(fac)$x, data.frame(origin = c("o2", "o1"), facility = c("f1", "f1"),
                                                count = c(1, 2)))
  # a tibble is read like a data frame
  expect_identical(obs_flows(tibble::as_tibble(d))$x, obs$x)
  expect_identical(.obs_error(obs_flows(wide)), "name the origin, facility and count columns")
  expect_identical(.obs_error(obs_flows(d[1:2])), "name the origin, facility and count columns")
  expect_identical(.obs_error(obs_flows(d, origin = "o", facility = "f")),
                   "name the origin, facility and count columns")
  expect_identical(.obs_error(obs_flows(d, origin = "o", facility = "nope", count = "n")),
                   "x has no column 'nope'")
  expect_identical(.obs_error(obs_flows(d, origin = "o", facility = "o", count = "n")),
                   "the columns named for x must be different columns")
  expect_identical(.obs_error(obs_flows(d, origin = 1, facility = "f", count = "n")),
                   "origin must be the name of one column of x")
  expect_identical(.obs_error(obs_flows(.obs_flow_matrix(), origin = "o")),
                   "origin name columns of a data frame, but x is not one")
})

test_that("obs_flows() refuses unnamed, duplicated and invalid input", {
  m <- matrix(1:4, 2, 2)
  hint <- "x must have row names (origin IDs) and column names (facility IDs)"
  expect_identical(.obs_error(obs_flows(m)), hint)
  rownames(m) <- c("a", "b")
  expect_identical(.obs_error(obs_flows(m)), hint)
  colnames(m) <- c("x", "y")
  expect_s3_class(obs_flows(m), "spax_obs")
  dup <- m
  rownames(dup) <- c("a", "a")
  expect_identical(.obs_error(obs_flows(dup)), "observed flows have duplicated origin IDs: a")
  dup <- m
  colnames(dup) <- c("x", "x")
  expect_identical(.obs_error(obs_flows(dup)), "observed flows have duplicated facility IDs: x")
  expect_identical(.obs_error(obs_flows(c(a = 1, b = 2))), "x must be a numeric matrix or a data frame")
  expect_identical(.obs_error(obs_flows(array(1:8, c(2, 2, 2)))),
                   "x must be a numeric matrix or a data frame")
  for (bad in list(-1, Inf, NaN, "3")) {
    mm <- matrix(c(1, 2, 3, bad), 2, 2, dimnames = list(c("a", "b"), c("x", "y")))
    expect_identical(.obs_error(obs_flows(mm)), "observed counts must be nonnegative numbers or NA")
  }
  expect_identical(.obs_error(obs_flows(data.frame(o = "a", f = "x", n = -1))),
                   "observed counts must be nonnegative numbers or NA")
  expect_identical(.obs_error(obs_flows(data.frame(o = NA_character_, f = "x", n = 1))),
                   "observed IDs must not be missing")
  expect_identical(.obs_error(obs_flows(data.frame(o = TRUE, f = "x", n = 1))),
                   "observed IDs must be character, factor or numeric")
  # NA counts are allowed: unobserved
  expect_true(is.na(obs_flows(data.frame(o = "a", f = "x", n = NA_real_))$x$count))
})

test_that("duplicate pairs of a long table are summed ignoring NA, all-NA stays NA", {
  d <- data.frame(o = c("o1", "o2", "o1", "o3", "o3", "o2"),
                  f = c("f1", "f1", "f1", "f2", "f2", "f1"),
                  n = c(4, NA, 5, NA, NA, 7))
  x <- obs_flows(d)$x
  expect_identical(x, data.frame(origin = c("o1", "o2", "o3"), facility = c("f1", "f1", "f2"),
                                 count = c(9, 7, NA_real_)))
  # no duplicate: the table is kept as it is, in its order
  e <- data.frame(o = c("o2", "o1"), f = c("f1", "f2"), n = c(1, NA))
  expect_identical(obs_flows(e)$x$count, c(1, NA))
  expect_identical(obs_flows(e)$x$origin, c("o2", "o1"))
})

# obs_totals() -------------------------------------------------------------------

test_that("obs_totals() reads vectors, 1-d arrays and 1-d tables", {
  v <- c(f1 = 4, f2 = 3, f3 = 0)
  obs <- obs_totals(v)
  expect_identical(names(obs), c("type", "x", "loss", "loss_args"))
  expect_identical(obs$type, "utilization")
  expect_identical(obs$x, v)
  expect_identical(obs_totals(c(f1 = 4L, f2 = 3L))$x, c(f1 = 4, f2 = 3))
  d <- data.frame(f = c("f2", "f1", "f2"), n = c(1, 2, 5))
  from_tapply <- obs_totals(tapply(d$n, d$f, sum))
  expect_identical(from_tapply$type, "utilization")
  expect_identical(from_tapply$x, c(f1 = 2, f2 = 6))
  expect_null(dim(from_tapply$x))
  from_table <- obs_totals(table(d$f))
  expect_identical(from_table$x, c(f1 = 1, f2 = 2))
  expect_identical(obs_totals(xtabs(n ~ f, d))$x, c(f1 = 2, f2 = 6))
  # a 1-d array whose unobserved level is NA
  tp <- tapply(d$n, factor(d$f, c("f1", "f2", "f3")), sum)
  expect_identical(obs_totals(tp)$x, c(f1 = 2, f2 = 6, f3 = NA_real_))
  expect_identical(obs_totals(v, loss = "weighted_sse")$loss, "weighted_sse")
  # subsetting a 1-d array by name keeps it a 1-d array
  subset <- tp[c("f2", "f1")]
  expect_identical(length(dim(subset)), 1L)
  expect_identical(obs_totals(subset)$x, c(f2 = 6, f1 = 2))
})

test_that("obs_totals() refuses unnamed, duplicated and invalid vectors", {
  expect_identical(.obs_error(obs_totals(c(1, 2))), "x must be named by facility IDs")
  expect_identical(.obs_error(obs_totals(c(a = 1, 2))), "x must be named by facility IDs")
  expect_identical(.obs_error(obs_totals(array(1:2, 2))), "x must be named by facility IDs")
  expect_identical(.obs_error(obs_totals(c(a = 1, a = 2, b = 3, b = 4, c = 5))),
                   "observed totals have duplicated facility IDs: a, b")
  expect_identical(.obs_error(obs_totals(c(a = -1))), "observed counts must be nonnegative numbers or NA")
  expect_identical(.obs_error(obs_totals(c(a = Inf))), "observed counts must be nonnegative numbers or NA")
  expect_identical(.obs_error(obs_totals(c(a = "1"))), "observed counts must be nonnegative numbers or NA")
  expect_identical(.obs_error(obs_totals(list(a = 1))),
                   "x must be a named numeric vector, a matrix or a data frame")
  expect_identical(.obs_error(obs_totals(NULL)),
                   "x must be a named numeric vector, a matrix or a data frame")
  expect_identical(.obs_error(obs_totals(c(a = 1), count = "n")),
                   "count name columns of a data frame, but x is not one")
})

test_that("obs_totals() reads a facility by group matrix", {
  m <- matrix(c(4, 3, 0, 2, 1, NA), 3, 2, dimnames = list(c("f3", "f1", "f2"), c("a", "b")))
  obs <- obs_totals(m)
  expect_identical(obs$type, "utilization_by_group")
  expect_identical(obs$x, m)
  tab <- table(c("f1", "f2", "f1"), c("a", "b", "b"))
  expect_identical(obs_totals(tab)$x, matrix(c(1, 0, 1, 1), 2, 2,
                                             dimnames = list(c("f1", "f2"), c("a", "b"))))
  expect_identical(.obs_error(obs_totals(matrix(1:4, 2, 2))),
                   "x must have row names (facility IDs) and column names (group IDs)")
  dup <- m
  rownames(dup) <- c("f1", "f1", "f2")
  expect_identical(.obs_error(obs_totals(dup)), "observed totals have duplicated facility IDs: f1")
  dup <- m
  colnames(dup) <- c("a", "a")
  expect_identical(.obs_error(obs_totals(dup)), "observed totals have duplicated group IDs: a")
})

test_that("obs_totals() reads a data frame by position or by column name", {
  two <- data.frame(f = c("f2", "f1", "f2"), n = c(1, 2, NA))
  obs <- obs_totals(two)
  expect_identical(obs$type, "utilization")
  expect_identical(obs$x, c(f2 = 1, f1 = 2))
  three <- data.frame(f = c("f1", "f2", "f2"), g = c("a", "a", "b"), n = c(4, 3, 2))
  by_group <- obs_totals(three)
  expect_identical(by_group$type, "utilization_by_group")
  expect_identical(by_group$x, matrix(c(4, 3, 0, 2), 2, 2,
                                      dimnames = list(c("f1", "f2"), c("a", "b"))))
  named <- three[c("n", "g", "f")]
  expect_identical(obs_totals(named, facility = "f", count = "n", group = "g")$x, by_group$x)
  # naming only facility and count ignores a group column
  expect_identical(obs_totals(three, facility = "f", count = "n")$x, c(f1 = 4, f2 = 5))
  # duplicates are summed as for flows; all-NA stays NA
  dups <- data.frame(f = c("f1", "f1", "f2", "f2"), n = c(1, 2, NA, NA))
  expect_identical(obs_totals(dups)$x, c(f1 = 3, f2 = NA_real_))
  msg <- "name the facility and count columns (and group, for totals by group)"
  expect_identical(.obs_error(obs_totals(cbind(three, 1))), msg)
  expect_identical(.obs_error(obs_totals(three[1]) ), msg)
  expect_identical(.obs_error(obs_totals(three, facility = "f")), msg)
  expect_identical(.obs_error(obs_totals(three, group = "g")), msg)
  expect_identical(.obs_error(obs_totals(three, facility = "f", count = "zz")), "x has no column 'zz'")
})

test_that("absent (facility, group) pairs of a long table are 0", {
  three <- data.frame(f = c("f1", "f2", "f3"), g = c("a", "a", "b"), n = c(4, 3, 2))
  expect_identical(obs_totals(three)$x, matrix(c(4, 3, 0, 0, 0, 2), 3, 2,
                                               dimnames = list(c("f1", "f2", "f3"), c("a", "b"))))
})

test_that("spax_obs prints its counts, total and loss", {
  d <- data.frame(o = rep(c("o1", "o2"), each = 2), f = rep(c("f1", "f2"), 2),
                  n = c(1200, 3400, NA, 5000))
  expect_identical(capture.output(print(obs_flows(d))),
    "<spax obs> flows: 3 origin-facility counts, total 9,600; loss poisson")
  expect_identical(capture.output(obs_totals(c(f1 = 12345, f2 = 586655), loss = "weighted_sse")),
    "<spax obs> totals: 2 facility counts, total 599,000; loss weighted_sse")
  expect_identical(capture.output(obs_totals(data.frame(f = c("a", "b"), g = c("x", "x"), n = c(1, 2)))),
    "<spax obs> totals by group: 2 facility-group counts, total 3; loss poisson")
  expect_identical(capture.output(obs_flows(matrix(c(1.5, 0), 1, 2, dimnames = list("o", c("a", "b"))))),
    "<spax obs> flows: 2 origin-facility counts, total 1.5; loss poisson")
  shown <- NULL
  invisible(capture.output(shown <- withVisible(print(obs_flows(d)))))
  expect_false(shown$visible)
  expect_s3_class(shown$value, "spax_obs")
})

# Canonical flows ------------------------------------------------------------------

test_that("flows are aligned to the active origins and facilities of the data", {
  data <- .obs_case()
  r <- .obs_messages(.canonical_observed(obs_flows(.obs_flow_matrix()), data, TRUE))
  expect_identical(r$value, .obs_flow_expected())
  expect_type(r$value, "double")
  # one message for the dropped origins and the facilities outside the data; counts
  # at the outside facility of the dropped origins (o6: 2, zz: 7) are not in it
  expect_identical(r$messages, paste0("observed: 180 counts from 3 origins outside the data ",
    "or left out by it are dropped; 1 counts at 1 facilities outside the data are treated ",
    "as the outside option\n"))
  # the long form of the same counts gives the same target
  long <- .obs_long(.obs_flow_matrix())
  r2 <- .obs_messages(.canonical_observed(
    obs_flows(long, origin = "lsoa", facility = "practice", count = "patients"), data, TRUE))
  expect_identical(r2$value, .obs_flow_expected())
  expect_identical(r2$messages, r$messages)
})

test_that("absent pairs of a long table are 0 and absent origins and facilities are filled", {
  data <- .obs_case()
  # only positive pairs; o3 has no row at all, f1 has no row at all
  long <- data.frame(o = c("o1", "o1", "o5", "o5"), f = c("f2", "f3", "f2", "f3"),
                     n = c(7, 2, 3, 5))
  r <- .obs_messages(.canonical_observed(obs_flows(long), data, TRUE))
  expect_identical(r$value, matrix(c(7, 0, 3,  0, 0, 0,  2, 0, 5), 3, 3,
    dimnames = list(c("o1", "o3", "o5"), c("f2", "f1", "f3"))))
  expect_identical(r$messages, paste0("observed: no counts for 1 active origins and 1 ",
    "facilities in the data; they are taken as 0\n"))
  # a pair that is absent while its origin and facility both appear is 0, silently
  long2 <- data.frame(o = c("o1", "o3", "o5", "o5"), f = c("f2", "f1", "f1", "f3"),
                      n = c(7, 8, 4, 5))
  r2 <- .obs_messages(.canonical_observed(obs_flows(long2), data, TRUE))
  expect_identical(r2$messages, character())
  expect_identical(unname(r2$value), matrix(c(7, 0, 0,  0, 8, 4,  0, 0, 5), 3, 3))
  # a matrix missing a row and a column fills them with 0; NA cells stay NA
  m <- matrix(c(7, NA, 6, 8), 2, 2, dimnames = list(c("o1", "o3"), c("f2", "f1")))
  r3 <- .obs_messages(.canonical_observed(obs_flows(m), data, TRUE))
  expect_identical(r3$value, matrix(c(7, NA, 0,  6, 8, 0,  0, 0, 0), 3, 3,
    dimnames = list(c("o1", "o3", "o5"), c("f2", "f1", "f3"))))
  expect_identical(r3$messages, paste0("observed: no counts for 1 active origins and 1 ",
    "facilities in the data; they are taken as 0\n"))
})

test_that("a transposed matrix is refused by its keys", {
  data <- .obs_case()
  m <- .obs_flow_matrix()
  expect_identical(.obs_error(.canonical_observed(obs_flows(t(m)), data, TRUE)),
    "none of the observed origin IDs are in the data; are rows and columns swapped?")
  # right origins, wrong facilities
  w <- m
  colnames(w) <- paste0("g", 1:4)
  expect_identical(.obs_error(.canonical_observed(obs_flows(w), data, TRUE)),
    "none of the observed facility IDs are in the data; are rows and columns swapped?")
  # an empty frame has no keys either
  expect_identical(.obs_error(.canonical_observed(obs_flows(.obs_long(m)[0, ]), data, TRUE)),
    "none of the observed origin IDs are in the data; are rows and columns swapped?")
})

test_that("all counts dropped is an error", {
  data <- .obs_case()
  m <- matrix(c(5, 6, 7, 8), 2, 2, dimnames = list(c("zz", "yy"), c("f1", "f2")))
  expect_identical(.obs_error(.canonical_observed(obs_flows(m), data, TRUE)),
    "none of the observed origin IDs are in the data; are rows and columns swapped?")
  # origins of the data that carry no positive count after the drops
  only_dropped <- matrix(c(5, 0, 6, 0), 2, 2, dimnames = list(c("o4", "o1"), c("f1", "f2")))
  r <- .obs_error_messages(.canonical_observed(obs_flows(only_dropped), data, TRUE))
  expect_identical(r$error,
    "no observed count remains after dropping origins and facilities outside the data")
  expect_identical(r$messages, paste0("observed: 11 counts from 1 origins outside the data or ",
    "left out by it are dropped\n"))
  # only zeros and NA remain
  zeros <- matrix(c(0, NA, 0, 0), 2, 2, dimnames = list(c("o1", "o3"), c("f1", "f2")))
  expect_identical(.obs_error(.canonical_observed(obs_flows(zeros), data, TRUE)),
    "no observed count remains after dropping origins and facilities outside the data")
  # facilities all outside the data, or origins excluded, besides one key match each
  mixed <- matrix(c(9, 1, 0, 0), 2, 2, dimnames = list(c("o6", "o1"), c("out", "f1")))
  msgs <- .obs_error_messages(.canonical_observed(obs_flows(mixed), data, TRUE))
  expect_identical(msgs$error,
    "no observed count remains after dropping origins and facilities outside the data")
  expect_identical(msgs$messages, paste0("observed: 9 counts from 1 origins outside the data ",
    "or left out by it are dropped; 1 counts at 1 facilities outside the data are treated ",
    "as the outside option\n"))
})

test_that("zero-demand origins: positive counts are an error, zeros are dropped silently", {
  data <- .obs_case()
  base <- .obs_flow_expected()
  with_zero <- rbind(base, o2 = c(0, NA, 0))
  r <- .obs_messages(.canonical_observed(obs_flows(with_zero), data, TRUE))
  expect_identical(r$value, base)
  expect_identical(r$messages, character())
  positive <- rbind(base, o2 = c(0, 3, 0))
  expect_identical(.obs_error(.canonical_observed(obs_flows(positive), data, TRUE)),
    "observed has counts for 1 origins with zero demand in the data: o2")
  # a long table, and the first five ids in the order of the table
  ids <- paste0("z", 1:7)
  supply <- c(f2 = 18, f1 = 25, f3 = 15)
  demand <- c(o1 = 40, o3 = 55, stats::setNames(rep(0, 7), ids))
  travel <- matrix(1:27, 9, 3, dimnames = list(names(demand), names(supply)))
  d0 <- prepare_allocation(demand, supply, travel)
  expect_identical(d0$origin_status, c("active", "active", rep("zero", 7)))
  long <- data.frame(o = c("o1", "o3", rev(ids)), f = "f1", n = c(4, 5, rep(1, 7)))
  expect_identical(.obs_error(.canonical_observed(obs_flows(long), d0, TRUE)),
    "observed has counts for 7 origins with zero demand in the data: z7, z6, z5, z4, z3")
  # only the origins with a positive count are named
  long$n[3:4] <- 0
  expect_identical(.obs_error(.canonical_observed(obs_flows(long), d0, TRUE)),
    "observed has counts for 5 origins with zero demand in the data: z5, z4, z3, z2, z1")
})

test_that("excluded and unknown origins are dropped with the message", {
  data <- .obs_case()
  m <- rbind(.obs_flow_expected(), o6 = c(1, 2, 3), o4 = c(4, 5, 6))
  r <- .obs_messages(.canonical_observed(obs_flows(m), data, TRUE))
  expect_identical(r$value, .obs_flow_expected())
  expect_identical(r$messages, paste0("observed: 21 counts from 2 origins outside the data or ",
    "left out by it are dropped\n"))
  # an excluded origin with only zero counts is not reported
  r2 <- .obs_messages(.canonical_observed(obs_flows(rbind(.obs_flow_expected(), o6 = c(0, 0, 0))),
                                          data, TRUE))
  expect_identical(r2$messages, character())
  expect_identical(r2$value, .obs_flow_expected())
})

test_that("an origin left out through included = is dropped like an outside origin", {
  inputs <- prepare_allocation(c(o1 = 40, o3 = 55, o5 = 35), c(f2 = 18, f1 = 25, f3 = 15),
    matrix(c(1, 3, 6,  6, 2, 4,  4, 1, 5), 3, 3,
           dimnames = list(c("o1", "o3", "o5"), c("f2", "f1", "f3"))),
    included = c(TRUE, TRUE, FALSE))
  expect_identical(inputs$origin_status, c("active", "active", "excluded"))
  r <- .obs_messages(.canonical_observed(obs_flows(.obs_flow_expected()), inputs, TRUE))
  expect_identical(r$value, .obs_flow_expected()[c("o1", "o3"), ])
  expect_identical(r$messages, paste0("observed: 12 counts from 1 origins outside the data or ",
    "left out by it are dropped\n"))
})

test_that("the Huff wording says that counts at facilities outside the data are dropped", {
  data <- .obs_case()
  r <- .obs_messages(.canonical_observed(obs_flows(.obs_flow_matrix()), data, FALSE))
  expect_identical(r$value, .obs_flow_expected())
  expect_identical(r$messages, paste0("observed: 180 counts from 3 origins outside the data ",
    "or left out by it are dropped; 1 counts at 1 facilities outside the data are dropped ",
    "(the Huff model has no outside option)\n"))
  # without dropped origins
  m <- cbind(.obs_flow_expected(), out = c(0, 4, 5))
  r2 <- .obs_messages(.canonical_observed(obs_flows(m), data, FALSE))
  expect_identical(r2$messages, paste0("observed: 9 counts at 1 facilities outside the data ",
    "are dropped (the Huff model has no outside option)\n"))
  r3 <- .obs_messages(.canonical_observed(obs_flows(m), data, TRUE))
  expect_identical(r3$messages, paste0("observed: 9 counts at 1 facilities outside the data ",
    "are treated as the outside option\n"))
  expect_identical(.obs_error(.canonical_observed(obs_flows(m), data, NA)),
    "outside_option must be TRUE or FALSE")
})

test_that("no message when nothing is dropped, and NA counts do not count", {
  data <- .obs_case()
  r <- .obs_messages(.canonical_observed(obs_flows(.obs_flow_expected()), data, TRUE))
  expect_identical(r$messages, character())
  m <- cbind(.obs_flow_expected(), out = NA_real_)
  expect_identical(.obs_messages(.canonical_observed(obs_flows(m), data, TRUE))$messages, character())
  rows <- rbind(.obs_flow_expected(), zz = NA_real_)
  expect_identical(.obs_messages(.canonical_observed(obs_flows(rows), data, TRUE))$messages,
                   character())
})

test_that("numeric IDs keep their digits", {
  ids <- c(100000, 100001, 123456)
  fac <- c(200000, 200001)
  chr <- format(ids, scientific = FALSE, trim = TRUE)
  fchr <- format(fac, scientific = FALSE, trim = TRUE)
  expect_identical(chr, c("100000", "100001", "123456"))
  data <- prepare_allocation(stats::setNames(c(10, 20, 30), chr), stats::setNames(c(5, 6), fchr),
                             matrix(c(1, 2, 3, 3, 2, 1), 3, 2, dimnames = list(chr, fchr)))
  long <- data.frame(origin = rep(ids, 2), facility = rep(fac, each = 3), n = c(4, 5, 6, 1, 2, 3))
  obs <- obs_flows(long)
  expect_identical(obs$x$origin[1:3], chr)
  expect_identical(obs$x$facility[c(1, 4)], fchr)
  expect_identical(.canonical_observed(obs, data, TRUE),
                   matrix(c(4, 5, 6, 1, 2, 3), 3, 2, dimnames = list(chr, fchr)))
  # factors and integer-valued numbers give the same IDs
  fac_long <- data.frame(origin = factor(chr), facility = rep(fchr, each = 3), n = long$n)
  expect_identical(obs_flows(fac_long)$x, obs$x)
  int_long <- long
  int_long$origin <- as.integer(int_long$origin)
  int_long$facility <- as.integer(int_long$facility)
  expect_identical(obs_flows(int_long)$x, obs$x)
  # totals: numeric facility IDs in a frame
  totals <- obs_totals(data.frame(f = fac, n = c(9, 8)))
  expect_identical(.canonical_observed(totals, data, TRUE), c("200000" = 9, "200001" = 8))
  # non-integer IDs are written as they are, one by one
  expect_identical(.obs_ids(c(1.5, 2, 10)), c("1.5", "2", "10"))
})

# Zero supply and unusable travel ------------------------------------------------------

test_that("positive counts at a zero-supply facility are an error", {
  data <- .obs_case(supply = c(f2 = 18, f1 = 0, f3 = 15))
  expect_identical(.obs_error(.canonical_observed(obs_flows(.obs_flow_expected()), data, TRUE)),
    paste0("observed has 18 counts at 1 facilities with zero supply (f1); the model sends no one ",
           "there. Remove those counts, set them to NA, or check the supply"))
  # zero or NA counts there are fine
  ok <- .obs_flow_expected()
  ok[, "f1"] <- c(0, NA, 0)
  expect_identical(.canonical_observed(obs_flows(ok), data, TRUE), ok)
  # the counts of dropped origins do not count
  m <- rbind(ok, o6 = c(5, 50, 5))
  expect_identical(suppressMessages(.canonical_observed(obs_flows(m), data, TRUE)), ok)
  # two facilities, and totals
  data2 <- .obs_case(supply = c(f2 = 0, f1 = 0, f3 = 15))
  expect_identical(.obs_error(.canonical_observed(obs_flows(.obs_flow_expected()), data2, TRUE)),
    paste0("observed has 37 counts at 2 facilities with zero supply (f2, f1); the model sends no one ",
           "there. Remove those counts, set them to NA, or check the supply"))
  expect_identical(.obs_error(.canonical_observed(obs_totals(c(f1 = 18, f2 = 5, f3 = 1)), data, TRUE)),
    paste0("observed has 18 counts at 1 facilities with zero supply (f1); the model sends no one ",
           "there. Remove those counts, set them to NA, or check the supply"))
  expect_identical(.canonical_observed(obs_totals(c(f1 = 0, f2 = 5, f3 = NA)), data, TRUE),
                   c(f2 = 5, f1 = 0, f3 = NA))
  gdata <- .obs_case_groups(supply = c(f2 = 18, f1 = 0, f3 = 15))
  expect_identical(gdata$group_ids, c("a", "b"))
  expect_identical(.obs_error(.canonical_observed(
    obs_totals(matrix(c(1, 2, 3, 4, 5, 6), 3, 2, dimnames = list(c("f1", "f2", "f3"), c("a", "b")))),
    gdata, TRUE)),
    paste0("observed has 5 counts at 1 facilities with zero supply (f1); the model sends no one ",
           "there. Remove those counts, set them to NA, or check the supply"))
})

test_that("positive counts on unknown or infinite travel are an error", {
  travel <- matrix(c(1, 5, 3, 4, 6, 2,  6, 2, 2, 3, 4, 3,  4, 6, 1, 2, 5, 4), 6, 3,
                   dimnames = list(paste0("o", 1:6), c("f2", "f1", "f3")))
  inf <- travel
  inf["o1", "f1"] <- Inf
  data <- .obs_case(travel = inf)
  msg <- paste0("observed has 6 counts on origin-facility pairs with unknown or infinite travel; ",
                "set them to NA or give their travel times")
  expect_identical(.obs_error(.canonical_observed(obs_flows(.obs_flow_expected()), data, TRUE)), msg)
  ok <- .obs_flow_expected()
  ok["o1", "f1"] <- NA
  expect_identical(.canonical_observed(obs_flows(ok), data, TRUE), ok)
  ok["o1", "f1"] <- 0
  expect_identical(.canonical_observed(obs_flows(ok), data, TRUE), ok)
  # unknown travel kept as an omitted edge by the legacy policy
  na <- travel
  na["o3", "f2"] <- NA
  data_na <- .obs_case(travel = na, missing_travel = "legacy_zero")
  expect_identical(.obs_error(.canonical_observed(obs_flows(.obs_flow_expected()), data_na, TRUE)),
    paste0("observed has 9 counts on origin-facility pairs with unknown or infinite travel; ",
           "set them to NA or give their travel times"))
  # the zero-supply error comes first
  both <- .obs_case(supply = c(f2 = 18, f1 = 0, f3 = 15), travel = inf)
  expect_match(.obs_error(.canonical_observed(obs_flows(.obs_flow_expected()), both, TRUE)),
               "with zero supply", fixed = TRUE)
  # travel by group: a pair is reachable when a group with demand there has a finite time
  g <- .obs_case_groups()
  ga <- g$distance_groups$a
  gb <- g$distance_groups$b
  ga["o1", "f1"] <- Inf
  gdata <- prepare_allocation(list(a = c(o1 = 15, o3 = 25, o5 = 20), b = c(o1 = 25, o3 = 30, o5 = 15)),
    c(f2 = 18, f1 = 25, f3 = 15), list(a = ga[c("o1", "o3", "o5"), ], b = gb[c("o1", "o3", "o5"), ]))
  expect_null(gdata$distance_active)
  expect_identical(.canonical_observed(obs_flows(.obs_flow_expected()), gdata, TRUE),
                   .obs_flow_expected())
  gb["o1", "f1"] <- Inf
  gdata2 <- prepare_allocation(list(a = c(o1 = 15, o3 = 25, o5 = 20), b = c(o1 = 25, o3 = 30, o5 = 15)),
    c(f2 = 18, f1 = 25, f3 = 15), list(a = ga[c("o1", "o3", "o5"), ], b = gb[c("o1", "o3", "o5"), ]))
  expect_identical(.obs_error(.canonical_observed(obs_flows(.obs_flow_expected()), gdata2, TRUE)), msg)
})

# Canonical totals ------------------------------------------------------------------------

test_that("totals are aligned to the facilities of the data", {
  data <- .obs_case()
  x <- c(f3 = 5, out = 7, f2 = 3, f1 = 4)
  r <- .obs_messages(.canonical_observed(obs_totals(x), data, TRUE))
  expect_identical(r$value, c(f2 = 3, f1 = 4, f3 = 5))
  expect_identical(r$messages, "observed: 7 counts at 1 facilities outside the data are treated as the outside option\n")
  r2 <- .obs_messages(.canonical_observed(obs_totals(x), data, FALSE))
  expect_identical(r2$messages, paste0("observed: 7 counts at 1 facilities outside the data are ",
                                       "dropped (the Huff model has no outside option)\n"))
  # no message without counts outside; NA stays NA
  y <- c(f3 = NA, f2 = 3, f1 = 0, out = 0)
  r3 <- .obs_messages(.canonical_observed(obs_totals(y), data, TRUE))
  expect_identical(r3$value, c(f2 = 3, f1 = 0, f3 = NA_real_))
  expect_identical(r3$messages, character())
  # a data frame of totals, with the facilities in any order
  d <- data.frame(f = c("f3", "f1", "f2", "f1"), n = c(5, 1, 3, 3))
  expect_identical(.canonical_observed(obs_totals(d), data, TRUE), c(f2 = 3, f1 = 4, f3 = 5))
})

test_that("a facility of the data missing from the totals is an error", {
  data <- .obs_case()
  expect_identical(.obs_error(.canonical_observed(obs_totals(c(f1 = 1, f3 = 2)), data, TRUE)),
    "observed totals have no count for 1 facilities in the data: f2; give 0, or NA if unobserved")
  # nothing in common with the data: every facility is missing, no outside message first
  r <- .obs_messages(.obs_error(.canonical_observed(obs_totals(c(a = 1, b = 2, c = 3, d = 4)), data, TRUE)))
  expect_identical(r$value,
    "observed totals have no count for 3 facilities in the data: f2, f1, f3; give 0, or NA if unobserved")
  expect_identical(r$messages, character())
  # only the first five are named
  supply <- stats::setNames(rep(5, 7), paste0("f", 1:7))
  travel <- matrix(1, 2, 7, dimnames = list(c("a", "b"), names(supply)))
  data7 <- prepare_allocation(c(a = 1, b = 2), supply, travel)
  expect_identical(.obs_error(.canonical_observed(obs_totals(c(f1 = 1)), data7, TRUE)),
    "observed totals have no count for 6 facilities in the data: f2, f3, f4, f5, f6; give 0, or NA if unobserved")
  # an NA count is a given count, not a missing facility
  expect_identical(.canonical_observed(obs_totals(c(f2 = NA, f1 = 1, f3 = 1)), data, TRUE),
                   c(f2 = NA_real_, f1 = 1, f3 = 1))
})

test_that("totals by group are aligned to the facilities and groups of the data", {
  data <- .obs_case_groups()
  x <- matrix(c(5, 3, 4, 9,  2, 1, 6, 1), 4, 2,
              dimnames = list(c("f3", "f2", "f1", "out"), c("b", "a")))
  r <- .obs_messages(.canonical_observed(obs_totals(x), data, TRUE))
  expect_identical(r$value, matrix(c(1, 6, 2,  3, 4, 5), 3, 2,
    dimnames = list(c("f2", "f1", "f3"), c("a", "b"))))
  expect_identical(r$messages, paste0("observed: 10 counts at 1 facilities outside the data ",
                                      "are treated as the outside option\n"))
  expect_type(r$value, "double")
  # NA cells stay NA
  y <- x
  y["f1", "a"] <- NA
  expect_identical(suppressMessages(.canonical_observed(obs_totals(y), data, TRUE))["f1", ],
                   c(a = NA_real_, b = 4))
})

test_that("a group mismatch is an error naming both sets", {
  data <- .obs_case_groups()
  x <- matrix(1, 3, 2, dimnames = list(c("f2", "f1", "f3"), c("a", "c")))
  expect_identical(.obs_error(.canonical_observed(obs_totals(x), data, TRUE)),
    "observed groups (a, c) must equal the groups of the data (a, b)")
  y <- matrix(1, 3, 3, dimnames = list(c("f2", "f1", "f3"), c("a", "b", "c")))
  expect_identical(.obs_error(.canonical_observed(obs_totals(y), data, TRUE)),
    "observed groups (a, b, c) must equal the groups of the data (a, b)")
  # the data has no groups
  expect_identical(.obs_error(.canonical_observed(obs_totals(x), .obs_case(), TRUE)),
    "observed groups (a, c) must equal the groups of the data (none)")
  # a long table that lacks a group
  long <- data.frame(f = c("f2", "f1", "f3"), g = "a", n = 1:3)
  expect_identical(.obs_error(.canonical_observed(obs_totals(long), data, TRUE)),
    "observed groups (a) must equal the groups of the data (a, b)")
})

test_that("absent (facility, group) pairs are 0 in the target; missing facilities are an error", {
  data <- .obs_case_groups()
  long <- data.frame(f = c("f3", "f2", "f1", "f1"), g = c("b", "a", "b", "a"), n = c(6, 1, 4, 3))
  r <- .canonical_observed(obs_totals(long), data, TRUE)
  expect_identical(r, matrix(c(1, 3, 0,  0, 4, 6), 3, 2,
                             dimnames = list(c("f2", "f1", "f3"), c("a", "b"))))
  # f3 has no row in any group
  short <- long[long$f != "f3", ]
  expect_identical(.obs_error(.canonical_observed(obs_totals(short), data, TRUE)),
    "observed totals have no count for 1 facilities in the data: f3; give 0, or NA if unobserved")
})

# Raw observed ------------------------------------------------------------------------------

test_that("a raw named vector, 1-d array or 1-d table is read as totals", {
  data <- .obs_case()
  v <- c(f2 = 3, f1 = 4, f3 = 5)
  expect_identical(.as_spax_obs(v, data), obs_totals(v))
  tp <- tapply(c(3, 4, 5), c("f2", "f1", "f3"), sum)
  expect_identical(.as_spax_obs(tp, data), obs_totals(tp))
  tb <- table(c("f2", "f1", "f1"))
  expect_identical(.as_spax_obs(tb, data), obs_totals(tb))
  expect_identical(.as_spax_obs(obs_totals(v, loss = "weighted_sse"), data)$loss, "weighted_sse")
  expect_identical(.obs_error(.as_spax_obs(c(3, 4, 5), data)), "observed must be named by origin and facility IDs")
  expect_identical(.obs_error(.as_spax_obs(array(1:3, 3), data)), "observed must be named by origin and facility IDs")
  expect_identical(.obs_error(.as_spax_obs(list(1), data)),
    "observed must be obs_flows(), obs_totals(), a named numeric vector or a matrix")
  expect_identical(.obs_error(.as_spax_obs(NULL, data)),
    "observed must be obs_flows(), obs_totals(), a named numeric vector or a matrix")
  expect_identical(.obs_error(.as_spax_obs(array(1, c(1, 1, 1)), data)),
    "observed must be a named numeric vector or a matrix")
})

test_that("a raw matrix is read as flows or as totals by group", {
  data <- .obs_case_groups()
  flows <- .obs_flow_matrix()
  expect_identical(.as_spax_obs(flows, data), obs_flows(flows))
  expect_identical(.as_spax_obs(xtabs(n ~ o + f, data.frame(o = c("o1", "o3"), f = c("f1", "f2"), n = 1:2)), data)$type,
                   "flow")
  by_group <- matrix(1:6, 3, 2, dimnames = list(c("f2", "f1", "f3"), c("b", "a")))
  expect_identical(.as_spax_obs(by_group, data), obs_totals(by_group))
  expect_identical(.as_spax_obs(by_group, data)$type, "utilization_by_group")
  # without groups in the data only the flow reading exists
  expect_identical(.obs_error(.as_spax_obs(by_group, .obs_case())),
    paste0("the rows and columns of observed are neither origin by facility nor facility by ",
           "group IDs of the data; use obs_flows() or obs_totals()"))
})

test_that("a raw matrix that reads both ways or neither way is an error", {
  ids <- c("a", "b", "o3")
  demand <- list(a = stats::setNames(c(1, 2, 3), ids), b = stats::setNames(c(3, 2, 1), ids))
  travel <- matrix(c(1, 2, 3, 3, 2, 1), 3, 2, dimnames = list(ids, c("a", "b")))
  data <- prepare_allocation(demand, c(a = 5, b = 6), travel)
  m <- matrix(1:4, 2, 2, dimnames = list(c("a", "b"), c("a", "b")))
  expect_identical(.obs_error(.as_spax_obs(m, data)),
    "observed reads both as flows and as totals by group; use obs_flows() or obs_totals()")
  # the explicit constructors still work
  expect_identical(.as_spax_obs(obs_flows(m), data)$type, "flow")
  expect_identical(.as_spax_obs(obs_totals(m), data)$type, "utilization_by_group")
  neither <- matrix(1:4, 2, 2, dimnames = list(c("x", "y"), c("u", "v")))
  expect_identical(.obs_error(.as_spax_obs(neither, .obs_case_groups())),
    paste0("the rows and columns of observed are neither origin by facility nor facility by ",
           "group IDs of the data; use obs_flows() or obs_totals()"))
  # transposed flows read neither way
  expect_identical(.obs_error(.as_spax_obs(t(.obs_flow_matrix()), .obs_case_groups())),
    paste0("the rows and columns of observed are neither origin by facility nor facility by ",
           "group IDs of the data; use obs_flows() or obs_totals()"))
  # group names right but no facility among the rows
  wrong_rows <- matrix(1:4, 2, 2, dimnames = list(c("x", "y"), c("a", "b")))
  expect_identical(.obs_error(.as_spax_obs(wrong_rows, .obs_case_groups())),
    paste0("the rows and columns of observed are neither origin by facility nor facility by ",
           "group IDs of the data; use obs_flows() or obs_totals()"))
})

test_that("a raw data frame or an unnamed matrix is an error", {
  data <- .obs_case()
  expect_identical(.obs_error(.as_spax_obs(data.frame(o = "o1", f = "f1", n = 1), data)),
    "wrap a table in obs_flows() or obs_totals() to name its columns")
  expect_identical(.obs_error(.as_spax_obs(matrix(1:4, 2, 2), data)),
    "observed must be named by origin and facility IDs")
  m <- matrix(1:4, 2, 2, dimnames = list(c("o1", "o3"), NULL))
  expect_identical(.obs_error(.as_spax_obs(m, data)), "observed must be named by origin and facility IDs")
})

# Accepted by fit_allocation() --------------------------------------------------------------

test_that("canonical targets are accepted by fit_allocation() and recover the truth", {
  data <- .obs_case()
  model <- clm_allocation(data, fit_v0 = TRUE)
  truth <- c(sigma = 3, v0 = 10)
  outputs <- evaluate_allocation(model, truth, requested_outputs = c("flow", "utilization"))$outputs
  starts <- rbind(near = c(sigma = 3.5, v0 = 8), far = c(sigma = 5, v0 = 20))
  lower <- c(sigma = 1, v0 = 1)
  upper <- c(sigma = 8, v0 = 40)
  control <- list(maxit = 300, factr = 10, pgtol = 1e-8)
  fit <- function(target, output) {
    fit_allocation(model, target, starts, lower, upper, output = output, loss = "poisson",
                   gradient = TRUE, control = control)
  }
  # flows: shuffled matrix with extras, then the same as a long table
  m <- cbind(outputs$flow, out = c(1, 0, 2))[c("o5", "o1", "o3"), c("f3", "out", "f2", "f1")]
  m <- rbind(m, zz = 9, o6 = 4, o2 = 0, o4 = 3)
  target <- suppressMessages(.canonical_observed(obs_flows(m), data, TRUE))
  expect_equal(target, outputs$flow)
  f <- fit(target, "flow")
  expect_equal(unname(coef(f)), unname(truth), tolerance = 1e-3)
  long <- .obs_long(m)
  target_long <- suppressMessages(.canonical_observed(
    obs_flows(long, origin = "lsoa", facility = "practice", count = "patients"), data, TRUE))
  expect_identical(target_long, target)
  # facility totals
  u <- stats::setNames(as.vector(outputs$utilization), data$facility_ids)
  totals <- c(out = 6, u[c("f3", "f1", "f2")])
  target_u <- suppressMessages(.canonical_observed(obs_totals(totals), data, TRUE))
  expect_equal(target_u, u)
  fu <- fit(target_u, "utilization")
  expect_equal(unname(coef(fu)), unname(truth), tolerance = 1e-3)
  # the exact vector and matrix are accepted by the binding itself
  expect_s3_class(.bind_problem_target(model, target, "flow"), "ae_bound_target")
  expect_s3_class(.bind_problem_target(model, target_u, "utilization"), "ae_bound_target")
})

test_that("totals by group are accepted by fit_allocation()", {
  data <- .obs_case_groups()
  model <- clm_allocation(data, fit_v0 = TRUE)
  truth <- c(sigma = 3, v0 = 10)
  expected <- evaluate_allocation(model, truth,
    requested_outputs = "utilization_by_group")$outputs$utilization_by_group
  long <- data.frame(f = rep(rownames(expected), 2), g = rep(colnames(expected), each = 3),
                     n = as.vector(expected))
  long <- rbind(long[c(6, 2, 4, 1, 5, 3), ], data.frame(f = "out", g = "a", n = 3))
  target <- suppressMessages(.canonical_observed(obs_totals(long), data, TRUE))
  expect_equal(target, expected)
  expect_identical(dimnames(target), list(data$facility_ids, data$group_ids))
  fit <- fit_allocation(model, target,
    rbind(near = c(sigma = 3.5, v0 = 8), far = c(sigma = 5, v0 = 20)),
    c(sigma = 1, v0 = 1), c(sigma = 8, v0 = 40), output = "utilization_by_group",
    loss = "poisson", gradient = TRUE, control = list(maxit = 300, factr = 10, pgtol = 1e-8))
  expect_equal(unname(coef(fit)), unname(truth), tolerance = 1e-3)
  # the same through a raw matrix
  raw <- suppressMessages(.canonical_observed(.as_spax_obs(expected[c(3, 1, 2), c(2, 1)], data),
                                              data, TRUE))
  expect_identical(raw, target)
})

# Cornwall ----------------------------------------------------------------------------------

.obs_cornwall <- function(zero_supply = NULL) {
  gp <- sf::st_drop_geometry(cornwall_gp)
  gp <- gp[!gp$scilly, ]
  areas <- sf::st_drop_geometry(cornwall_lsoa)
  areas <- areas[!areas$scilly, ]
  travel <- cornwall_travel[areas$lsoa, gp$practice]
  supply <- stats::setNames(gp$gp_fte_extg, gp$practice)
  if (!is.null(zero_supply)) supply[zero_supply] <- 0
  list(data = prepare_allocation(stats::setNames(areas$registered, areas$lsoa), supply, travel),
       travel = travel)
}

test_that("the long cornwall_flows table reproduces the counts of the review (D9)", {
  skip_on_cran()
  cw <- .obs_cornwall()
  fl <- cornwall_flows
  in_origin <- fl$lsoa %in% cw$data$all_origin_ids
  in_facility <- fl$practice %in% cw$data$facility_ids
  # the counts the message must report: the Scilly small area has 2,274 counts at the
  # Scilly practice and 16 at mainland practices; only 5 counts from mainland origins
  # go to a facility outside the data
  expect_identical(sum(fl$patients[!in_origin & !in_facility]), 2274L)
  expect_identical(sum(fl$patients[!in_origin]), 2290L)
  expect_identical(sum(fl$patients[in_origin & !in_facility]), 5L)
  obs <- obs_flows(cornwall_flows, origin = "lsoa", facility = "practice", count = "patients")
  r <- .obs_messages(.canonical_observed(obs, cw$data, TRUE))
  expect_identical(r$messages, paste0("observed: 2,290 counts from 1 origins outside the data ",
    "or left out by it are dropped; 5 counts at 1 facilities outside the data are treated as ",
    "the outside option\n"))
  expected <- matrix(0, nrow(cw$travel), ncol(cw$travel), dimnames = dimnames(cw$travel))
  keep <- in_origin & in_facility
  expected[cbind(fl$lsoa[keep], fl$practice[keep])] <- fl$patients[keep]
  expect_identical(r$value, expected)
  # no origin of Cornwall lacks a row in the long table here, so nothing is filled
  expect_identical(sum(!cw$data$origin_ids %in% fl$lsoa), 0L)
  # the Huff wording
  h <- .obs_messages(.canonical_observed(obs, cw$data, FALSE))
  expect_identical(h$messages, paste0("observed: 2,290 counts from 1 origins outside the data ",
    "or left out by it are dropped; 5 counts at 1 facilities outside the data are dropped ",
    "(the Huff model has no outside option)\n"))
  expect_identical(h$value, expected)
  # the target binds to a CLM on the same data
  model <- clm_allocation(cw$data, fit_v0 = TRUE)
  expect_s3_class(.bind_problem_target(model, r$value, "flow"), "ae_bound_target")
})

test_that("xtabs and tapply input give the same counts as the long table on Cornwall", {
  skip_on_cran()
  cw <- .obs_cornwall()
  by_long <- suppressMessages(.canonical_observed(
    obs_flows(cornwall_flows, "lsoa", "practice", "patients"), cw$data, TRUE))
  by_xtabs <- .obs_messages(.canonical_observed(
    obs_flows(xtabs(patients ~ lsoa + practice, cornwall_flows)), cw$data, TRUE))
  expect_identical(by_xtabs$value, by_long)
  expect_identical(by_xtabs$messages, paste0("observed: 2,290 counts from 1 origins outside the ",
    "data or left out by it are dropped; 5 counts at 1 facilities outside the data are treated ",
    "as the outside option\n"))
  totals <- tapply(cornwall_flows$patients, cornwall_flows$practice, sum)
  r <- .obs_messages(.canonical_observed(obs_totals(totals), cw$data, TRUE))
  expect_identical(r$value, stats::setNames(as.double(totals[colnames(cw$travel)]), colnames(cw$travel)))
  # totals are not origin by origin: all 2,279 counts at the Scilly practice are outside
  expect_identical(r$messages, paste0("observed: 2,279 counts at 1 facilities outside the data ",
    "are treated as the outside option\n"))
  # a transposed Cornwall matrix
  expect_identical(.obs_error(.canonical_observed(
    obs_flows(t(xtabs(patients ~ lsoa + practice, cornwall_flows))), cw$data, TRUE)),
    "none of the observed origin IDs are in the data; are rows and columns swapped?")
})

test_that("Cornwall counts at a facility without supply are an error", {
  skip_on_cran()
  cw0 <- .obs_cornwall()
  first <- cw0$data$facility_ids[1]
  cw <- .obs_cornwall(zero_supply = first)
  fl <- cornwall_flows
  expected <- sum(fl$patients[fl$practice == first & fl$lsoa %in% cw$data$all_origin_ids])
  obs <- obs_flows(cornwall_flows, "lsoa", "practice", "patients")
  expect_identical(.obs_error(.canonical_observed(obs, cw$data, TRUE)),
    paste0("observed has ", format(expected, big.mark = ","), " counts at 1 facilities with zero ",
           "supply (", first, "); the model sends no one there. Remove those counts, set them ",
           "to NA, or check the supply"))
  # the review's check: the counts at that practice in the facility totals
  totals <- tapply(fl$patients, fl$practice, sum)
  expect_match(.obs_error(.canonical_observed(obs_totals(totals), cw$data, TRUE)),
               paste0("observed has ", format(totals[[first]], big.mark = ","), " counts at 1 facilities"),
               fixed = TRUE)
})
