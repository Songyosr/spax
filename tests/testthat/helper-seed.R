# Seed the random stream for one test (or helper call) and restore the
# caller's state afterwards. withr::local_seed() 3.0.2 leaves the new stream
# in place, so preserve the old state explicitly.
local_test_seed <- function(seed, envir = parent.frame()) {
  withr::local_preserve_seed(.local_envir = envir)
  set.seed(seed)
  invisible(seed)
}
