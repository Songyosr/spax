# Benchmark setup for access chain operations
library(microbenchmark)

# Setup test data
n_facilities <- 1000
n_iter <- 100
lambda <- 0.3

# Generate test data
set.seed(123)
supply <- runif(n_facilities, 10, 100)
weights <- matrix(runif(n_facilities * n_facilities), n_facilities, n_facilities)
demand <- runif(n_facilities, 50, 200)

# 1. List-based approach
setup_list <- function() {
  list(
    attractiveness = supply,
    utilization = numeric(n_facilities),
    ratio = numeric(n_facilities)
  )
}

update_list <- function(state, weights, supply, lambda) {
  # Choice probabilities
  huff_probs <- state$attractiveness * weights
  huff_probs <- sweep(huff_probs, 1, rowSums(huff_probs), '/')

  # Utilization
  util_probs <- huff_probs * weights
  new_util <- colSums(sweep(util_probs, 1, demand, '*'))

  # Ratios and attractiveness
  new_ratio <- supply / new_util
  new_attract <- (1 - lambda) * state$attractiveness + lambda * new_ratio

  list(
    attractiveness = new_attract,
    utilization = new_util,
    ratio = new_ratio
  )
}

# 2. Matrix-based approach (single iteration)
setup_matrix <- function() {
  matrix(
    c(supply, rep(0, 2*n_facilities)),
    nrow = n_facilities,
    ncol = 3  # [attractiveness, utilization, ratio]
  )
}

update_matrix <- function(state, weights, supply, lambda) {
  # Extract current values
  attract <- state[,1]

  # Choice probabilities
  huff_probs <- attract * weights
  huff_probs <- sweep(huff_probs, 1, rowSums(huff_probs), '/')

  # Utilization
  util_probs <- huff_probs * weights
  new_util <- colSums(sweep(util_probs, 1, demand, '*'))

  # Ratios and attractiveness
  new_ratio <- supply / new_util
  new_attract <- (1 - lambda) * attract + lambda * new_ratio

  # Update state
  state[,1] <- new_attract
  state[,2] <- new_util
  state[,3] <- new_ratio
  state
}

# 3. Array-based approach (with history)
setup_array <- function() {
  array(0, dim = c(n_iter, n_facilities, 3))
}

update_array <- function(state, iter, weights, supply, lambda) {
  # Extract current values
  if(iter == 1) {
    attract <- supply
  } else {
    attract <- state[iter-1,,1]
  }

  # Choice probabilities
  huff_probs <- attract * weights
  huff_probs <- sweep(huff_probs, 1, rowSums(huff_probs), '/')

  # Utilization
  util_probs <- huff_probs * weights
  new_util <- colSums(sweep(util_probs, 1, demand, '*'))

  # Ratios and attractiveness
  new_ratio <- supply / new_util
  new_attract <- (1 - lambda) * attract + lambda * new_ratio

  # Update state
  state[iter,,1] <- new_attract
  state[iter,,2] <- new_util
  state[iter,,3] <- new_ratio
  state
}

# Run benchmark
benchmark_results <- microbenchmark(
  list = {
    state <- setup_list()
    state <- update_list(state, weights, supply, lambda)
  },
  matrix = {
    state <- setup_matrix()
    state <- update_matrix(state, weights, supply, lambda)
  },
  array = {
    state <- setup_array()
    state <- update_array(state, 1, weights, supply, lambda)
  },
  times = 100
)

print(benchmark_results)

# Test convergence speed
convergence_benchmark <- microbenchmark(
  list = {
    prev <- setup_list()
    curr <- update_list(prev, weights, supply, lambda)
    max(abs(curr$utilization - prev$utilization))
  },
  matrix = {
    state <- setup_matrix()
    new_state <- update_matrix(state, weights, supply, lambda)
    max(abs(new_state[,2] - state[,2]))
  },
  array = {
    state <- setup_array()
    state <- update_array(state, 1, weights, supply, lambda)
    state <- update_array(state, 2, weights, supply, lambda)
    max(abs(state[2,,2] - state[1,,2]))
  },
  times = 10
)

print(convergence_benchmark)
