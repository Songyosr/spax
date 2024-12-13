tune_decay_params_optim_parallel <- function(distance_raster,
                                             demand_raster,
                                             supply,
                                             observed,
                                             n_starts = 10,  # Number of random starts
                                             n_cores = parallel::detectCores() - 1,
                                             lower = c(sigma = 10, a0 = 0.1),
                                             upper = c(sigma = 100, a0 = 2),
                                             method = "L-BFGS-B",
                                             debug = FALSE) {

  # Input validation
  if(!inherits(distance_raster, "SpatRaster")) {
    stop("distance_raster must be a SpatRaster object")
  }
  if(nlyr(distance_raster) != length(supply)) {
    stop("Number of distance layers must match length of supply vector")
  }
  if(length(supply) != length(observed)) {
    stop("Supply and observed vectors must have same length")
  }

  # Generate random starting points
  set.seed(123)  # for reproducibility
  starting_points <- replicate(n_starts, c(
    sigma = runif(1, lower["sigma"], upper["sigma"]),
    a0 = runif(1, lower["a0"], upper["a0"])
  ), simplify = FALSE)

  if (debug) {
    cat("Generated", n_starts, "random starting points\n")
    print(do.call(rbind, starting_points))
  }

  # Determine if we're on Windows
  is_windows <- Sys.info()["sysname"] == "Windows"
  if (is_windows) {
    if (debug) cat("Windows detected - falling back to sequential processing\n")
    n_cores <- 1
  }

  # Run parallel optimizations
  if (debug) cat("Starting parallel optimizations with", n_cores, "cores...\n")

  results_list <- parallel::mclapply(starting_points, function(init_params) {
    if (debug) cat("Starting optimization from sigma =", init_params[1], ", a0 =", init_params[2], "\n")

    tune_decay_params_optim(
      distance_raster = distance_raster,
      demand_raster = demand_raster,
      supply = supply,
      observed = observed,
      init_params = init_params,
      lower = lower,
      upper = upper,
      method = method,
      debug = debug
    )
  }, mc.cores = n_cores)

  # Find best result
  log_likes <- sapply(results_list, function(x) x$log_likelihood)
  best_idx <- which.max(log_likes)
  best_result <- results_list[[best_idx]]

  # Summarize all results
  all_params <- do.call(rbind, lapply(results_list, function(x) {
    data.frame(
      sigma = x$parameters["sigma"],
      a0 = x$parameters["a0"],
      log_likelihood = x$log_likelihood,
      rmse = x$rmse,
      correlation = x$correlation,
      converged = x$convergence
    )
  }))

  if (debug) {
    cat("\nOptimization complete. Results summary:\n")
    print(summary(all_params))
    cat("\nBest result found:\n")
    print(best_result$parameters)
  }

  # Return results
  return(list(
    best_fit = best_result,
    all_results = all_params,
    starting_points = do.call(rbind, starting_points)
  ))
}

tune_decay_params_optim <- function(distance_raster,
                                    demand_raster,
                                    supply,
                                    observed,
                                    init_params = c(sigma = 10, a0 = 0.01),
                                    lower = c(sigma = 1, a0 = 0.001),
                                    upper = c(sigma = 50, a0 = 0.1),
                                    method = "L-BFGS-B",
                                    debug = FALSE) {

  # Store best results globally
  best_pred <- NULL
  best_fit <- NULL
  best_cor <- -Inf

  # Objective function for optimization
  objective_fn <- function(params) {
    if (debug) cat("Testing sigma =", params[1], ", a0 =", params[2], "\n")

    tryCatch({
      result <- calc_params_fit(
        sigma = params[1],
        a0 = params[2],
        distance_raster = distance_raster,
        demand_raster = demand_raster,
        supply = supply,
        observed = observed,
        debug = debug
      )

      # Use negative correlation as objective (since optim minimizes)
      cor_val <- -result$correlation

      # Store best result
      if (!is.na(cor_val) && cor_val < -best_cor) {
        best_pred <<- result$predicted
        best_fit <<- result
        best_cor <<- -cor_val
      }

      if (debug) cat("  Correlation =", -cor_val, "\n")
      return(cor_val)

    }, error = function(e) {
      warning("Error in parameter evaluation: ", e$message)
      return(Inf)
    })
  }

  # Run optimization
  if (debug) cat("Starting optimization...\n")

  opt_result <- optim(
    par = init_params,
    fn = objective_fn,
    method = method,
    lower = lower,
    upper = upper,
    control = list(
      trace = if(debug) 1 else 0,
      maxit = 100
    )
  )

  if (debug) cat("\nOptimization completed.\n")

  # Get final parameter estimates
  best_params <- opt_result$par
  names(best_params) <- c("sigma", "a0")

  # Run one final time to ensure we have the predictions
  final_fit <- calc_params_fit(
    sigma = best_params["sigma"],
    a0 = best_params["a0"],
    distance_raster = distance_raster,
    demand_raster = demand_raster,
    supply = supply,
    observed = observed,
    debug = FALSE
  )

  # Return results
  return(list(
    parameters = best_params,
    correlation = -opt_result$value,
    convergence = opt_result$convergence == 0,
    message = if(opt_result$convergence == 0) "Optimization converged successfully"
    else "Warning: Optimization may not have converged",
    rmse = final_fit$rmse,
    predicted = final_fit$predicted,
    compare = data.frame(
      observed = observed,
      predicted = final_fit$predicted
    ),
    optim_details = opt_result
  ))
}


# Helper function to plot optimization results
plot_optim_results <- function(results) {
  par(mfrow = c(2, 2))

  # Plot starting points and final positions
  plot(results$starting_points[,1], results$starting_points[,2],
       xlab = "Sigma", ylab = "a0", main = "Starting Points",
       col = "blue", pch = 16)

  plot(results$all_results$sigma, results$all_results$a0,
       xlab = "Sigma", ylab = "a0", main = "Final Positions",
       col = "red", pch = 16)
  points(results$best_fit$parameters["sigma"],
         results$best_fit$parameters["a0"],
         col = "green", pch = 16, cex = 2)

  # Plot log-likelihood distribution
  hist(results$all_results$log_likelihood,
       main = "Log-likelihood Distribution",
       xlab = "Log-likelihood")

  # Plot predicted vs observed for best fit
  plot(results$best_fit$predicted, observed,
       xlab = "Predicted", ylab = "Observed",
       main = paste("Best Fit\nR =",
                    round(results$best_fit$correlation, 3)))
  abline(0, 1, col = "red", lty = 2)

  par(mfrow = c(1, 1))
}
