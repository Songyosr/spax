tune_decay_params <- function(distance_raster,
                              demand_raster,
                              supply,
                              observed,
                              sigma_range = seq(20, 100, by = 10),
                              a0_range = seq(0.1, 2, by = 0.2),
                              n_cores = parallel::detectCores() - 1,
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

  # Create parameter grid
  param_grid <- expand.grid(sigma = sigma_range, a0 = a0_range)
  if (debug) cat("Parameter grid created with", nrow(param_grid), "parameter combinations\n")

  # Determine if we're on Windows
  is_windows <- Sys.info()["sysname"] == "Windows"
  if (is_windows) {
    if (debug) cat("Windows detected - falling back to sequential processing\n")
    n_cores <- 1
  } else {
    if (debug) cat("Using parallel processing with", n_cores, "cores\n")
  }

  # Process using mclapply (falls back to lapply on Windows)
  if (debug) cat("Starting computations...\n")

  results_list <- parallel::mclapply(1:nrow(param_grid),
                                     mc.cores = n_cores,
                                     FUN = function(i) {
                                       params <- param_grid[i,]
                                       if (debug) cat("Processing sigma=", params$sigma, ", a0=", params$a0, "\n")

                                       tryCatch({
                                         result <- calc_params_fit(
                                           sigma = params$sigma,
                                           a0 = params$a0,
                                           distance_raster = distance_raster,
                                           demand_raster = demand_raster,
                                           supply = supply,
                                           observed = observed,
                                           debug = debug
                                         )

                                         if (debug) cat("Calculation complete for sigma=", params$sigma, ", a0=", params$a0, "\n")
                                         result

                                       }, error = function(e) {
                                         message(sprintf("Error with sigma=%s, a0=%s: %s",
                                                         params$sigma, params$a0, conditionMessage(e)))
                                         list(
                                           error = TRUE,
                                           sigma = params$sigma,
                                           a0 = params$a0,
                                           error_msg = conditionMessage(e)
                                         )
                                       })
                                     })

  # Process results
  is_error <- sapply(results_list, function(x) is.list(x) && !is.null(x$error) && x$error == TRUE)
  error_results <- results_list[is_error]
  valid_results <- results_list[!is_error]

  if (length(error_results) > 0) {
    warning("Some computations failed with errors:")
    for (er in error_results) {
      warning(sprintf("  Sigma: %s  A0: %s  Error: %s",
                      er$sigma, er$a0, er$error_msg))
    }
  }

  if (length(valid_results) == 0) {
    stop("All computations failed. Check the printed error messages above.")
  }

  # Combine results
  results_df <- do.call(rbind, lapply(valid_results, function(x) {
    data.frame(
      sigma = x$sigma,
      a0 = x$a0,
      log_likelihood = x$log_likelihood,
      rmse = x$rmse,
      correlation = x$correlation
    )
  }))

  predictions_mat <- do.call(rbind, lapply(valid_results, function(x) x$predicted))

  if (debug) cat("All computations finished.\n")

  return(list(
    results = results_df,
    predictions = predictions_mat,
    errors = error_results,
    timing = system.time({NULL})  # Add timing information
  ))
}
# tune_decay_params <- function(distance_raster,
#                               demand_raster,
#                               supply,
#                               observed,
#                               sigma_range = seq(20, 100, by = 10),
#                               a0_range = seq(0.1, 2, by = 0.2),
#                               debug = FALSE) {
#
#   # Input validation
#   if(!inherits(distance_raster, "SpatRaster")) {
#     stop("distance_raster must be a SpatRaster object")
#   }
#   if(nlyr(distance_raster) != length(supply)) {
#     stop("Number of distance layers must match length of supply vector")
#   }
#   if(length(supply) != length(observed)) {
#     stop("Supply and observed vectors must have same length")
#   }
#
#   # Create parameter grid
#   param_grid <- expand.grid(sigma = sigma_range, a0 = a0_range)
#   if (debug) cat("Parameter grid created with", nrow(param_grid), "parameter combinations\n")
#
#   # Sequential processing using lapply
#   if (debug) cat("Starting computations...\n")
#
#   results_list <- lapply(1:nrow(param_grid), function(i) {
#     params <- param_grid[i,]
#     if (debug) cat("Processing sigma=", params$sigma, ", a0=", params$a0, "\n")
#
#     tryCatch({
#       result <- calc_params_fit(
#         sigma = params$sigma,
#         a0 = params$a0,
#         distance_raster = distance_raster,
#         demand_raster = demand_raster,
#         supply = supply,
#         observed = observed,
#         debug = debug
#       )
#
#       if (debug) cat("Calculation complete for sigma=", params$sigma, ", a0=", params$a0, "\n")
#       result
#
#     }, error = function(e) {
#       message(sprintf("Error with sigma=%s, a0=%s: %s",
#                       params$sigma, params$a0, conditionMessage(e)))
#       list(
#         error = TRUE,
#         sigma = params$sigma,
#         a0 = params$a0,
#         error_msg = conditionMessage(e)
#       )
#     })
#   })
#
#   # Process results
#   is_error <- sapply(results_list, function(x) is.list(x) && !is.null(x$error) && x$error == TRUE)
#   error_results <- results_list[is_error]
#   valid_results <- results_list[!is_error]
#
#   if (length(error_results) > 0) {
#     warning("Some computations failed with errors:")
#     for (er in error_results) {
#       warning(sprintf("  Sigma: %s  A0: %s  Error: %s",
#                       er$sigma, er$a0, er$error_msg))
#     }
#   }
#
#   if (length(valid_results) == 0) {
#     stop("All computations failed. Check the printed error messages above.")
#   }
#
#   # Combine results
#   results_df <- do.call(rbind, lapply(valid_results, function(x) {
#     data.frame(
#       sigma = x$sigma,
#       a0 = x$a0,
#       log_likelihood = x$log_likelihood,
#       rmse = x$rmse,
#       correlation = x$correlation
#     )
#   }))
#
#   predictions_mat <- do.call(rbind, lapply(valid_results, function(x) x$predicted))
#
#   if (debug) cat("All computations finished.\n")
#
#   return(list(
#     results = results_df,
#     predictions = predictions_mat,
#     errors = error_results
#   ))
# }

calc_params_fit <- function(sigma, a0, distance_raster, demand_raster, supply, observed, debug = FALSE) {
  if (debug) cat("[calc_params_fit]: Start with sigma=", sigma, ", a0=", a0, "\n")

  if (debug) {
    cat("[calc_params_fit]: Computing weights...\n")
    cat("dim(distance_raster): ", dim(distance_raster), "\n")
    }
  Wr <- spax::compute_weights(distance_raster, method = "gaussian", sigma = sigma)

  if (debug) cat("[calc_params_fit]: Gathering demand...\n")
  demand_by_site <- spax::gather_demand(demand_raster, Wr)

  if (debug && is.null(demand_by_site$potential_demand)) {
    cat("[calc_params_fit]: potential_demand is NULL!\n")
  }

  supply_demand_ratios <- supply / demand_by_site$potential_demand

  if (debug) {
    cat("[calc_params_fit]: Spreading access...\n")
    cat("ratios: ", supply_demand_ratios, "\n")
    cat("dim(Wr): ", dim(Wr), "\n")
  }
  accessibility <- spax::spread_access(supply_demand_ratios, Wr, full_output = T)

  if (debug) cat("[calc_params_fit]: Calculating total demand and predictions...\n")
  #total_demand <- global(demand_raster, "sum", na.rm = TRUE)$sum
  predicted <- calc_choice_distribution(accessibility$site_specific, demand_raster, a0)

  if (debug) cat("[calc_params_fit]: Calculating fit metrics...\n")
  fit_metrics <- calc_fit_metrics(observed, predicted)

  if (debug) cat("[calc_params_fit]: Done with sigma=", sigma, ", a0=", a0, "\n")

  list(
    sigma = sigma,
    a0 = a0,
    log_likelihood = fit_metrics$log_likelihood,
    rmse = fit_metrics$rmse,
    correlation = fit_metrics$correlation,
    compare = data.frame(observed = observed, predicted = predicted)
  )
}

calc_choice_distribution <- function(accessibility, demand_raster, a0) {
  # Calculate sum of accessibility across all facilities for each location
  access_sum <- sum(accessibility, na.rm = TRUE)
#
#   cat("dim access: ", dim(accessibility), "\n")
#   cat("dim access_sum: ", dim(access_sum) , "\n")
#   cat("dim demand: ", dim(demand_raster), "\n")

  # For each facility layer, calculate probability P(k,j) = A(k,j)/(sum(A(k,*)) + a0)
  probs <- accessibility / (access_sum + a0)

  #cat("dim probs: ", dim(probs), "\n")

  # Sum probabilities across all locations and multiply by total demand
  # This gives predicted demand for each facility
  tmp <- global(probs, 'sum',
                weights = demand_raster,
                na.rm = TRUE)$weighted_sum

  tmp
}

calc_fit_metrics <- function(observed, predicted) {

  # Small constant to avoid log(0)
  epsilon <- 1e-10
  # Ensure predicted values are positive
  predicted <- pmax(predicted, epsilon)

  list(
    log_likelihood = sum(dpois(observed+ epsilon, predicted+ epsilon, log = TRUE)),
    rmse = sqrt(mean((observed - predicted)^2)),
    correlation = cor(observed, predicted)
  )
}

plot_decay_tuning <- function(results, observed) {
  p1 <- ggplot(results$results, aes(x = sigma, y = a0)) +
    geom_tile(aes(fill = log_likelihood)) +
    scale_fill_viridis_c(option = "plasma") +
    labs(title = "Parameter Space",
         x = "Sigma (Distance Decay)",
         y = "A0 (No-Care Threshold)") +
    theme_minimal()

  best_idx <- which.max(results$results$log_likelihood)
  best_params <- results$results[best_idx,]
  best_pred <- results$predictions[best_idx,]

  p2 <- ggplot(data.frame(Observed = observed, Predicted = best_pred),
               aes(x = Observed, y = Predicted)) +
    geom_point() +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
    labs(title = sprintf("Best Fit\nσ=%.1f, A0=%.2f",
                         best_params$sigma,
                         best_params$a0)) +
    theme_minimal()

  gridExtra::grid.arrange(p1, p2, ncol = 2)
}
