# Functions for classifying behaviour using Hidden Markov Models (HMMs)
# supported by a hmmTMB backend. These functions are all internal to the
# package.

#' INTERNAL Classify activity states using Hidden Markov Models
#'
#' Main internal HMM classification function. Fits HMMs to detection data with
#' automatic distribution selection for each feature. Supports per-individual
#' fitting (recommended for stability when dealing with multiple individuals)
#' and handles temporal breaks via video IDs.
#'
#' @param dets Data frame containing detection data with feature columns
#' @param features Character vector of feature column names to use in HMM
#' @param n_states Integer. Number of hidden states to fit (default 2)
#' @param indiv_id Character. Column name for individual ID. If provided, fits
#'   separate HMMs per individual (recommended). Default NULL fits single HMM
#'   to all data.
#' @param video_id Character. Column name for video/temporal ID used to handle
#'   temporal breaks within individuals. Default NULL.
#'
#' @return Data frame identical to \code{dets} with two columns added:
#'   \itemize{
#'     \item \code{state}: Integer state assignment (1 to n_states)
#'     \item \code{state_prob}: Probability of the assigned state
#'   }
#'   Returns NA for rows with incomplete feature data or if fitting fails.
#'
#' @details
#' The function performs the following steps:
#' \enumerate{
#'   \item Validates that all required features exist in the data
#'   \item If \code{indiv_id} provided, fits separate HMMs per individual
#'   \item Calls \code{.fit_individual_hmm} for each individual or dataset
#'   \item Standardizes state labels so higher states = higher activity
#' }
#'
#' Individuals with fewer than 10 observations are skipped with a warning.
#'
#' @keywords internal

.classify_hmm <- function(
  dets,
  features,
  n_states = 2,
  indiv_id = NULL,
  video_id = NULL
) {

  # Check that features are available
  missing_features <- setdiff(features, colnames(dets))
  if (length(missing_features) > 0) {
    stop("The following features are missing from the detections data frame: ",
         paste(missing_features, collapse = ", "))
  }

  # Validate ID inputs
  if (!is.null(indiv_id) && !(indiv_id %in% colnames(dets))) {
    stop("Individual ID column '", indiv_id,
         "' not found in detections data frame.")
  }

  if (!is.null(video_id) && !(video_id %in% colnames(dets))) {
    stop("Video ID column '", video_id, "' not found in detections data frame.")
  }

  # Per individual fitting
  if (!is.null(indiv_id)) {

    individuals <- unique(dets[[indiv_id]])


    dets$state <- NA_integer_
    dets$state_prob <- NA_real_

    for (i in seq_along(individuals)) {
      indiv <- individuals[i]

      indiv_idx <- dets[[indiv_id]] == indiv
      indiv_data <- dets[indiv_idx, ]

      # Check if individual has enough data
      if (nrow(indiv_data) < 10) {
        warning("Not enough data to fit HMM for individual '", indiv,
                "'. Skipping.")
        next
      }

      indiv_result <- .fit_individual_hmm(
        indiv_data,
        features,
        n_states,
        id_var = video_id
      )

      indiv_result <- .standardise_state_labels(indiv_result, features)

      dets$state[indiv_idx] <- indiv_result$state
      dets$state_prob[indiv_idx] <- indiv_result$state_prob
    }
  } else {
    # If no individual ID provided fit to all data at once
    dets <- .fit_individual_hmm(
      dets, features, n_states, id_var = video_id
    )
    dets <- .standardise_state_labels(dets, features)
  }

  return(dets)
}


#' Fit HMM for a single individual or dataset
#'
#' Performs the full HMM fitting pipeline: distribution selection, data
#' preparation, model fitting, and state extraction. Handles temporal breaks via
#' id_var (e.g., vid_id).
#' Returns input data frame with state and state_prob columns, and attributes
#' for model and distributions.
#'
#' @param dets Data frame containing feature columns and optional id_var
#' @param features Character vector of feature column names
#' @param n_states Integer. Number of hidden states
#' @param id_var Character. Column name for temporal breaks (e.g., vid_id)
#'
#' @return The input data frame with two columns added:
#'   \item{state}{Integer state assignment (1 to n_states),
#'                NA for incomplete rows}
#'   \item{state_prob}{Numeric probability of the assigned state}
#'
#'   Additionally, attributes are attached:
#'   \itemize{
#'     \item \code{hmm_model}: The fitted hmmTMB model object
#'     \item \code{feature_distributions}: List of distribution info for each
#'                                         feature
#'   }
#'
#' @keywords internal
#' @importFrom stats complete.cases

.fit_individual_hmm <- function(
  dets,
  features,
  n_states,
  id_var = NULL
) {

  ## 1. SELECT DISTRIBUTIONS FOR FEATURES --------------------------------------
  feature_distributions <- list()
  for (feat in features) {
    feature_distributions[[feat]] <- .auto_select_distribution(
      dets[[feat]],
      feature_name = feat
    )
  }

  ## 2. GET COMPLETE CASES -----------------------------------------------------
  feature_matrix <- dets[, features, drop = FALSE]
  complete_idx <- complete.cases(feature_matrix)

  # Include id_var column in complete_data if specified (needed for temporal
  # breaks)
  cols_to_include <- features
  if (!is.null(id_var) && id_var %in% names(dets)) {
    cols_to_include <- c(cols_to_include, id_var)
  }

  complete_data <- dets[complete_idx, cols_to_include, drop = FALSE]

  # Handle zero-shifted data for gamma/lnorm
  for (feat in features) {
    if (isTRUE(feature_distributions[[feat]]$shifted)) {
      min_positive <- min(
        complete_data[[feat]][complete_data[[feat]] > 0], na.rm = TRUE
      )
      epsilon <- min_positive * 0.0001
      complete_data[[feat]] <- complete_data[[feat]] + epsilon
    }
  }

  ## 3. FIT HMM ----------------------------------------------------------------
  fitted_model <- .fit_hmmTMB_wrapper(
    data = complete_data,
    features = features,
    n_states = n_states,
    feature_distributions = feature_distributions,
    id_var = id_var,
    max_restarts = 3
  )

  if (is.null(fitted_model)) {
    warning("HMM fitting failed. Returning NA states.")
    dets$state <- NA_integer_
    dets$state_prob <- NA_real_
    return(dets)
  }

  ## 4. DECODE STATES ----------------------------------------------------------
  state_results <- .extract_hmm_states(
    fitted_model,
    n_obs = nrow(dets),
    complete_idx = complete_idx
  )

  dets$state <- state_results$states
  dets$state_prob <- state_results$state_probs

  attr(dets, "hmm_model") <- fitted_model
  attr(dets, "feature_distributions") <- feature_distributions

  return(dets)
}


#' Standardise state labels (higher state = higher activity)
#'
#' Reorders state labels so that higher state numbers correspond to higher
#' activity levels. Uses speed as the activity metric if available, otherwise
#' uses the first feature.
#'
#' @param dets Data frame with a \code{state} column containing integer state
#'   assignments
#' @param features Character vector of feature names (used as fallback if
#'   \code{speed} column is not present)
#'
#' @return Data frame with state column relabelled such that:
#'   \itemize{
#'     \item State 1 = lowest mean activity (e.g., resting)
#'     \item State 2 = higher activity
#'     \item State n = highest activity (for n_states > 2)
#'   }
#'
#' @keywords internal
#' @importFrom stats setNames

.standardise_state_labels <- function(dets, features) {

  # Use speed if available
  activity_feature <- if ("speed" %in% names(dets)) {
    "speed"
  } else {
    features[1]
  }

  states_present <- unique(dets$state[!is.na(dets$state)])

  if (length(states_present) < 2) {
    return(dets)
  }

  state_means <- tapply(
    dets[[activity_feature]],
    dets$state,
    mean,
    na.rm = TRUE
  )

  if (length(states_present) == 2) {
    if (state_means["1"] > state_means["2"]) {
      dets$state <- ifelse(dets$state == 1L, 2L,
                           ifelse(dets$state == 2L, 1L, dets$state))
    }
  } else {
    state_order <- order(state_means)
    state_map <- setNames(
      seq_along(state_order), names(state_means)[state_order]
    )
    dets$state <- state_map[as.character(dets$state)]
  }

  return(dets) # nolint: return_linter.
}


#' Automatically select the best-fitting distribution for a feature
#'
#' Uses data properties and AIC comparison to select the most appropriate
#' distribution from those supported by hmmTMB. Handles angular data,
#' positive continuous data, and bounded (0, 1) data.
#' Feature name heuristics are used for angular detection. Returns distribution
#' name, estimated parameters, AIC, and method used for selection.
#'
#' @param values Numeric vector of feature values
#' @param feature_name Character. Name of the feature (used to detect angular
#'   data based on naming conventions like "turning", "angle", "heading")
#'
#' @return A list containing:
#'   \item{distribution}{Character string naming the selected distribution}
#'   \item{params}{List of estimated distribution parameters}
#'   \item{aic}{AIC value for the selected distribution (if fitted)}
#'   \item{method}{How the distribution was selected ("fitted" or "default")}
#'
#' @details
#' Supported distributions:
#' \itemize{
#'   \item \code{norm}: Gaussian - for unbounded continuous data
#'   \item \code{gamma}: Gamma - for positive, right-skewed data (e.g., speed)
#'   \item \code{lnorm}: Log-normal - for positive, right-skewed data
#'   \item \code{vm}: von Mises - for angular/circular data
#'                    (e.g., turning angle)
#'   \item \code{beta}: Beta - for data bounded on (0, 1)
#' }
#'
#' @importFrom fitdistrplus fitdist
#' @importFrom CircStats vm.ml
#' @importFrom stats sd var
#'
#' @keywords internal

.auto_select_distribution <- function(
  values,
  feature_name = ""
) {

  # Remove NAs
  values <- values[!is.na(values)]
  n <- length(values)

  # Default result
  default_result <- list(
    distribution = "norm",
    params = list(mean = mean(values), sd = sd(values)),
    aic = NA_real_,
    method = "default"
  )

  if (n < 30) {
    warning("Too few observations (n=", n, ")
             when trying to determine distribution for feature '", feature_name,
            "'. defaulting to Gaussian")
    return(default_result)
  }

  # Data properties
  min_val <- min(values)
  max_val <- max(values)
  has_negative <- min_val < 0
  has_zero <- any(values == 0)

  # Check if angular data based on feature name ONLY
  # Don't auto-detect based on data range - this causes false positives for
  # features like acceleration that happen to have small values in [-pi, pi]
  angular_keywords <- c("turn", "angle", "heading", "direction", "bearing")
  is_angular <- any(sapply(
    angular_keywords, function(k) grepl(k, feature_name, ignore.case = TRUE)
  ))

  # Check if bounded [0, 1]
  is_bounded_01 <- min_val >= 0 && max_val <= 1

  # ANGULAR DATA: Use von Mises
  if (is_angular) {

    # Fit von Mises using CircStats
    vm_params <- tryCatch({
      # CircStats::vm.ml returns list with mu and kappa
      CircStats::vm.ml(values)
    }, error = function(e) NULL)

    if (!is.null(vm_params)) {
      # Cap kappa to prevent numerical overflow
      # (very high kappa = nearly point mass)
      kappa_capped <- min(vm_params$kappa, 50)
      if (vm_params$kappa > 50) {
        warning(paste0(
          "Kappa capped from ", round(vm_params$kappa, 1), " to 50\n",
          "values for feature '", feature_name, "' may be too concentrated."
        ))
      }
      return(list(
        distribution = "vm",
        params = list(mu = vm_params$mu, kappa = kappa_capped),
        aic = NA_real_,  # CircStats doesn't provide AIC directly
        method = "fitted"
      ))
    } else {
      # Fallback: estimate parameters manually
      # Mean direction using circular mean
      mean_dir <- atan2(mean(sin(values)), mean(cos(values)))
      # Concentration: rough estimate from resultant length
      r <- sqrt(mean(cos(values))^2 + mean(sin(values))^2)
      kappa <- ifelse(r < 0.53, 2 * r + r^3 + 5 * r^5 / 6,
                      ifelse(r < 0.85, -0.4 + 1.39 * r + 0.43 / (1 - r),
                             1 / (r^3 - 4 * r^2 + 3 * r)))

      return(list(
        distribution = "vm",
        params = list(mu = mean_dir, kappa = max(0.1, kappa)),
        aic = NA_real_,
        method = "estimated"
      ))
    }
  }


  # BOUNDED [0,1] DATA: Use beta
  if (is_bounded_01 && !has_zero && max_val < 1) {

    # Need to handle edge cases for beta
    # Shrink slightly away from 0 and 1
    values_adj <- pmax(0.001, pmin(0.999, values))

    beta_fit <- tryCatch({
      fitdistrplus::fitdist(values_adj, "beta", method = "mle")
    }, error = function(e) NULL)

    norm_fit <- tryCatch({
      fitdistrplus::fitdist(values, "norm", method = "mle")
    }, error = function(e) NULL)

    # Compare AICs
    if (!is.null(beta_fit) && !is.null(norm_fit)) {
      if (beta_fit$aic < norm_fit$aic) {
        return(list(
          distribution = "beta",
          params = as.list(beta_fit$estimate),
          aic = beta_fit$aic,
          method = "fitted"
        ))
      }
    } else if (!is.null(beta_fit)) {
      return(list(
        distribution = "beta",
        params = as.list(beta_fit$estimate),
        aic = beta_fit$aic,
        method = "fitted"
      ))
    }

    # Fall through to norm if beta didn't work or wasn't better
  }


  # NEGATIVE VALUES: Only Gaussian
  if (has_negative) {
    fit <- tryCatch({
      fitdistrplus::fitdist(values, "norm", method = "mle")
    }, error = function(e) NULL)

    if (!is.null(fit)) {
      return(list(
        distribution = "norm",
        params = as.list(fit$estimate),
        aic = fit$aic,
        method = "fitted"
      ))
    } else {
      return(default_result)
    }
  }


  # POSITIVE DATA (possibly with zeros): Compare gamma, lnorm, norm
  if (has_zero) {
    # Shift data slightly for gamma/lnorm fitting
    min_positive <- min(values[values > 0])
    epsilon <- min_positive * 0.0001
    values_shifted <- values + epsilon
  } else {
    values_shifted <- values
  }

  candidates <- c("norm", "gamma", "lnorm")
  fits <- list()

  for (dist in candidates) {
    fits[[dist]] <- tryCatch({
      if (dist %in% c("gamma", "lnorm")) {
        fitdistrplus::fitdist(values_shifted, dist, method = "mle")
      } else {
        fitdistrplus::fitdist(values, dist, method = "mle")
      }
    }, error = function(e) NULL)
  }

  # Find best by AIC
  aics <- sapply(fits, function(f) if (is.null(f)) Inf else f$aic)
  best_dist <- names(which.min(aics))

  if (is.infinite(aics[best_dist])) {
    return(default_result)
  }

  return(list(
    distribution = best_dist,
    params = as.list(fits[[best_dist]]$estimate),
    aic = aics[best_dist],
    method = "fitted",
    shifted = has_zero  # Flag if data was shifted
  ))
}

#' Fit HMM with robust error handling and multiple restarts
#'
#' Attempts to fit an HMM using hmmTMB, retrying up to max_restarts times if
#' fitting fails.
#' Returns the best fitted model (highest log-likelihood) or NULL if all
#' attempts fail.
#'
#' @param data Data frame containing features and optional ID column for
#'   temporal breaks
#' @param features Character vector of feature column names
#' @param n_states Integer. Number of hidden states
#' @param feature_distributions Named list of distribution info
#' @param id_var Character. Name of column for temporal breaks (e.g., video_ID)
#' @param max_restarts Integer. Maximum fitting attempts
#'
#' @return Fitted hmmTMB model, or NULL if all attempts failed
#'
#' @keywords internal

.fit_hmmTMB_wrapper <- function( # nolint: object_name_linter.
  data,
  features,
  n_states,
  feature_distributions,
  id_var = NULL,
  max_restarts = 3
) {
  # Max restarts must be greater than 1
  if (max_restarts < 1) {
    max_restarts <- 1
    warning("max_restarts must be at least 1. Setting to 1.")
  }

  best_fit <- NULL
  best_nll <- Inf

  for (attempt in 1:max_restarts) {
    fit_result <- .fit_hmmTMB(
      data = data,
      features = features,
      n_states = n_states,
      feature_distributions = feature_distributions,
      id_var = id_var
    )

    if (!is.null(fit_result)) {
      # hmmTMB uses llk() for log-likelihood (not nll)
      current_llk <- tryCatch({
        llk_val <- fit_result$llk()
        llk_val
      }, error = function(e) {
        -Inf
      })

      # Higher log-likelihood is better (compare with negative for minimization
      # logic)
      if (is.finite(current_llk) && (-current_llk) < best_nll) {
        best_fit <- fit_result
        best_nll <- -current_llk  # Store as negative for comparison

        break
      }
    }
  }

  if (is.null(best_fit)) {
    warning("HMM fitting failed after ", max_restarts, " attempts")
  }

  return(best_fit)
}


#' INTERNAL Fit Hidden Markov Model using hmmTMB
#'
#' Core HMM fitting function that creates and fits an hmmTMB model. Handles
#' distribution specification, parameter initialization (via GMM or
#' quantile-based defaults), and model fitting with multiple optimizer
#' fallbacks.
#'
#' @param data Data frame containing feature columns and optional ID column
#' @param features Character vector of feature column names
#' @param n_states Integer. Number of hidden states
#' @param feature_distributions Named list of distribution specifications from
#'   \code{.auto_select_distribution}
#' @param initial_params Optional list of initial parameter values. If NULL,
#'   uses GMM-based initialization or quantile-based defaults
#' @param id_var Character. Column name for temporal breaks. If provided, the
#'   column will be duplicated as 'ID' for hmmTMB integration
#'
#' @return Fitted hmmTMB model object, or NULL if fitting fails
#'
#' @details
#' Fitting procedure:
#' \enumerate{
#'   \item Maps distribution names to hmmTMB format
#'   \item Initializes parameters via GMM (\code{.estimate_initial_params_gmm})
#'   \item Falls back to quantile-based initialization if GMM fails
#'   \item Creates hmmTMB Observation and MarkovChain objects
#'   \item Attempts fitting with default optimizer
#'   \item Falls back to BFGS optimizer if default fails
#'   \item Returns NULL with informative error messages if all attempts fail
#' }
#'
#' Handles special cases:
#' \itemize{
#'   \item Ensures positive values for gamma distributions
#'   \item Caps von Mises kappa to prevent overflow
#'   \item Constrains beta parameters for numerical stability
#' }
#'
#' @importFrom hmmTMB Observation MarkovChain HMM
#' @importFrom stats quantile
#'
#' @keywords internal

.fit_hmmTMB <- function( # nolint: object_name_linter.
  data,
  features,
  n_states,
  feature_distributions,
  initial_params = NULL,
  id_var = NULL
) {

  # Build distribution specification for hmmTMB
  dists <- list()

  for (feat in features) {
    dist_name <- feature_distributions[[feat]]$distribution

    # Map distribution names to hmmTMB names
    hmmTMB_dist <- switch( # nolint: object_name_linter.
      dist_name,
      "norm" = "norm",
      "gamma" = "gamma",
      "lnorm" = "lnorm",
      "vm" = "vm",
      "beta" = "beta",
      "norm"  # default
    )

    dists[[feat]] <- hmmTMB_dist
  }

  # If no initial parameters provided, use GMM initialisation
  if (is.null(initial_params)) {

    gmm_init <- tryCatch({
      .estimate_initial_params_gmm(
        feature_matrix = data[, features, drop = FALSE],
        n_states = n_states,
        feature_distributions = feature_distributions
      )
    }, error = function(e) NULL)

    if (!is.null(gmm_init)) {
      # Convert GMM params to hmmTMB format
      initial_params <- .convert_params_to_hmmTMB(
        gmm_init$params,
        features,
        feature_distributions
      )
    }
  }

  # If GMM initialisation fails, create simple default parameters based on
  # data qualities
  if (is.null(initial_params)) {

    initial_params <- list()

    for (feat in features) {
      feat_vals <- data[[feat]][!is.na(data[[feat]])]
      dist_type <- feature_distributions[[feat]]$distribution

      # Use quantiles to separate states
      quantiles <- quantile(
        feat_vals,
        probs = seq(0, 1, length.out = n_states + 1),
        na.rm = TRUE
      )
      state_means <- sapply(1:n_states, function(s) {
        mean(
          feat_vals[feat_vals >= quantiles[s] & feat_vals <= quantiles[s + 1]],
          na.rm = TRUE
        )
      })
      # Handle any NAs in state_means
      state_means[is.na(state_means)] <- mean(feat_vals, na.rm = TRUE)

      overall_sd <- sd(feat_vals, na.rm = TRUE)

      # Create parameters based on distribution type
      if (dist_type == "norm") {
        initial_params[[feat]] <- list(
          mean = state_means,
          sd = rep(overall_sd / sqrt(n_states), n_states)
        )
      } else if (dist_type == "gamma") {
        # Ensure positive values for gamma
        state_means <- pmax(state_means, 0.01)
        shape_est <- (state_means / overall_sd)^2
        scale_est <- overall_sd^2 / state_means
        initial_params[[feat]] <- list(
          shape = pmax(shape_est, 0.1),
          scale = pmax(scale_est, 0.01)
        )
      } else if (dist_type == "lnorm") {
        # Log-transform for lognormal
        log_vals <- log(pmax(feat_vals, 0.001))
        log_means <- log(pmax(state_means, 0.001))
        log_sd <- sd(log_vals, na.rm = TRUE)
        initial_params[[feat]] <- list(
          meanlog = log_means,
          sdlog = rep(log_sd / sqrt(n_states), n_states)
        )
      } else if (dist_type == "vm") {
        # von Mises - use circular mean for each quantile group
        state_mus <- sapply(1:n_states, function(s) {
          group_vals <- feat_vals[feat_vals >= quantiles[s] &
                                    feat_vals <= quantiles[s + 1]]
          if (length(group_vals) > 0) {
            atan2(mean(sin(group_vals)), mean(cos(group_vals)))
          } else {
            0
          }
        })
        initial_params[[feat]] <- list(
          mu = state_mus,
          kappa = rep(2, n_states)  # moderate concentration
        )
      } else if (dist_type == "beta") {
        # Beta - use method of moments
        state_means <- pmax(pmin(state_means, 0.99), 0.01)
        var_est <- overall_sd^2 / n_states
        var_est <- pmin(var_est, state_means * (1 - state_means) * 0.9)
        common_factor <- state_means * (1 - state_means) / var_est - 1
        common_factor <- pmax(common_factor, 1)
        initial_params[[feat]] <- list(
          shape1 = state_means * common_factor,
          shape2 = (1 - state_means) * common_factor
        )
      } else {
        # Fallback to normal
        initial_params[[feat]] <- list(
          mean = state_means,
          sd = rep(overall_sd / sqrt(n_states), n_states)
        )
      }
    }
  }

  # Create hmmTMB model components

  # If an ID variable is provided, duplicate the column and call it `ID` for
  # integration with hmmTMB
  if (!is.null(id_var) && id_var %in% names(data)) {
    data$ID <- data[[id_var]]
  }

  # Fit HMM with error handling
  fitted_model <- tryCatch({

    # Observation model
    obs <- hmmTMB::Observation$new(
      data = data,
      dists = dists,
      par = initial_params,
      n_states = n_states
    )

    # Hidden state model
    hid <- hmmTMB::MarkovChain$new(
      n_states = n_states,
      data = data
    )

    # Create HMM
    hmm <- hmmTMB::HMM$new(
      obs = obs,
      hid = hid
    )

    # Try default optimiser
    fit_success <- FALSE
    tryCatch({
      hmm$fit(silent = TRUE)
      llk_check <- hmm$llk()
      if (is.finite(llk_check)) {
        fit_success <- TRUE
      }
    }, error = function(e) NULL)

    # If default failed, try BFGS optimizer as fallback
    if (!fit_success) {

      # Recreate model objects for fresh start
      obs2 <- hmmTMB::Observation$new(
        data = data,
        dists = dists,
        par = initial_params,
        n_states = n_states
      )
      hid2 <- hmmTMB::MarkovChain$new(
        data = data,
        n_states = n_states
      )
      hmm <- hmmTMB::HMM$new(obs = obs2, hid = hid2)

      hmm$fit(silent = TRUE, method = "BFGS")
      llk_check <- hmm$llk()
      if (!is.finite(llk_check)) {
        stop("Model fitted but log-likelihood is not finite (NaN or Inf). ",
             "This often indicates numerical issues with extreme values or ",
             "poor initial parameter estimates.")
      }
    }

    hmm
  }, error = function(e) {
    # Provide informative error message
    message("HMM fitting failed for this dataset.")
    message("  Error: ", e$message)
    message("  Possible causes:")
    message("    - Insufficient data (need enough observations per state)")
    message("    - Extreme outliers in features (check data quality)")
    message("    - Features with zero variance")
    message("    - Incompatible distribution for data type")
    NULL
  })

  return(fitted_model)
}


#' INTERNAL Extract HMM state assignments and probabilities
#'
#' Decodes hidden states from a fitted HMM using Viterbi algorithm and extracts
#' state probabilities. Maps results back to original dataset, handling rows
#' with missing feature values.
#'
#' @param fitted_model Fitted hmmTMB model object
#' @param n_obs Integer. Total number of observations in original dataset
#' @param complete_idx Logical vector indicating which rows had complete feature
#'   data and were used in model fitting
#'
#' @return List with two elements:
#'   \itemize{
#'     \item \code{states}: Integer vector of length \code{n_obs} with state
#'       assignments (NA for rows not used in fitting)
#'     \item \code{state_probs}: Numeric vector of length \code{n_obs} with
#'       probabilities of assigned states (NA for rows not used in fitting)
#'   }
#'
#' @details
#' Uses the Viterbi algorithm (\code{fitted_model$viterbi()}) to find the most
#' likely sequence of hidden states. Extracts the probability of each assigned
#' state from the state probability matrix. Returns NA for all observations if
#' Viterbi decoding fails.
#'
#' @keywords internal

.extract_hmm_states <- function(
  fitted_model,
  n_obs,
  complete_idx
) {

  # Get Viterbi decoded states
  viterbi_states <- tryCatch({
    fitted_model$viterbi()
  }, error = function(e) NULL)

  # Get state probablities
  state_probs_matrix <- tryCatch({
    fitted_model$state_probs()
  }, error = function(e) NULL)

  if (is.null(viterbi_states)) {
    return(list(
      states = rep(NA_integer_, n_obs),
      state_probs = rep(NA_real_, n_obs)
    ))
  }

  # Get probability of assigned state
  if (!is.null(state_probs_matrix)) {
    state_probs <- sapply(seq_along(viterbi_states), function(i) {
      state_probs_matrix[i, viterbi_states[i]]
    })
  } else {
    state_probs <- rep(NA_real_, length(viterbi_states))
  }

  # Map back to full dataset
  full_states <- rep(NA_integer_, n_obs)
  full_probs <- rep(NA_real_, n_obs)

  full_states[complete_idx] <- viterbi_states
  full_probs[complete_idx] <- state_probs

  return(list( # nolint: return_linter.
    states = full_states,
    state_probs = full_probs
  ))

}

#' INTERNAL Estimate initial HMM parameters using Gaussian Mixture Model
#'
#' Uses GMM clustering (via mclust) to initialize HMM parameters. Normalizes
#' features appropriately (z-score for continuous, x-y coordinates for angular)
#' to ensure all features contribute equally to clustering. Then estimates
#' distribution-specific parameters for each state based on cluster assignments.
#'
#' @param feature_matrix Matrix or data frame containing feature values (rows =
#'   observations, columns = features)
#' @param n_states Integer. Number of states/clusters
#' @param feature_distributions Named list of distribution info from
#'   \code{.auto_select_distribution}
#'
#' @return List containing:
#'   \itemize{
#'     \item \code{initial_states}: Integer vector of cluster assignments
#'     \item \code{params}: Nested list of parameters. Structure is
#'       \code{params[[feature]][[state]][[param_name]]}
#'     \item \code{gmm_model}: The fitted mclust model object
#'     \item \code{complete_idx}: Logical vector of complete cases
#'   }
#'
#' @details
#' Normalization strategy:
#' \itemize{
#'   \item \strong{Angular features} (von Mises): Converted to x = cos(angle),
#'     y = sin(angle). Automatically scaled to (-1, 1)
#'   \item \strong{Non-angular features}: Z-score normalized to ensure equal
#'     contribution regardless of variance
#'   \item \strong{Constant features}: Standard deviation set to 1 to avoid
#'     division by zero
#' }
#'
#' After clustering, estimates distribution-specific parameters for each state
#' using \code{.estimate_dist_params}. Stops with error if GMM fitting fails
#' (e.g., insufficient data for requested number of states).
#'
#' @importFrom mclust Mclust
#' @importFrom stats complete.cases
#'
#' @keywords internal

.estimate_initial_params_gmm <- function(
  feature_matrix,
  n_states,
  feature_distributions
) {

  # Format feature matrix
  feature_matrix <- as.matrix(feature_matrix)
  feature_names <- colnames(feature_matrix)

  # Ensure feature names are set
  if (is.null(feature_names)) {
    feature_names <- paste0("feature_", seq_len(ncol(feature_matrix)))
    colnames(feature_matrix) <- feature_names
  }

  # Identify complete cases
  complete_idx <- complete.cases(feature_matrix)
  n_complete <- sum(complete_idx)

  # Warn if very few complete observations relative to states
  if (n_complete < n_states * 10) {
    warning("Very few complete observations (", n_complete,
            ") relative to number of states (", n_states,
            "). HMM fitting may be unreliable.")
  }

  # Select only complete cases for GMM
  data_complete <- feature_matrix[complete_idx, , drop = FALSE]

  # For GMM clustering, we need to:
  # 1. Normalize non-angular features (so all contribute equally)
  # 2. Transform angular data to x,y coordinates (inherently scaled to [-1,1])
  # This ensures GMM isn't dominated by features with larger variance

  gmm_data <- matrix(nrow = nrow(data_complete), ncol = 0)

  for (i in seq_along(feature_names)) {
    feat <- feature_names[i]
    feat_values <- data_complete[, i]

    if (feature_distributions[[feat]]$distribution == "vm") {
      # Angular data: convert to x,y coordinates (automatically in [-1,1])
      gmm_data <- cbind(gmm_data,
                        cos(feat_values),
                        sin(feat_values))
    } else {
      # Non-angular data: z-score normalize for equal contribution
      feat_mean <- mean(feat_values, na.rm = TRUE)
      feat_sd <- sd(feat_values, na.rm = TRUE)

      # Handle constant features (sd = 0)
      if (is.na(feat_sd) || feat_sd < .Machine$double.eps) {
        feat_sd <- 1
      }

      gmm_data <- cbind(gmm_data, (feat_values - feat_mean) / feat_sd)
    }
  }

  gmm_fit <- mclust::Mclust(gmm_data, G = n_states, verbose = FALSE)

  if (is.null(gmm_fit)) {
    stop("GMM initialisation failed. Consider reducing n_states.")
  }

  initial_states <- gmm_fit$classification

  # Estimate per-state parameters for each feature
  params <- list()

  for (feat in feature_names) {
    dist_type <- feature_distributions[[feat]]$distribution
    params[[feat]] <- list()

    for (s in 1:n_states) {
      state_values <- data_complete[initial_states == s, feat]
      state_values <- state_values[!is.na(state_values)]
      params[[feat]][[s]] <- .estimate_dist_params(state_values, dist_type)
    }
  }

  return(list( # nolint: return_linter.
    initial_states = initial_states,
    params = params,
    gmm_model = gmm_fit,
    complete_idx = complete_idx
  ))
}


#' INTERNAL: Estimate distribution-specific parameters from data
#'
#' Calculates distribution parameters from a numeric vector based on the
#' specified distribution type. Handles gamma, lognormal, von Mises, and beta
#' distributions with appropriate fallback values when data is insufficient
#' or invalid. Uses stats functions for basic calculations and atan2 for
#' circular mean direction.
#'
#' @param values Numeric vector of observed values from which to estimate
#'   parameters. NA values are automatically removed.
#' @param dist_type Character string specifying distribution: "gamma", "lnorm",
#'   "vm" (von Mises), "beta", or others (defaults to normal).
#'
#' @return Named list containing distribution-specific parameters:
#'   \itemize{
#'     \item gamma: \code{shape}, \code{scale} (hmmTMB parameterization)
#'     \item lnorm: \code{meanlog}, \code{sdlog}
#'     \item vm: \code{mu}, \code{kappa}
#'     \item beta: \code{shape1}, \code{shape2}
#'     \item default: \code{mean}, \code{sd}
#'   }
#'   Returns default values (shape=1, scale=1, etc.) if data is insufficient
#'   (<2 values) or invalid (zero variance).
#'
#' @keywords internal
#' @importFrom stats sd var

.estimate_dist_params <- function(values, dist_type) {

  values <- values[!is.na(values)]

  if (length(values) < 2) {
    return(switch(
      dist_type,
      "gamma" = list(shape = 1, scale = 1),
      "lnorm" = list(meanlog = 0, sdlog = 1),
      "vm" = list(mu = 0, kappa = 1),
      "beta" = list(shape1 = 1, shape2 = 1),
      list(mean = 0, sd = 1)
    ))
  }

  switch(dist_type,
    "gamma" = {
      m <- mean(values[values > 0])
      v <- var(values[values > 0])
      if (is.na(v) || v <= 0 || is.na(m) || m <= 0) {
        list(shape = 1, scale = 1)
      } else {
        # hmmTMB uses shape/scale parameterization (not shape/rate)
        # shape = mean^2 / var, scale = var / mean
        list(shape = m^2 / v, scale = v / m)
      }
    },
    "lnorm" = {
      log_vals <- log(values[values > 0])
      if (length(log_vals) < 2) {
        list(meanlog = 0, sdlog = 1)
      } else {
        list(meanlog = mean(log_vals), sdlog = sd(log_vals))
      }
    },
    "vm" = {
      mean_dir <- atan2(mean(sin(values)), mean(cos(values)))
      r <- sqrt(mean(cos(values))^2 + mean(sin(values))^2)
      kappa <- ifelse(r < 0.53, 2 * r + r^3 + 5 * r^5 / 6,
                      ifelse(r < 0.85, -0.4 + 1.39 * r + 0.43 / (1 - r),
                             1 / (r^3 - 4 * r^2 + 3 * r)))
      # Cap kappa to prevent numerical overflow
      list(mu = mean_dir, kappa = min(50, max(0.1, kappa)))
    },
    "beta" = {
      m <- mean(values)
      v <- var(values)
      if (v >= m * (1 - m)) {
        list(shape1 = 1, shape2 = 1)
      } else {
        common <- m * (1 - m) / v - 1
        list(shape1 = m * common, shape2 = (1 - m) * common)
      }
    },
    # Default: Gaussian
    list(mean = mean(values), sd = sd(values))
  )
}


#' INTERNAL Convert GMM parameter format to hmmTMB format
#'
#' Reorganizes parameter structure from GMM output format (parameters nested by
#' state then by parameter name) to hmmTMB input format (parameters nested by
#' feature then by parameter name, with values as vectors across states).
#'
#' @param gmm_params Nested list from \code{.estimate_initial_params_gmm}.
#'   Structure: \code{gmm_params[[feature]][[state]][[param_name]]}
#' @param features Character vector of feature names
#' @param feature_distributions Named list of distribution info (not currently
#'   used but included for potential future extensions)
#'
#' @return Nested list in hmmTMB format.
#'   Structure: \code{result[[feature]][[param_name]][state]}
#'
#' @details
#' For example, transforms:
#' \preformatted{
#'   gmm_params$speed$state1$mean = 2.5
#'   gmm_params$speed$state2$mean = 5.0
#' }
#' Into:
#' \preformatted{
#'   result$speed$mean = c(2.5, 5.0)
#' }
#'
#' This format is required by hmmTMB's Observation$new() constructor.
#'
#' @keywords internal

.convert_params_to_hmmTMB <- function( # nolint: object_name_linter.
  gmm_params,
  features,
  feature_distributions
) {
  hmmTMB_params <- list() # nolint: object_name_linter.

  for (feat in features) {
    state_params <- gmm_params[[feat]]  # List of params per state
    n_states <- length(state_params)

    # Get parameter names for this distribution
    param_names <- names(state_params[[1]])

    # Reorganize: from params[[state]][[param]] to params[[param]][states]
    feat_params <- list()
    for (param_name in param_names) {
      feat_params[[param_name]] <- sapply(1:n_states, function(s) {
        state_params[[s]][[param_name]]
      })
    }

    hmmTMB_params[[feat]] <- feat_params # nolint: object_name_linter.
  }

  return(hmmTMB_params) # nolint: return_linter.
}