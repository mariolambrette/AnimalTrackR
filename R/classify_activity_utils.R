# Helper functions for the classify activity pipeline. Some of these functions
# are also exported as user facing functions.


#' @title Calculate movement features for activity classification
#'
#' @description
#' This function calculates movement features from AnimalTrackR detections that
#' can be used for behavioural classification. Users can specify the features
#' they would like and a dataframe with the calculated features is returned.
#'
#' @param dets Dataframe of AnimalTrackR detections with at least the following
#' columns: \itemize{
#'  \item \code{vid_id}
#'  \item \code{Frame}
#'  \item \code{xc}
#'  \item \code{yc}
#' }
#' If detections are present for multiple individuals, an additional column
#' specifying individual ID can be provided via the \code{indiv_id} parameter.
#' Any other columns in the dataframe will be ignored and retained in the
#' output.
#'
#' @param features Character vector of movement features to use for
#' classification. Options include: \itemize{
#'  \item \code{"speed"} - Instantaneous speed (pixels per frame)
#'  \item \code{"acceleration"} - Change in speed between frames
#'  \item \code{"turning"} - Absolute turning angle (radians)
#'  \item \code{"heading"} - Instantaneous movement direction (radians)
#'  \item \code{"speed_var"} - Variance in speed over rolling window
#'  \item \code{"speed_smooth"} - Speed smoothed over rolling window
#'  \item \code{"meander"} - Path tortuosity measure
#'  \item \code{"path_straightness"} - Net displacement to path length ratio
#'  \item \code{"paused"} - Binary indicator of paused state
#'  \item \code{"time_since_pause"} - Frames since last pause
#'  \item \code{"spatial_spread"} - Space use variance in rolling window
#'  \item \code{"edge_preference"} - Proximity to arena edges
#'  \item \code{"roaming_entropy"} - Spatial entropy based on grid occupancy
#' }
#' Default features are \code{c("speed", "acceleration", "turning")}. A more
#' detailed description of each feature can be found in
#' \code{vignette("classify-activity")}.
#'
#' @param window_size Integer. Rolling window size for smoothed features. The
#' window will be \code{window_size} rows wide in the \code{dets} dataframe, so
#' users should consider the framerate of their data when selecting this
#' parameter. Default is 5.
#'
#' @param pause_threshold Numeric. Speed threshold (in pixels per frame) below
#' which the animal is considered to be paused/stationary. Required if using
#' pause-based features (\code{"paused"} or \code{"time_since_pause"}).
#' Default is \code{NULL}.
#'
#' @return A dataframe identical to \code{dets} with additional columns for
#' each requested feature.
#'
#' @examples
#' \dontrun{
#' # Load example detection data
#' dets <- read.csv("path/to/detections.csv")
#'
#' # Calculate speed, acceleration and turning features with a rolling window
#' # of 5 frames
#' feature_dets <- calculate_movement_features(
#'  dets,
#'  features = c("speed", "acceleration", "turning"),
#'  window_size = 5
#' )
#'
#' # Calculate features including pause-based features
#' feature_dets <- calculate_movement_features(
#'  dets,
#'  features = c("speed", "acceleration", "paused", "time_since_pause"),
#'  window_size = 5,
#'  pause_threshold = 0.1
#' )
#' }
#'
#' @seealso \code{\link{classify_activity}} for end-to-end activity
#' classification
#'
#' @export
#'
#' @import magrittr
#' @import dplyr
#' @importFrom data.table frollapply frollmean frollsum
#' @importFrom rlang .data
#' @importFrom stats quantile

calculate_movement_features <- function(
  dets,
  features,
  window_size,
  pause_threshold = NULL
) {

  # Group detections by video ID
  dets <- dets %>%
    dplyr::arrange(.data$vid_id, .data$Frame) %>%
    dplyr::group_by(.data$vid_id)

  ## SPEED (ALWAYS CALCULATED) -------------------------------------------------

  # Speed is always calcualted as it is a fundamental metric. It is calculated
  # as movement pixels per frame

  dets <- dets %>%
    dplyr::mutate(

      # Displacement between frames
      dx = .data$xc - dplyr::lag(.data$xc),
      dy = .data$yc - dplyr::lag(.data$yc),
      dt = .data$Frame - dplyr::lag(.data$Frame),

      # Speed
      speed = sqrt(.data$dx^2 + .data$dy^2) / .data$dt
    )

  ## ACCELERATION --------------------------------------------------------------

  if ("acceleration" %in% features) {

    dets <- dets %>%
      dplyr::mutate(
        acceleration = (.data$speed - dplyr::lag(.data$speed)) / .data$dt
      )

  }

  ## TURNING ANGLE -------------------------------------------------------------

  if ("turning" %in% features) {

    dets <- dets %>%
      dplyr::mutate(
        turning = abs(atan2(.data$dy, .data$dx) -
                        dplyr::lag(atan2(.data$dy, .data$dx))),
        # Correct for angle wrapping
        turning = ifelse(
          .data$turning > pi, 2 * pi - .data$turning, .data$turning
        )
      )

  }

  ## HEADING ------------------------------------------------------------------

  heading_features <- c("heading", "meander", "path_straightness", "heading_smooth")

  if (any(heading_features %in% features)) {

    dets <- dets %>%
      dplyr::mutate(
        heading = atan2(.data$dy, .data$dx)
      )

    if ("heading_smooth" %in% features) {

      dets <- dets %>%
        dplyr::mutate(
          heading_smooth = data.table::frollmean(
            .data$heading,
            n = window_size,
            align = "center"
          )
        )

  }

  ## ROLLING SPPED FEATURES ----------------------------------------------------

  if ("speed_var" %in% features) {

    dets <- dets %>%
      dplyr::mutate(
        speed_var = data.table::frollapply(
          .data$speed,
          n = window_size,
          FUN = var,
          align = "center"
        )
      )

  }

  if ("speed_smooth" %in% features) {

    dets <- dets %>%
      dplyr::mutate(
        speed_smooth = data.table::frollmean(
          .data$speed,
          n = window_size,
          align = "center"
        )
      )

  }

  ## DIRECTIONAL PERSISTENCE ---------------------------------------------------

  # Meander: rolling mean of absolute heading change
  if ("meander" %in% features) {

    dets <- dets %>%
      dplyr::mutate(
        heading_change = {
          h_diff <- .data$heading - dplyr::lag(.data$heading)

          # Correct for angle wrapping
          h_diff <- ifelse(h_diff > pi, h_diff - 2 * pi, h_diff)
          h_diff <- ifelse(h_diff < -pi, h_diff + 2 * pi, h_diff)

          abs(h_diff)
        },

        meander = data.table::frollmean(
          .data$heading_change,
          n = window_size,
          align = "center"
        )
      )

  }

  # Path straightness: ratio of net displacement to total path length over
  # rolling window

  if ("path_straightness" %in% features) {

    dets <- dets %>%
      dplyr::mutate(
        path_straightness = {
          # Rolling sum of distance travelled
          path_length <- data.table::frollsum(
            sqrt(.data$dx^2 + .data$dy^2),
            n = window_size,
            align = "center"
          )

          # Net displacement over window
          x_start <- data.table::frollapply(
            .data$xc,
            n = window_size,
            FUN = function(x) x[1],
            align = "center"
          )

          y_start <- data.table::frollapply(
            .data$yc,
            n = window_size,
            FUN = function(y) y[1],
            align = "center"
          )

          x_end <- data.table::frollapply(
            .data$xc,
            n = window_size,
            FUN = function(x) x[length(x)],
            align = "center"
          )

          y_end <- data.table::frollapply(
            .data$yc,
            n = window_size,
            FUN = function(y) y[length(y)],
            align = "center"
          )

          net_displacement <- sqrt((x_end - x_start)^2 + (y_end - y_start)^2)

          # Straghtness = net displacement / path length
          ifelse(
            path_length > 0,
            net_displacement / path_length,
            NA_real_
          )
        }
      )

  }

  ## PAUSE DETECTION -----------------------------------------------------------

  if (any(c("paused", "time_since_pause") %in% features)) {

    dets <- dets %>%
      dplyr::mutate(
        # Identify paused frames
        paused = .data$speed < pause_threshold | is.na(.data$speed)
      )

    # Time since last paused
    if ("time_since_pause" %in% features) {

      dets <- dets %>%
        dplyr::mutate(
          time_since_pause = {
            pause_positions <- which(.data$paused)

            if (length(pause_positions) == 0) {
              # Never paused
              rep(NA_integer_, n())
            } else {
              # For each frame find frames since last pause
              sapply(seq_len(n()), function(i) {
                prev_pauses <- pause_positions[pause_positions < i]
                if (length(prev_pauses) == 0) {
                  NA_integer_
                } else {
                  i - max(prev_pauses)
                }
              })
            }
          }
        )

    }

  }

  ## SPACE USE -----------------------------------------------------------------

  if ("spatial_spread" %in% features) {
    dets <- dets %>%
      dplyr::mutate(
        spatial_spread = {

          # Rolling SD of x and y positions
          sd_x <- data.table::frollapply(
            .data$xc,
            n = window_size,
            FUN = sd,
            align = "center"
          )
          sd_y <- data.table::frollapply(
            .data$yc,
            n = window_size,
            FUN = sd,
            align = "center"
          )

          # Euclidean distance of SDs as spatial spread metric
          sqrt(sd_x^2 + sd_y^2)
        }
      )
  }

  if ("edge_preference" %in% features) {
    dets <- dets %>%
      dplyr::mutate(
        edge_preference = {
          # Calculate distance from centroid of all positions in video
          centroid_x <- mean(.data$xc, na.rm = TRUE)
          centroid_y <- mean(.data$yc, na.rm = TRUE)
          dist_from_centroid <- sqrt(
            (.data$xc - centroid_x)^2 + (.data$yc - centroid_y)^2
          )

          # Normalise by 95th percentile of distances (robust to outliers)
          max_dist <- quantile(dist_from_centroid, 0.95, na.rm = TRUE)

          if (max_dist > 0) {
            dist_from_centroid / max_dist
          } else {
            rep(0, n())
          }
        }
      )
  }

  # Roaming entropy: spatial entropy based on grid occupancy
  # High entropy = Animal explores many different locations
  # Low entropy = Animal stays in few locations
  if ("roaming_entropy" %in% features) {
    dets <- dets %>%
      dplyr::mutate(
        roaming_entropy = {
          # Define grid
          n_bins <- 10
          x_bins <- cut(.data$xc, breaks = n_bins, labels = FALSE)
          y_bins <- cut(.data$yc, breaks = n_bins, labels = FALSE)

          # Calculate rolling entropy over window
          data.table::frollapply(
            seq_len(n()),
            n = window_size * 4,  # Larger window for spatial metric
            FUN = function(indicies) {
              # Get grid cells visited in this window
              cells <- paste(x_bins[indicies], y_bins[indicies], sep = "_")
              cells <- cells[!is.na(cells)]

              if (length(cells) == 0) {
                return(NA_real_)
              }

              # Calcualte Shannon entropy
              cell_probs <- table(cells) / length(cells)
              -sum(cell_probs * log(cell_probs + 1e-10)) # avoid log(0)

            },
            align = "center"
          )
        }
      )
  }

  # Ungroup data
  dets <- dets %>%
    dplyr::ungroup()

  return(dets)

}


#' INTERNAL: Create composite activity score from multiple features
#'
#' Combines multiple normalized movement features into a single composite
#' activity score using mean, PCA, or weighted mean methods. For PCA method,
#' analysis is conducted separately per individual and loadings are stored
#' as attributes.
#'
#' @param dets Dataframe containing normalized feature columns
#' @param feature_cols Character vector of normalized feature column names
#'   (e.g., "norm_speed", "norm_acceleration")
#' @param method Character. Method for combining features: "mean" (simple
#'   average), "pca" (first principal component), or "weighted" (weighted mean)
#' @param feature_weights Named numeric vector of feature weights for
#'   "weighted" method. Must match length and names of feature_cols (with
#'   "norm_" prefix). Weights are normalized to sum to 1 internally
#' @param indiv_id Character. Optional column name for individual ID. When
#'   provided with method = "pca", PCA is run separately per individual
#'
#' @return Dataframe with added \code{activity_score} column. For PCA method,
#'   includes attribute "pca_loadings_by_fish" with loadings for each
#'   individual. For weighted method, includes attribute "feature_weights"
#'   with normalized weights
#'
#' @keywords internal
#' @importFrom stats complete.cases prcomp cor

.create_composite_score <- function(
  dets,
  feature_cols,
  method = "mean",
  feature_weights = NULL,
  indiv_id = NULL
) {
  if (method == "mean") {
    # A simple mean of selected normalised features

    # Create a matrix of feature scores - results in a matirx with columns for
    # each feature and rows for each detection
    feature_matrix <- dets %>%
      dplyr::select(dplyr::all_of(feature_cols)) %>%
      as.matrix()

    # Calculate simple mean of all feature scores in each row and add as new
    # column
    dets$activity_score <- rowMeans(feature_matrix, na.rm = TRUE)

  } else if (method == "pca") {

    # Principal component analysis is run on the movement metrics for each
    # individual. The first principal component (PC1) is extracted as the
    # composite activity socre and is made up of a weighted combination of
    # the specified features. Selected weights and features may vary between
    # individuals.

    # Create data placeholders
    dets$activity_score <- NA_real_
    pca_loadings_list <- list()

    # Add placeholder column if no individual ID provided
    if (is.null(indiv_id)) {
      dets$indiv_id_temp <- "all"
      indiv_id <- "indiv_id_temp"
    }

    # Loop over individuals to run PCA separately
    for (indiv in unique(dets[[indiv_id]])) {

      # Row labels for individual
      idx <- dets[[indiv_id]] == indiv

      # Subset feature matrix for individual
      feature_matrix <- dets %>%
        dplyr::filter(.data[[indiv_id]] == indiv) %>%
        dplyr::select(dplyr::all_of(feature_cols)) %>%
        as.matrix()

      # Check which rows of the matrix are complete
      complete_idx <- complete.cases(feature_matrix)

      # Skip with warning if not enough complete cases
      if (sum(complete_idx) < length(feature_cols)) {
        warning(paste(
          "Not enough complete cases to run PCA for individual",
          indiv, "- skipping"
        ))
        next
      }

      # Run PCA on the individual's data
      pca_fit <- prcomp(
        feature_matrix[complete_idx, ],
        center = TRUE,
        scale. = FALSE
      )

      # Extract PC1 scores
      pc1_scores <- rep(NA_real_, nrow(feature_matrix))
      pc1_scores[complete_idx] <- pca_fit$x[, 1]

      # Ensure that higher PC1 values mean a higher activity level (the sign of
      # PC1 is arbitrary so it can be flipped if needed)
      speed_cor <- cor(pc1_scores,
                       dets$norm_speed[idx],
                       use = "complete.obs")
      if (speed_cor < 0) {
        pc1_scores <- -pc1_scores
      }

      # Store PC1 values into the main dataframe
      dets$activity_score[idx] <- pc1_scores

      # Store PCA loadings for this individual
      pca_loadings_list[[as.character(indiv)]] <- pca_fit$rotation[, 1]
    }

    attr(dets, "pca_loadings_by_fish") <- pca_loadings_list

  } else if (method == "weighted") {
    # A weighted mean of selected normalised features. Relies on the `weights`
    # parameter being set.

    # Create matrix of features
    feature_matrix <- dets %>%
      dplyr::select(dplyr::all_of(feature_cols)) %>%
      as.matrix()

    # Check weights provided
    if (is.null(feature_weights)) {
      stop(
        "Feature weights must be provided when composite_method = 'weighted'"
      )
    }

    # Check weights length matches number of features
    if (length(feature_weights) != length(feature_cols)) {
      stop("Length of feature_weights must match number of features")
    }

    # Check names of weights match feature names
    # Add norm_ prefix to weight names for matching
    names(feature_weights) <- paste0("norm_", names(feature_weights))
    if (!all(names(feature_weights) %in% feature_cols)) {
      stop("Names of feature_weights must match feature names")
    }

    # Normalise weights to sum to 1
    if (sum(feature_weights) != 1) {
      feature_weights <- feature_weights / sum(feature_weights)
    }

    # Calculate weighted mean of features for each detection
    dets$activity_score <- as.vector(
      feature_matrix %*% feature_weights[feature_cols]
    )

    attr(dets, "feature_weights") <- feature_weights
  }

  return(dets)

}

#' INTERNAL: Classify activity states using Gaussian Mixture Model
#'
#' Fits a Gaussian Mixture Model to normalized movement features to identify
#' distinct behavioral states. Uses mclust package for model fitting and
#' returns state assignments with associated probabilities.
#'
#' @param dets Dataframe containing detection data with normalized features
#' @param feature_cols Character vector of normalized feature column names
#'   to use for GMM clustering
#' @param n_states Integer. Number of states (mixture components) to fit
#'
#' @return Dataframe with added columns:
#'   \itemize{
#'     \item \code{state}: Integer state assignment (1 to n_states)
#'     \item \code{state_prob}: Probability of assigned state (maximum
#'       posterior probability)
#'   }
#'   GMM model object is stored as attribute "gmm_model"
#'
#' @keywords internal
#' @import mclust
#' @importFrom stats complete.cases

.classify_gmm <- function(dets, feature_cols, n_states) {

  # Fit a Gaussian Mixture Model to selected features (not the composite score).
  # Fits to the normalised features to control for variation between individuals

  # Create matrix of specified features
  feature_matrix <- dets %>%
    dplyr::select(dplyr::all_of(feature_cols)) %>%
    as.matrix()

  # Complete row indices
  complete_idx <- complete.cases(feature_matrix)

  # Fit GMM
  gmm_fit <- mclust::Mclust(
    feature_matrix[complete_idx, ],
    G = n_states
  )

  # Extract state classifications
  dets$state <- NA_integer_
  dets$state[complete_idx] <- gmm_fit$classification

  # Add state probabilities
  dets$state_prob <- NA_real_
  dets$state_prob[complete_idx] <- apply(gmm_fit$z, 1, max)

  attr(dets, "gmm_model") <- gmm_fit

  return(dets) # nolint: return_linter.
}

#' INTERNAL: Classify activity states using Otsu thresholding
#'
#' Applies Otsu's method to automatically determine an optimal threshold for
#' the composite activity score, separating data into two states (active vs
#' inactive). Verifies that higher state numbers correspond to higher activity
#' levels using normalized speed.
#'
#' @param dets Dataframe containing \code{activity_score} and
#'   \code{norm_speed} columns. The \code{dets} parameter is implicitly
#'   used from the parent scope
#'
#' @return Dataframe with added columns:
#'   \itemize{
#'     \item \code{state}: Integer state assignment (1 = inactive, 2 = active)
#'     \item \code{state_prob}: Set to NA (threshold method provides no
#'       probabilities)
#'   }
#'   Optimal threshold value is stored as attribute "threshold"
#'
#' @keywords internal
#' @importFrom graphics hist

.classify_threshold <- function(dets) {

  values <- dets$activity_score

  # Early exit if all values are identical (or nearly so)
  if (diff(range(values, na.rm = TRUE)) < .Machine$double.eps) {
    warning("Activity scores show no variation - cannot classify")
    dets$state <- 1L
    dets$state_prob <- 1.0
    return(dets)
  }

  threshold <- .otsu_threshold(values[!is.na(values)])

  dets$state <- ifelse(dets$activity_score > threshold, 2L, 1L)
  dets$state_prob <- NA_real_

  # Verify that state 2 corresponds to higher activity
  state_speeds <- tapply(dets$norm_speed, dets$state, mean, na.rm = TRUE)

  if (state_speeds["1"] > state_speeds["2"]) {
    dets$state <- ifelse(dets$state == 1L, 2L, 1L)
  }

  attr(dets, "threshold") <- threshold

  return(dets) # nolint: return_linter.
}

#' INTERNAL: Classify activity states using changepoint detection
#'
#' Identifies behavioral state transitions by detecting changepoints in the
#' composite activity score using PELT algorithm. Can detect changes in mean,
#' variance, or both. States are assigned based on whether segment means are
#' above or below the global median activity score.
#'
#' @param dets Dataframe containing \code{activity_score}, \code{vid_id} columns
#' @param method Character. Changepoint detection method: "mean" (changes in
#'   mean), "var" (changes in variance), or "meanvar" (changes in both mean
#'   and variance). Default is "mean"
#'
#' @return Dataframe with added columns:
#'   \itemize{
#'     \item \code{state}: Integer state assignment based on segment means
#'       relative to global median (1 = below median, 2 = above median)
#'     \item \code{state_prob}: Set to NA (changepoint method provides no
#'       probabilities)
#'   }
#'   Analysis is conducted separately for each video ID
#'
#' @keywords internal
#' @importFrom changepoint cpt.mean cpt.var cpt.meanvar param.est cpts

.classify_changepoint <- function(dets, method = "mean") {
  # Classify behaviour based on changepoint detection in the composite activity
  # score. This method identifies points where the statistical properties of
  # the activity score change significantly, indicating a shift between active
  # and inactive states.

  # Calculate global median of activity score
  global_median <- median(dets$activity_score, na.rm = TRUE)

  # Process each video using dplyr group_by
  dets <- dets %>%
    group_by(.data$vid_id) %>%
    mutate(
      state = {

        # Activity score values
        values <- .data$activity_score

        # Replace NAs with the video median
        values[is.na(values)] <- median(values, na.rm = TRUE)

        if (method == "mean") {
          # Detect changepoints in mean
          cpt <- changepoint::cpt.mean(
            values,
            method = "PELT",
            penalty = "BIC"
          )
        } else if (method == "meanvar") {
          # Detect changepoints in mean and variance
          cpt <- changepoint::cpt.meanvar(
            values,
            method = "PELT",
            penalty = "BIC"
          )
        } else if (method == "var") {
          # Detect changepoints in variance
          cpt <- changepoint::cpt.var(
            values,
            method = "PELT",
            penalty = "BIC"
          )
        } else {
          stop("Invalid method for changepoint detection. Must be 'mean', 'var'
                or 'meanvar'.")
        }

        # Get segment means and changepoint locations
        seg_means <- changepoint::param.est(cpt)$mean
        cpt_locs <- c(0, changepoint::cpts(cpt), n())

        # Assign states based on global median
        seg_states <- ifelse(seg_means > global_median, 2L, 1L)

        # Vectorized state assignment using cut()
        # cut() assigns each position to an interval
        state_vec <- seg_states[cut(
          seq_len(n()),
          breaks = cpt_locs,
          labels = FALSE,
          include.lowest = TRUE
        )]

        state_vec
      }
    ) %>%
    ungroup()

  dets$state_prob <- NA_real_

  return(dets)

}

#' INTERNAL: Standardize state labels to ensure consistency
#'
#' Relabels behavioral states so that higher state numbers consistently
#' correspond to higher activity levels. States are ranked by their mean
#' normalized speed, and labels are reassigned accordingly (1 = lowest
#' activity, n = highest activity).
#'
#' @param dets Dataframe containing \code{state} and \code{norm_speed} columns
#'
#' @return Dataframe with relabeled \code{state} column where state numbers
#'   increase monotonically with activity level
#'
#' @keywords internal
#' @importFrom stats setNames

.label_states <- function(dets) {
  # Ensure higher state number = "active" (higher activity)
  state_scores <- dets %>%
    filter(!is.na(.data$state)) %>%
    group_by(.data$state) %>%
    summarise(mean_activity = mean(.data$norm_speed, na.rm = TRUE)) %>%
    arrange(.data$mean_activity)

  state_map <- setNames(seq_len(nrow(state_scores)), state_scores$state)
  dets$state <- state_map[as.character(dets$state)]
  return(dets) # nolint: return_linter.
}

#' INTERNAL: Calculate optimal threshold using Otsu's method
#'
#' Implements Otsu's thresholding algorithm to automatically determine the
#' optimal threshold that separates data into two classes by minimizing
#' within-class variance (equivalently, maximizing between-class variance).
#' Uses histogram-based approach for computational efficiency.
#'
#' @param x Numeric vector of values to threshold. NA values are removed
#'   automatically
#'
#' @return Numeric scalar representing the optimal threshold value that
#'   separates the data into two classes
#'
#' @keywords internal

.otsu_threshold <- function(x) {
  # Remove NAs
  x <- x[!is.na(x)]
  n <- length(x)

  if (n < 2) {
    stop("Need at least 2 values to calculate Otsu threshold")
  }

  # Use histogram-based approach (much faster than sorting all values)
  # Create histogram with automatic binning
  hist_data <- hist(x, breaks = "FD", plot = FALSE)

  # Extract bin information
  counts <- hist_data$counts
  mids <- hist_data$mids
  n_bins <- length(counts)

  # Precompute cumulative sums for efficiency
  cum_counts <- cumsum(counts)
  cum_sums <- cumsum(counts * mids)
  total_sum <- cum_sums[n_bins]

  # Initialize best threshold
  best_variance <- Inf
  best_threshold <- mids[1]

  # Loop through possible thresholds (much fewer iterations than before)
  for (i in 1:(n_bins - 1)) {

    # Number of values in each class
    n1 <- cum_counts[i]
    n2 <- n - n1

    # Skip if either class is empty
    if (n1 == 0 || n2 == 0) next

    # Calculate class weights
    w1 <- n1 / n
    w2 <- n2 / n

    # Calculate class means
    mu1 <- cum_sums[i] / n1
    mu2 <- (total_sum - cum_sums[i]) / n2

    # Calculate between-class variance (maximizing this is equivalent to
    # minimizing within-class variance)
    between_var <- w1 * w2 * (mu1 - mu2)^2

    # Update best threshold (we minimize within-class variance, which is
    # equivalent to maximizing between-class variance)
    if (-between_var < best_variance) {
      best_variance <- -between_var
      best_threshold <- mids[i]
    }
  }

  return(best_threshold) # nolint: return_linter.

}


