# The main activity classification function for AnimalTrackR.

#' @title Classify activity states from AnimalTrackR detections
#'
#' @description
#' This function takes AnimalTrackR detections and classifies them into
#' behavioural states. The default behaviour is to use a Gaussian Mixture Model
#' (GMM) to classify activity based on speed, acceleration and turning features,
#' into two states (active/inactive). Other classification methods and features
#' are also supported. Users are encouraged to explore the various options to
#' find the best approach for their data. A detailed description of the
#' function and its parameters can be found in the vignette:
#' \code{vignette("classify-activity")}. It is recommended to read through the
#' vignette to understand the various options and their implications before
#' using this function.
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
#' @param method Classification method to use. Options are: \itemize{
#'  \item \code{"gmm"}: Gaussian Mixture Model (default)
#'  \item \code{"threshold"}: Otsu thresholding on composite activity score
#'  \item \code{"changepoint"}: Change point detection on composite activity
#'  score
#'  \item \code{"hmm"}: Hidden Markov Model on raw features
#' }
#' @param features Character vector of movement features to use for
#' classification. Options include: \itemize{
#'  \item \code{"speed"} - Instantaneous speed (pixels per frame)
#'  \item \code{"acceleration"} - Change in speed between frames
#'  \item \code{"turning"} - Absolute turning angle (radians)
#'  \item \code{"heading"} - Absolute heading (radians)
#'  \item \code{"heading_smooth"} - Heading smoothed over rolling window
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
#' @param n_states Integer. Number of activity states to classify. Default is 2
#' (active/inactive).
#' @param window_size Integer. Rolling window size for smoothed features. The
#' window will be \code{window_size} rows wide in the \code{dets} dataframe, so
#' users should consider the framerate of their data when selecting this
#' parameter. Default is 5.
#' @param composite_method Method to combine multiple features into a single
#' composite activity score. Options are: \itemize{
#'  \item \code{"mean"}: Mean of selected features (default)
#'  \item \code{"pca"}: First principal component of selected features
#'  \item \code{"weighted"}: Weighted mean of selected features (requires
#'  \code{feature_weights} parameter)
#' }
#' @param feature_weights Named numeric vector of feature weights to use when
#' \code{composite_method = "weighted"}. If \code{NULL}, equal weights are used.
#' The length and names of the vector must match the \code{features} parameter.
#' Weights are normalised to sum to 1 internally if they do not already.
#' @param indiv_id Character. Optional column name in \code{dets} specifying
#' individual ID. If \code{NULL}, all detections are assumed to be from a single
#' individual. For HMM, separate models are fitted per individual.
#' @param cpt_method Change point detection method to use when
#' \code{method = "changepoint"}.
#' Options are: \itemize{
#'  \item \code{"mean"}: Identify changepoints in the mean of the composite
#'  activity score
#'  \item \code{"var"}: Identify changepoints in the variance of the composite
#'  activity score
#'  \item \code{"meanvar"}: Identify changepoints in both the mean and variance
#'  of the composite activity score
#' }
#' Default is \code{"mean"}.
#' @param pause_threshold Numeric. Speed threshold (in pixels per frame) below
#' which the animal is considered to be paused/stationary. Required if using
#' pause-based features (\code{"paused"} or \code{"time_since_pause"}).
#' Default is \code{NULL}.
#' @param video_id Character. Column name in \code{dets} that identifies
#' different videos. Default is \code{"vid_id"}. Change this only if your
#' detection data uses a different column name for video identification (e.g.,
#' \code{"video_name"} or \code{"recording_id"}).
#'
#' @return A dataframe identical to \code{dets} with the following columns
#' added:
#' \itemize{
#'   \item \code{state}: Integer state assignment (1 = inactive, 2 = active when
#'   \code{n_states = 2})
#'   \item Feature columns as calculated (e.g., \code{speed},
#'   \code{acceleration})
#'   \item \code{composite_score}: Composite activity metric (for GMM,
#'   threshold, and changepoint methods only)
#'   \item Normalized feature columns prefixed with \code{norm_} (for non-HMM
#'   methods)
#' }
#'
#' @section Dependencies:
#' Different classification methods require different packages:
#' \itemize{
#'   \item \strong{gmm}: Requires \code{mclust}
#'   \item \strong{threshold}: No additional dependencies
#'   \item \strong{changepoint}: Requires \code{changepoint}
#'   \item \strong{hmm}: Requires \code{hmmTMB}, \code{fitdistrplus}, and
#'     \code{CircStats}
#' }
#' Install optional dependencies with:
#' \code{install.packages(c("changepoint", "hmmTMB", "fitdistrplus",
#' "CircStats", "mclust"))}
#'
#' @examples
#' \dontrun{
#' # Load example detection data
#' dets <- read.csv("path/to/detections.csv")
#'
#' # Basic GMM classification with default features
#' result <- classify_activity(dets)
#'
#' # HMM classification with custom features
#' result <- classify_activity(
#'   dets,
#'   method = "hmm",
#'   features = c("speed", "turning", "spatial_spread"),
#'   indiv_id = "fish_id"
#' )
#'
#' # Threshold classification with weighted composite
#' result <- classify_activity(
#'   dets,
#'   method = "threshold",
#'   features = c("speed", "acceleration", "turning"),
#'   composite_method = "weighted",
#'   feature_weights = c(speed = 2, acceleration = 1, turning = 1)
#' )
#'
#' # Changepoint detection with custom window
#' result <- classify_activity(
#'   dets,
#'   method = "changepoint",
#'   features = c("speed_smooth", "path_straightness"),
#'   window_size = 30,
#'   cpt_method = "meanvar"
#' )
#' }
#'
#' @seealso \code{\link{calculate_movement_features}},
#' \code{\link{behaviour_viz}}, \code{vignette("classify-activity")}
#'
#' @export
#'
#' @import magrittr
#' @import dplyr
#' @importFrom data.table frollmean frollapply frollsum
#' @importFrom stats quantile
#' @importFrom utils head

classify_activity <- function(
  dets,
  method = "gmm",
  features = c("speed", "acceleration", "turning"),
  n_states = 2,
  window_size = 5,
  composite_method = "mean",
  feature_weights = NULL,
  indiv_id = NULL,
  cpt_method = "mean",
  pause_threshold = NULL,
  video_id = "Video"
) {

  #### 0. INPUT VALIDATION -----------------------------------------------------

  # Check dets is a data frame
  if (!is.data.frame(dets)) {
    stop("'dets' must be a data frame. Received: ", class(dets)[1])
  }

  # Check required columns exist
  required_cols <- if (is.null(indiv_id)) {
    c(video_id, "Frame", "xc", "yc")
  } else {
    c(video_id, "Frame", "xc", "yc", indiv_id)
  }
  missing_cols <- setdiff(required_cols, names(dets))
  if (length(missing_cols) > 0) {
    stop(
      "Missing required columns: ", paste(missing_cols, collapse = ", "), "\n",
      "Your data has columns: ", paste(head(names(dets), 10), collapse = ", "),
      if (length(names(dets)) > 10) "..." else ""
    )
  }

  # Check for minimum data
  if (nrow(dets) < 2) {
    stop("Need at least 2 detections to classify activity. Your data has ",
         nrow(dets), " row(s).")
  }

  # Check window_size is valid
  if (!is.numeric(window_size) || window_size < 1) {
    stop("'window_size' must be a positive integer. Received: ", window_size)
  }

  if (window_size > nrow(dets)) {
    warning(
      "'window_size' (", window_size, ") is larger than number of detections (",
      nrow(dets), "). Rolling features will be mostly NA. ",
      "Consider using a smaller window_size."
    )
  }

  # Check n_states is valid
  if (!is.numeric(n_states) || n_states < 2) {
    stop("'n_states' must be an integer >= 2. Received: ", n_states)
  }

  # Check features are valid
  valid_features <- c("speed", "acceleration", "turning", "speed_var",
                      "speed_smooth", "meander",
                      "path_straightness", "paused",
                      "time_since_pause", "spatial_spread", "edge_preference",
                      "roaming_entropy", "heading", "heading_smooth")
  invalid_features <- setdiff(features, valid_features)
  if (length(invalid_features) > 0) {
    stop(
      "Invalid features specified: ",
      paste(invalid_features, collapse = ", "), "\n",
      "  Valid features are:\n    ",
      paste(valid_features, collapse = ", ")
    )
  }

  # Check method is valid
  valid_methods <- c("gmm", "threshold", "changepoint", "hmm")
  if (!method %in% valid_methods) {
    stop(
      "'method' must be one of: ", paste(valid_methods, collapse = ", "), "\n",
      "  Received: '", method, "'"
    )
  }

  # Check composite_method is valid
  valid_composite <- c("mean", "pca", "weighted")
  if (!composite_method %in% valid_composite) {
    stop(
      "'composite_method' must be one of: ",
      paste(valid_composite, collapse = ", "), "\n",
      "  Received: '", composite_method, "'"
    )
  }

  # Check feature_weights if weighted method used
  if (composite_method == "weighted") {
    if (is.null(feature_weights)) {
      warning(
        "'feature_weights' not provided for weighted method -",
        "using equal weights"
      )
      feature_weights <- rep(1, length(features))
      names(feature_weights) <- features
    }
    if (!is.numeric(feature_weights) || is.null(names(feature_weights))) {
      stop(
        "'feature_weights' must be a named numeric vector.\n",
        " Example: c(speed = 2, acceleration = 1, turning = 1)"
      )
    }
  }

  # Check indiv_id column exists if provided
  if (!is.null(indiv_id) && !indiv_id %in% names(dets)) {
    stop(
      "Individual ID column '", indiv_id, "' not found in data.\n",
      "  Available columns: ", paste(names(dets), collapse = ", ")
    )
  }

  # Warn about coordinates
  if (any(is.na(dets$xc)) || any(is.na(dets$yc))) {
    n_na <- sum(is.na(dets$xc) | is.na(dets$yc))
    pct_na <- round(100 * n_na / nrow(dets), 1)
    warning(
      n_na,
      " detections (",
      pct_na,
      "%) have missing coordinates and will produce NA features"
    )
  }

  # Check pause_threshold if pause-based features used
  pause_features <- c("paused", "time_since_pause")
  if (
    any(features %in% c("paused", "time_since_pause")) &&
      is.null(pause_threshold)
  ) {
    stop(
      "'pause_threshold' must be provided when using pause-based features.\n",
      "  You requested: ",
      paste(intersect(features, pause_features), collapse = ", "), "\n",
      "Set pause_threshold to the speed below which the animal is considered
       stationary."
    )
  }

  # Handle vid_id input name - MUST happen before any processing
  if (video_id != "vid_id") {
    if (!video_id %in% names(dets)) {
      stop(
        "Video ID column '", video_id, "' not found in data.\n",
        "  Available columns: ", paste(names(dets), collapse = ", ")
      )
    }
    names(dets)[names(dets) == video_id] <- "vid_id"
    video_id <- "vid_id"  # Update variable to reflect the renamed column
  }


  #### 1. CALCULATE FEATURES ---------------------------------------------------

  dets <- calculate_movement_features(
    dets,
    features = features,
    window_size = window_size,
    pause_threshold = pause_threshold
  )


  #### 2. CAP EXTREME OUTLIERS -------------------------------------------------

  # Cap extreme outliers at 1st/99th percentile for all feature columns.
  # This is applied consistently across all classification methods to:
  #   1. Remove tracking artifacts (i.e. the effect on distance measurements of
  #      tracking errors)
  #   2. Ensure normalization uses biologically meaningful range
  #   3. Prevent numerical issues in model fitting (HMM, GMM)
  #
  # Note: This affects ~2% of observations (1% at each tail). Values are
  # capped (Winsorized), not removed, so sample size is preserved.

  feature_cols_to_cap <- intersect(features, names(dets))
  # Always include speed since it's calculated regardless of feature selection
  if ("speed" %in% names(dets)) {
    feature_cols_to_cap <- unique(c("speed", feature_cols_to_cap))
  }

  total_capped <- 0
  total_obs <- 0

  for (feat in feature_cols_to_cap) {
    feat_vals <- dets[[feat]]
    if (!all(is.na(feat_vals))) {
      q99 <- quantile(feat_vals, 0.99, na.rm = TRUE)
      q01 <- quantile(feat_vals, 0.01, na.rm = TRUE)
      n_capped_high <- sum(feat_vals > q99, na.rm = TRUE)
      n_capped_low <- sum(feat_vals < q01, na.rm = TRUE)

      total_capped <- total_capped + n_capped_high + n_capped_low
      total_obs <- total_obs + sum(!is.na(feat_vals))

      if (n_capped_high > 0 || n_capped_low > 0) {
        dets[[feat]][dets[[feat]] > q99] <- q99
        dets[[feat]][dets[[feat]] < q01] <- q01
      }
    }
  }

  # Warn if a lot of data was capped (might indicate data quality issues)
  if (total_obs > 0) {
    pct_capped <- 100 * total_capped / total_obs
    if (pct_capped > 5) {
      warning(
        "High proportion of data capped (", round(pct_capped, 1), "%). ",
        "This may indicate data quality issues (tracking errors, artifacts). ",
        "Consider inspecting your raw data."
      )
    }
  }


  #### 3. HMM EARLY EXIT -------------------------------------------------------

  # HMM uses raw features (not normalized) and handles distribution selection
  # internally, so we branch off here before normalization
  if (method == "hmm") {
    dets <- .classify_hmm(
      dets = dets,
      features = features,
      n_states = n_states,
      indiv_id = indiv_id,
      video_id = video_id
    )
    return(dets)
  }


  #### 4. NORMALISE FEATURES WITHIN FISH ---------------------------------------

  # This controls for individual variation in activity levels. Normalising
  # across individual rather than video allows variation between videos to
  # be preserved.
  feature_cols <- unique(c("speed", features))

  dets <- dets %>%

    # Group by Individual ID if provided
    {if (!is.null(indiv_id)) dplyr::group_by(., dplyr::across(dplyr::all_of(indiv_id))) else .} %>%

    # Normalise selected features to [0, 1] range
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(feature_cols),
        ~ (. - min(., na.rm = TRUE)) /
          (max(., na.rm = TRUE) -
             min(., na.rm = TRUE)),
        .names = "norm_{col}"
      )
    ) %>%

    # Ungroup data
    dplyr::ungroup() %>%

    # Deselect non-normalised features
    dplyr::select(-dplyr::all_of(feature_cols))

  # Create vector of feature names to use for analysis based on user selected
  # features
  feature_cols_use <- paste0("norm_", features)


  #### 5. COMBINE FEATURES INTO COMPOSITE ACTIVITY SCORE -----------------------

  dets <- .create_composite_score(
    dets,
    feature_cols = feature_cols_use,
    method = composite_method,
    feature_weights = feature_weights,
    indiv_id = indiv_id
  )


  #### 6. CLASSIFY ACTIVITY STATES ---------------------------------------------

  # Conduct classification based on the specified method.

  if (method == "gmm") {
    dets <- .classify_gmm(dets, feature_cols_use, n_states)
  } else if (method == "threshold") {
    dets <- .classify_threshold(dets)
  } else if (method == "changepoint") {
    dets <- .classify_changepoint(dets, method = cpt_method)
  } else if (method == "hmm") {
    # HMM is handled earlier (before normalization) - this branch should not
    # be reached
    stop("Internal error: HMM method should have returned earlier")
  } else {
    stop(
      "Invalid classification method. Must be 'gmm', 'threshold',
      'changepoint', or 'hmm'"
    )
  }

  #### 7. LABEL STATES CONSISTENTLY --------------------------------------------

  # This ensures that state 2 always refers to the "active" state. Only applied
  # when n_states == 2

  if (n_states == 2) {
    dets <- .label_states(dets)
  } else {
    warning(
      "State labels could not be verified for n_states > 2.\n
      You may wish to check that higher state numbers correspond to higher
      activity levels."
    )
  }
  # Remove only 'state_labeled' and 'active' columns if present
  drop_cols <- intersect(c("state_labeled", "active"), names(dets))
  if (length(drop_cols) > 0) {
    dets <- dets[, !(names(dets) %in% drop_cols), drop = FALSE]
  }


  #### 8. RETURN CLASSIFIED DETECTIONS -----------------------------------------
  return(dets)

}
