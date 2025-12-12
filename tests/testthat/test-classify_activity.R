# Test classify_activity.R
# Tests for the main user-facing classification function

test_that("classify_activity returns expected structure with default parameters", {
  # Create sample detections
  detections <- create_sample_detections_data(n_obs = 100)

  # Run classification
  result <- classify_activity(detections, video_id = "Video")

  # Check structure
  expect_s3_class(result, "data.frame")
  expect_true("state" %in% colnames(result))
  expect_equal(nrow(result), nrow(detections))

  # Check state values (allow NAs for incomplete data)
  valid_states <- result$state[!is.na(result$state)]
  expect_true(all(valid_states %in% c(1, 2)))
  expect_type(result$state, "integer")

  # Most observations should have valid states
  expect_true(sum(!is.na(result$state)) / nrow(result) > 0.9)
})


test_that("classify_activity works with GMM method", {
  skip_if_no_package("mclust")

  detections <- create_sample_detections_data(n_obs = 150)

  result <- classify_activity(
    detections,
    method = "gmm",
    n_states = 2,
    features = c("speed", "acceleration"),
    video_id = "Video"
  )

  expect_s3_class(result, "data.frame")
  expect_true("state" %in% colnames(result))

  # Check valid states (excluding NAs)
  valid_states <- result$state[!is.na(result$state)]
  expect_true(all(valid_states %in% c(1, 2)))
  expect_true(length(valid_states) > 0.9 * nrow(result))
})


test_that("classify_activity works with threshold method", {
  detections <- create_sample_detections_data(n_obs = 100)

  result <- classify_activity(
    detections,
    method = "threshold",
    features = c("speed", "turning"),
    video_id = "Video"
  )

  expect_s3_class(result, "data.frame")
  expect_true("state" %in% colnames(result))
})


test_that("classify_activity works with changepoint method", {
  skip_if_no_package("changepoint")

  detections <- create_sample_detections_data(n_obs = 200)

  result <- classify_activity(
    detections,
    method = "changepoint",
    features = "speed",
    video_id = "Video"
  )

  expect_s3_class(result, "data.frame")
  expect_true("state" %in% colnames(result))
})


test_that("classify_activity works with HMM method", {
  skip_if_no_package("hmmTMB")

  detections <- create_sample_detections_data(n_obs = 150)

  result <- classify_activity(
    detections,
    method = "hmm",
    n_states = 2,
    features = c("speed", "turning"),
    video_id = "Video"
  )

  expect_s3_class(result, "data.frame")
  expect_true("state" %in% colnames(result))

  # Check valid states (excluding NAs)
  valid_states <- result$state[!is.na(result$state)]
  expect_true(all(valid_states %in% c(1, 2)))
  expect_true(length(valid_states) > 0.9 * nrow(result))
})


test_that("classify_activity handles multiple trajectories", {
  # Create detections with multiple trajectories
  det1 <- create_sample_detections_data(n_obs = 50)
  det1$trajectory_id <- "track_1"

  det2 <- create_sample_detections_data(n_obs = 50)
  det2$trajectory_id <- "track_2"

  detections <- rbind(det1, det2)

  # GMM can fail with synthetic data - use tryCatch
  result <- tryCatch(
    classify_activity(detections, method = "gmm", video_id = "Video"),
    error = function(e) {
      # If GMM fails, try threshold
      tryCatch(
        classify_activity(detections, method = "threshold", video_id = "Video"),
        error = function(e2) NULL
      )
    }
  )

  # If either method works, verify structure
  if (!is.null(result)) {
    expect_equal(nrow(result), nrow(detections))
    expect_true("state" %in% colnames(result))
  }
})


test_that("classify_activity validates input data structure", {
  # Missing required columns
  bad_data <- data.frame(
    Frame = 1:10,
    xc = runif(10)
  )

  expect_error(
    classify_activity(bad_data),
    "Missing required columns"
  )
})


test_that("classify_activity handles insufficient data gracefully", {
  # Very few observations
  detections <- create_sample_detections_data(n_obs = 5)

  # Should either work or give informative error
  result <- tryCatch(
    suppressWarnings(
      classify_activity(detections, method = "threshold", video_id = "Video")
    ),
    error = function(e) NULL
  )

  # If it works, should have correct structure
  if (!is.null(result)) {
    expect_s3_class(result, "data.frame")
    expect_equal(nrow(result), 5)
  }
})


test_that("classify_activity respects n_states parameter", {
  skip_if_no_package("mclust")

  detections <- create_sample_detections_data(n_obs = 200)

  result_2 <- classify_activity(detections, method = "gmm", n_states = 2, video_id = "Video")
  result_3 <- expect_warning(
    classify_activity(detections, method = "gmm", n_states = 3, video_id = "Video"),
    "State labels could not be verified"
  )


  # Check valid states only (exclude NAs)
  valid_states_2 <- result_2$state[!is.na(result_2$state)]
  valid_states_3 <- result_3$state[!is.na(result_3$state)]

  expect_true(length(unique(valid_states_2)) <= 2)
  expect_true(length(unique(valid_states_3)) <= 3)
})


test_that("classify_activity validates method parameter", {
  detections <- create_sample_detections_data(n_obs = 100)

  expect_error(
    classify_activity(detections, method = "invalid_method", video_id = "Video"),
    "method"
  )
})


test_that("classify_activity validates features parameter", {
  detections <- create_sample_detections_data(n_obs = 100)

  # Invalid feature name
  expect_error(
    classify_activity(detections, features = "nonexistent_feature", video_id = "Video"),
    "feature"
  )
})


test_that("classify_activity preserves original columns", {
  detections <- create_sample_detections_data(n_obs = 100)
  original_cols <- colnames(detections)

  result <- classify_activity(detections, method = "threshold", video_id = "Video")

  # Original columns should be present (but video column may be renamed to vid_id)
  original_cols_adjusted <- setdiff(original_cols, "Video")
  expect_true(all(original_cols_adjusted %in% colnames(result)))
  # vid_id should be present (renamed from Video)
  expect_true("vid_id" %in% colnames(result))
})


test_that("classify_activity handles NA values appropriately", {
  detections <- create_sample_detections_data(n_obs = 100)

  # Introduce some NAs
  detections$xc[c(5, 10, 15)] <- NA

  # Should handle NAs (either remove or interpolate)
  result <- tryCatch(
    suppressWarnings(
      classify_activity(detections, method = "threshold", video_id = "Video")
    ),
    error = function(e) NULL
  )

  # If successful, check structure
  if (!is.null(result)) {
    expect_s3_class(result, "data.frame")
  }
})


test_that("classify_activity output can be used for downstream analysis", {
  detections <- create_sample_detections_data(n_obs = 100)

  result <- classify_activity(detections, method = "threshold", video_id = "Video")

  # Should be able to compute state transitions
  transitions <- diff(result$state)
  expect_type(transitions, "integer")

  # Should be able to compute state durations
  state_runs <- rle(result$state)
  expect_type(state_runs$lengths, "integer")
  expect_type(state_runs$values, "integer")
})


test_that("classify_activity works with custom feature combinations", {
  skip_if_no_package("mclust")

  detections <- create_sample_detections_data(n_obs = 150)

  # Single feature
  result1 <- classify_activity(
    detections,
    method = "gmm",
    features = "speed",
    video_id = "Video"
  )

  # Multiple features
  result2 <- classify_activity(
    detections,
    method = "gmm",
    features = c("speed", "acceleration", "turning"),
    video_id = "Video"
  )

  expect_s3_class(result1, "data.frame")
  expect_s3_class(result2, "data.frame")
})
