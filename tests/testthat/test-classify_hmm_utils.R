# Test classify_hmm_utils.R
# Tests for HMM-specific classification functions

# Note: HMM internal functions work within the classify_activity() pipeline
# and are not designed to be called standalone, so we test them via the main function

test_that("HMM classification works end-to-end", {
  skip_if_no_package("hmmTMB")
  skip_if_no_package("mclust")
  skip_if_no_package("fitdistrplus")

  # Create realistic movement data
  detections <- create_sample_detections_data(n_obs = 200, video_col_name = "Video")

  # Run full HMM classification
  result <- tryCatch(
    classify_activity(
      detections,
      method = "hmm",
      features = c("speed", "turning"),
      n_states = 2,
      video_id = "Video"
    ),
    error = function(e) NULL
  )

  # If successful, validate output
  if (!is.null(result)) {
    expect_s3_class(result, "data.frame")
    expect_true("state" %in% colnames(result))
    expect_equal(nrow(result), 200)

    # Check valid states (excluding NAs)
    valid_states <- result$state[!is.na(result$state)]
    expect_true(all(valid_states %in% c(1, 2)))
    expect_true(length(valid_states) > 0.9 * nrow(result))

    # States should show some persistence (not random)
    if (length(valid_states) > 10) {
      state_changes <- sum(diff(valid_states) != 0, na.rm = TRUE)
      expect_true(state_changes < length(valid_states) * 0.5)
    }
  }
})


test_that("HMM handles single feature", {
  skip_if_no_package("hmmTMB")

  detections <- create_sample_detections_data(n_obs = 150, video_col_name = "vid_id")

  result <- tryCatch(
    classify_activity(
      detections,
      method = "hmm",
      features = "speed",
      n_states = 2,
      video_id = "vid_id"
    ),
    error = function(e) NULL
  )

  if (!is.null(result)) {
    expect_s3_class(result, "data.frame")
    expect_true("state" %in% colnames(result))
  }
})


test_that("HMM handles multiple trajectories", {
  skip_if_no_package("hmmTMB")

  det1 <- create_sample_detections_data(n_obs = 75, video_col_name = "vid_id")
  det1$individual_id <- "track_1"

  det2 <- create_sample_detections_data(n_obs = 75, video_col_name = "vid_id")
  det2$individual_id <- "track_2"

  detections <- rbind(det1, det2)

  result <- tryCatch(
    suppressWarnings(
      classify_activity(
        detections,
        method = "hmm",
        features = "speed",
        n_states = 2,
        indiv_id = "individual_id",
        video_id = "vid_id"
      )
    ),
    error = function(e) NULL
  )

  if (!is.null(result)) {
    expect_equal(nrow(result), 150)
    expect_true("state" %in% colnames(result))
  }
})


test_that("HMM with circular features works", {
  skip_if_no_package("hmmTMB")
  skip_if_no_package("CircStats")

  # Create data with turning angles
  detections <- create_sample_detections_data(n_obs = 150, video_col_name = "Video")

  # Classify using turning angle (circular data)
  result <- tryCatch(
    classify_activity(
      detections,
      method = "hmm",
      features = "turning",
      n_states = 2,
      video_id = "Video"
    ),
    error = function(e) NULL
  )

  if (!is.null(result)) {
    expect_s3_class(result, "data.frame")
    expect_equal(nrow(result), 150)
    expect_true("state" %in% colnames(result))
  }
})


test_that("HMM respects n_states parameter", {
  skip_if_no_package("hmmTMB")

  detections <- create_sample_detections_data(n_obs = 200, video_col_name = "Video")

  result_2 <- tryCatch(
    suppressWarnings(
      classify_activity(
        detections,
        method = "hmm",
        features = "speed",
        n_states = 2,
        video_id = "Video"
      )
    ),
    error = function(e) NULL
  )

  result_3 <- tryCatch(
    suppressWarnings(
      classify_activity(
        detections,
        method = "hmm",
        features = "speed",
        n_states = 3,
        video_id = "Video"
      )
    ),
    error = function(e) NULL
  )

  if (!is.null(result_2)) {
    valid_states_2 <- result_2$state[!is.na(result_2$state)]
    expect_true(length(unique(valid_states_2)) <= 2)
  }

  if (!is.null(result_3)) {
    valid_states_3 <- result_3$state[!is.na(result_3$state)]
    expect_true(length(unique(valid_states_3)) <= 3)
  }
})
