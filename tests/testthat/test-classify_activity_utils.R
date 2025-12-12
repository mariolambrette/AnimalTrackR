# Test classify_activity_utils.R
# Tests for helper functions in activity classification

# Test calculate_movement_features() -------------------------------------------

test_that("calculate_movement_features returns expected structure", {
  detections <- create_sample_detections_data(n_obs = 100, video_col_name = "vid_id")
  
  result <- calculate_movement_features(
    detections,
    features = c("speed", "acceleration", "turning"),
    window_size = 5
  )
  
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), nrow(detections))
  expect_true("speed" %in% colnames(result))
  expect_true("acceleration" %in% colnames(result))
})


test_that("calculate_movement_features handles minimal data", {
  detections <- create_sample_detections_data(n_obs = 10, video_col_name = "vid_id")
  
  result <- calculate_movement_features(
    detections,
    features = c("speed"),
    window_size = 3
  )
  
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 10)
})


test_that("calculate_movement_features preserves original columns", {
  detections <- create_sample_detections_data(n_obs = 50, video_col_name = "vid_id")
  original_cols <- colnames(detections)
  
  result <- calculate_movement_features(
    detections,
    features = c("speed", "acceleration"),
    window_size = 5
  )
  
  expect_true(all(original_cols %in% colnames(result)))
})


test_that("calculate_movement_features produces valid numeric values", {
  detections <- create_sample_detections_data(n_obs = 100, video_col_name = "vid_id")
  
  result <- calculate_movement_features(
    detections,
    features = c("speed", "acceleration", "turning"),
    window_size = 5
  )
  
  # Speed should be non-negative
  expect_true(all(result$speed >= 0, na.rm = TRUE))
})


# Test .otsu_threshold() ------------------------------------------------------

test_that(".otsu_threshold computes threshold", {
  values <- c(rnorm(50, mean = 2, sd = 0.5), rnorm(50, mean = 8, sd = 1))
  
  threshold <- AnimalTrackR:::.otsu_threshold(values)
  
  expect_type(threshold, "double")
  expect_length(threshold, 1)
  expect_true(threshold > 2 && threshold < 8)
})


test_that(".otsu_threshold handles data with variation", {
  low_activity <- rnorm(100, mean = 2, sd = 0.3)
  high_activity <- rnorm(100, mean = 10, sd = 0.3)
  values <- c(low_activity, high_activity)
  
  threshold <- AnimalTrackR:::.otsu_threshold(values)
  
  # Threshold should be between the two modes
  expect_true(threshold > max(low_activity))
  expect_true(threshold < min(high_activity))
})
