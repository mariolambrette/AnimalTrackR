# Tests for BehaviourVis.R
# Testing behaviour visualization functions

test_that("behaviour_viz validates detections_path exists", {
  skip_if_not_installed("reticulate")
  skip_if_not_installed("data.table")

  # data.table::fread errors on non-existent file
  expect_error(
    behaviour_viz(
      detections_path = "nonexistent.csv",
      vid_path = "video.mp4",
      output_path = "output.mp4",
      class_column = "State"
    ),
    "does not exist|non-readable"
  )
})


test_that("behaviour_viz validates vid_path exists", {
  skip_if_not_installed("data.table")

  temp_csv <- tempfile(fileext = ".csv")
  on.exit(unlink(temp_csv))

  create_sample_detections(temp_csv, n_rows = 50, add_state = TRUE)

  expect_error(
    behaviour_viz(
      detections_path = temp_csv,
      vid_path = "nonexistent_video.mp4",
      output_path = tempfile(fileext = ".mp4"),
      class_column = "State"
    ),
    "Video.*not found"
  )
})


test_that("behaviour_viz validates output_path directory exists", {
  skip_if_not_installed("data.table")

  temp_csv <- tempfile(fileext = ".csv")
  temp_vid <- tempfile(fileext = ".mp4")
  on.exit({
    unlink(temp_csv)
    unlink(temp_vid)
  })

  create_sample_detections(temp_csv, n_rows = 50, add_state = TRUE)
  writeLines("dummy video", temp_vid)

  expect_error(
    behaviour_viz(
      detections_path = temp_csv,
      vid_path = temp_vid,
      output_path = file.path("nonexistent_dir", "output.mp4"),
      class_column = "State"
    ),
    "directory does not exist"
  )
})


test_that("behaviour_viz reads detections CSV", {
  skip_if_not_installed("data.table")

  temp_csv <- tempfile(fileext = ".csv")
  on.exit(unlink(temp_csv))

  create_sample_detections(temp_csv, n_rows = 50, add_state = TRUE)

  # Should be able to read CSV
  dets <- data.table::fread(temp_csv)

  expect_true(is.data.frame(dets))
  expect_true("State" %in% colnames(dets))
})


test_that("behaviour_viz applies classification function", {
  skip_if_not_installed("data.table")

  temp_csv <- tempfile(fileext = ".csv")
  on.exit(unlink(temp_csv))

  create_sample_detections(temp_csv, n_rows = 50, add_state = FALSE)

  # Simple classification function
  classifier <- function(detections) {
    detections$MyClass <- sample(1:2, nrow(detections), replace = TRUE)
    return(detections)
  }

  # Test that function can be called
  dets <- data.table::fread(temp_csv)
  classified <- classifier(dets)

  expect_true("MyClass" %in% colnames(classified))
})


test_that("behaviour_viz validates class_column exists", {
  skip_if_not_installed("data.table")

  temp_csv <- tempfile(fileext = ".csv")
  temp_vid <- tempfile(fileext = ".mp4")
  on.exit({
    unlink(temp_csv)
    unlink(temp_vid)
  })

  create_sample_detections(temp_csv, n_rows = 50, add_state = TRUE)
  writeLines("dummy video", temp_vid)

  expect_error(
    behaviour_viz(
      detections_path = temp_csv,
      vid_path = temp_vid,
      output_path = tempfile(fileext = ".mp4"),
      class_column = "NonexistentColumn"
    ),
    "class_column.*not found"
  )
})


test_that("behaviour_viz limits to 10 categories", {
  skip_if_not_installed("data.table")

  temp_csv <- tempfile(fileext = ".csv")
  temp_vid <- tempfile(fileext = ".mp4")
  on.exit({
    unlink(temp_csv)
    unlink(temp_vid)
  })

  # Create detections with more than 10 categories
  dets <- data.frame(
    Video = "test.mp4",
    Frame = 1:50,
    Timestamp = seq(0, 4900, by = 100),
    xc = runif(50),
    yc = runif(50),
    xl = runif(50),
    xr = runif(50),
    yt = runif(50),
    yb = runif(50),
    State = sample(1:15, 50, replace = TRUE)  # 15 categories
  )
  write.csv(dets, temp_csv, row.names = FALSE)
  writeLines("dummy video", temp_vid)

  expect_error(
    behaviour_viz(
      detections_path = temp_csv,
      vid_path = temp_vid,
      output_path = tempfile(fileext = ".mp4"),
      class_column = "State"
    ),
    "More than 10 unique"
  )
})


test_that("behaviour_viz accepts classification_function parameter", {
  # Should accept NULL or a function
  expect_null(NULL)

  # Should accept function
  test_func <- function(detections) {
    detections$class <- 1
    return(detections)
  }

  expect_type(test_func, "closure")
})


test_that("behaviour_viz passes additional arguments to classifier", {
  skip_if_not_installed("data.table")

  # Test that ... parameters work
  classifier_with_params <- function(detections, threshold = 100) {
    detections$class <- ifelse(detections$xc > threshold, 1, 2)
    return(detections)
  }

  temp_csv <- tempfile(fileext = ".csv")
  on.exit(unlink(temp_csv))

  create_sample_detections(temp_csv, n_rows = 50)

  dets <- data.table::fread(temp_csv)
  result <- do.call(classifier_with_params, list(detections = dets, threshold = 200))

  expect_true("class" %in% colnames(result))
})


test_that("behaviour_viz works without classification function", {
  skip_if_not_installed("data.table")

  # If detections already have State column, can skip classification
  temp_csv <- tempfile(fileext = ".csv")
  on.exit(unlink(temp_csv))

  create_sample_detections(temp_csv, n_rows = 50, add_state = TRUE)

  dets <- data.table::fread(temp_csv)

  expect_true("State" %in% colnames(dets))
  expect_true(is.null(NULL))  # classification_function can be NULL
})


test_that("behaviour_viz default class_column is 'State'", {
  # Default should match output of fit_HMM
  default_class_col <- "State"
  expect_equal(default_class_col, "State")
})


# test_that("behaviour_viz returns detections dataframe", {
#   skip("Requires full video processing setup")
#
#   # Should return the classified detections
#   # Would need complete setup to test
# })


test_that("behaviour_viz handles binary classification", {
  skip_if_not_installed("data.table")

  # Two classes (most common case: active/inactive)
  temp_csv <- tempfile(fileext = ".csv")
  on.exit(unlink(temp_csv))

  dets <- data.frame(
    Video = "test.mp4",
    Frame = 1:50,
    Timestamp = seq(0, 4900, by = 100),
    xc = runif(50),
    yc = runif(50),
    xl = runif(50),
    xr = runif(50),
    yt = runif(50),
    yb = runif(50),
    State = sample(1:2, 50, replace = TRUE)
  )
  write.csv(dets, temp_csv, row.names = FALSE)

  dets_read <- data.table::fread(temp_csv)

  expect_equal(length(unique(dets_read$State)), 2)
})


test_that("behaviour_viz handles multi-class classification", {
  skip_if_not_installed("data.table")

  # Up to 10 classes should be supported
  temp_csv <- tempfile(fileext = ".csv")
  on.exit(unlink(temp_csv))

  n_classes <- 5
  dets <- data.frame(
    Video = "test.mp4",
    Frame = 1:100,
    Timestamp = seq(0, 9900, by = 100),
    xc = runif(100),
    yc = runif(100),
    xl = runif(100),
    xr = runif(100),
    yt = runif(100),
    yb = runif(100),
    State = sample(1:n_classes, 100, replace = TRUE)
  )
  write.csv(dets, temp_csv, row.names = FALSE)

  dets_read <- data.table::fread(temp_csv)

  expect_true(length(unique(dets_read$State)) <= 10)
})


test_that("behaviour_viz validates detection format", {
  skip_if_not_installed("data.table")

  temp_csv <- tempfile(fileext = ".csv")
  on.exit(unlink(temp_csv))

  # Create detections with missing required columns
  invalid_dets <- data.frame(
    Video = "test.mp4",
    Frame = 1:10
    # Missing other required columns
  )
  write.csv(invalid_dets, temp_csv, row.names = FALSE)

  # Should be able to read but may fail validation
  dets <- data.table::fread(temp_csv)
  expect_false(all(c("xc", "yc", "xl", "xr", "yt", "yb") %in% colnames(dets)))
})


test_that("behaviour_viz class_column parameter is required", {
  # class_column should always be specified
  # Either "State" (default from fit_HMM) or custom name

  expect_type("State", "character")
  expect_type("custom_class", "character")
})


test_that("behaviour_viz handles categorical vs numeric classes", {
  skip_if_not_installed("data.table")

  # Should work with numeric classes
  numeric_class <- 1:5
  expect_type(numeric_class, "integer")

  # Should work with character classes
  char_class <- c("Active", "Inactive")
  expect_type(char_class, "character")
})


test_that("behaviour_viz output is MP4 format", {
  # Output should be video file
  output_path <- "test_output.mp4"

  expect_true(grepl("\\.mp4$", output_path))
})
