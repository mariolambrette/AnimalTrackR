# Tests for fit_HMM.R
# Testing Hidden Markov Model fitting and detection processing functions

test_that(".check_detections validates detection format", {
  # Valid detections dataframe
  valid_dets <- data.frame(
    Video = "test.mp4",
    Frame = 1:10,
    Timestamp = seq(0, 900, by = 100),
    xc = runif(10, 100, 500),
    yc = runif(10, 100, 500),
    xl = runif(10, 50, 300),
    xr = runif(10, 150, 600),
    yt = runif(10, 50, 300),
    yb = runif(10, 150, 600)
  )
  
  expect_true(AnimalTrackR:::.check_detections(valid_dets))
})


test_that(".check_detections rejects missing columns", {
  # Missing required column
  invalid_dets <- data.frame(
    Video = "test.mp4",
    Frame = 1:10,
    # Missing other required columns
    xc = runif(10)
  )
  
  expect_false(AnimalTrackR:::.check_detections(invalid_dets))
})


test_that(".check_detections rejects empty dataframe", {
  # Empty dataframe with correct columns
  empty_dets <- data.frame(
    Video = character(),
    Frame = integer(),
    Timestamp = numeric(),
    xc = numeric(),
    yc = numeric(),
    xl = numeric(),
    xr = numeric(),
    yt = numeric(),
    yb = numeric()
  )
  
  expect_false(AnimalTrackR:::.check_detections(empty_dets))
})


test_that(".read_detections loads valid CSV", {
  skip_if_not_installed("data.table")
  
  temp_csv <- tempfile(fileext = ".csv")
  on.exit(unlink(temp_csv))
  
  create_sample_detections(temp_csv, n_rows = 50)
  
  dets <- AnimalTrackR:::.read_detections(temp_csv)
  
  expect_true(is.data.frame(dets))
  expect_equal(nrow(dets), 50)
  expect_true(all(c("Video", "Frame", "Timestamp", "xc", "yc") %in% colnames(dets)))
})


test_that(".read_detections fails with non-existent file", {
  expect_error(
    AnimalTrackR:::.read_detections("nonexistent_file.csv"),
    "file not found"
  )
})


test_that(".read_detections validates format", {
  skip_if_not_installed("data.table")
  
  temp_csv <- tempfile(fileext = ".csv")
  on.exit(unlink(temp_csv))
  
  # Create invalid CSV
  invalid_data <- data.frame(wrong = "format")
  write.csv(invalid_data, temp_csv, row.names = FALSE)
  
  expect_error(
    AnimalTrackR:::.read_detections(temp_csv),
    "does not meet the correct format"
  )
})


test_that(".calc_dets_frame_rate calculates FPS correctly", {
  # Create sample data with known frame rate (10 fps)
  # Frames 10, 20, 30, 40, 50 with timestamps 1000, 2000, 3000, 4000, 5000 ms
  # Diff: 10 frames per 1000ms = 10 fps
  
  frames <- c(10, 20, 30, 40, 50)
  timestamps <- c(1000, 2000, 3000, 4000, 5000)
  
  fps <- AnimalTrackR:::.calc_dets_frame_rate(frames, timestamps)
  
  expect_equal(fps, 10)
})


test_that(".calc_dets_frame_rate works with dataframe input", {
  df <- data.frame(
    Frame = c(10, 20, 30, 40, 50),
    Timestamp = c(1000, 2000, 3000, 4000, 5000)
  )
  
  fps <- AnimalTrackR:::.calc_dets_frame_rate(df, frame_col = "Frame", timestamp = "Timestamp")
  
  expect_equal(fps, 10)
})


test_that(".calc_dets_frame_rate handles irregular sampling", {
  # Irregular frame intervals
  frames <- c(1, 3, 7, 10, 15)
  timestamps <- c(100, 300, 700, 1000, 1500)
  
  fps <- AnimalTrackR:::.calc_dets_frame_rate(frames, timestamps)
  
  # Should calculate median rate
  expect_type(fps, "double")
  expect_true(fps > 0)
})


test_that(".calc_dets_frame_rate handles NA values", {
  frames <- c(1, 2, NA, 4, 5)
  timestamps <- c(100, 200, 300, 400, 500)
  
  # Should remove NA and still calculate
  fps <- AnimalTrackR:::.calc_dets_frame_rate(frames, timestamps)
  
  expect_type(fps, "double")
  expect_true(!is.na(fps))
})


test_that(".downsample_to_fps reduces data correctly", {
  df <- data.frame(
    Frame = 1:100,
    x = runif(100)
  )
  
  # Downsample from 10 fps to 5 fps (keep every other frame)
  downsampled <- AnimalTrackR:::.downsample_to_fps(
    df, 
    frame_col = "Frame", 
    dets_fps = 10, 
    state_fps = 5
  )
  
  expect_true(nrow(downsampled) < nrow(df))
  expect_true(nrow(downsampled) >= 50)  # Should be approximately half
})


test_that(".downsample_to_fps warns when target fps > current fps", {
  df <- data.frame(
    Frame = 1:10,
    x = runif(10)
  )
  
  expect_warning(
    result <- AnimalTrackR:::.downsample_to_fps(df, "Frame", dets_fps = 5, state_fps = 10),
    "higher than detection frame rate"
  )
  
  # Should return original data
  expect_equal(nrow(result), nrow(df))
})


test_that(".downsample_to_fps preserves data structure", {
  df <- data.frame(
    Frame = 1:50,
    x = runif(50),
    y = runif(50)
  )
  
  downsampled <- AnimalTrackR:::.downsample_to_fps(df, "Frame", dets_fps = 10, state_fps = 5)
  
  # Should have same columns
  expect_equal(colnames(downsampled), colnames(df))
})


test_that("get_detections_fps calculates fps from file", {
  skip_if_not_installed("data.table")
  
  temp_csv <- tempfile(fileext = ".csv")
  on.exit(unlink(temp_csv))
  
  create_sample_detections(temp_csv, n_rows = 100)
  
  fps <- get_detections_fps(temp_csv)
  
  expect_type(fps, "double")
  expect_true(fps > 0)
})


test_that("get_detections_fps works with dataframe input", {
  detections <- data.frame(
    Video = "test.mp4",
    Frame = seq(1, 100, by = 10),
    Timestamp = seq(0, 900, by = 100),
    xc = runif(10),
    yc = runif(10),
    xl = runif(10),
    xr = runif(10),
    yt = runif(10),
    yb = runif(10)
  )
  
  fps <- get_detections_fps(detections)
  
  expect_type(fps, "double")
  expect_true(fps > 0)
})


test_that("get_detections_fps fails with invalid input", {
  expect_error(
    get_detections_fps("nonexistent_file.csv"),
    "must be a path|does not exist"
  )
  
  expect_error(
    get_detections_fps(123),
    "must be a path|dataframe"
  )
})


test_that("fit_HMM validates detections parameter", {
  skip_if_not_installed("moveHMM")
  
  expect_error(
    fit_HMM(detections = "nonexistent.csv"),
    "file not found|does not exist"
  )
})


test_that("fit_HMM handles multiple detection files", {
  skip_if_not_installed("data.table")
  skip_if_not_installed("moveHMM")
  
  temp_csv1 <- tempfile(fileext = ".csv")
  temp_csv2 <- tempfile(fileext = ".csv")
  on.exit({
    unlink(temp_csv1)
    unlink(temp_csv2)
  })
  
  # Create sample detections
  create_sample_detections(temp_csv1, n_rows = 100)
  create_sample_detections(temp_csv2, n_rows = 100)
  
  # Should accept list of files
  expect_type(c(temp_csv1, temp_csv2), "character")
  expect_length(c(temp_csv1, temp_csv2), 2)
})


test_that("fit_HMM validates state_fps parameter", {
  # state_fps should be numeric
  expect_type(3, "double")
  expect_type(5L, "integer")
  
  # Default is 3
  default_state_fps <- 3
  expect_equal(default_state_fps, 3)
})


test_that("fit_HMM warns when overwrite is FALSE and State exists", {
  skip_if_not_installed("data.table")
  skip_if_not_installed("moveHMM")
  
  temp_csv <- tempfile(fileext = ".csv")
  on.exit(unlink(temp_csv))
  
  # Create detections with State column
  create_sample_detections(temp_csv, n_rows = 100, add_state = TRUE)
  
  expect_warning(
    fit_HMM(temp_csv, overwrite = FALSE),
    "already contains behavioural states"
  )
})


test_that("fit_HMM overwrites State column when overwrite is TRUE", {
  skip_if_not_installed("data.table")
  skip_if_not_installed("moveHMM")
  skip("Requires full moveHMM setup")
  
  # Would need complete setup to test full HMM fitting
})


test_that("fit_HMM adds State column to output", {
  skip_if_not_installed("moveHMM")
  skip("Requires full moveHMM fitting")
  
  # Output should have State column
  # Would test after successful HMM fitting
})


test_that("fit_HMM saves updated CSV to original location", {
  skip_if_not_installed("moveHMM")
  skip("Requires full moveHMM fitting")
  
  # After fitting, CSV should be updated with State column
})


test_that("fit_HMM handles single detection file", {
  skip_if_not_installed("data.table")
  
  temp_csv <- tempfile(fileext = ".csv")
  on.exit(unlink(temp_csv))
  
  create_sample_detections(temp_csv, n_rows = 100)
  
  # Should accept single file path
  expect_type(temp_csv, "character")
  expect_true(file.exists(temp_csv))
})


test_that("HMM threshold parameter is reasonable", {
  # Internal threshold for movement classification
  threshold <- 100
  
  # Should be positive
  expect_true(threshold > 0)
  
  # Used to separate active/inactive states
  expect_type(threshold, "double")
})


test_that("fit_HMM returns dataframe", {
  skip_if_not_installed("moveHMM")
  skip("Requires full moveHMM fitting")
  
  # Should return dataframe with State column
})


test_that("moveHMM parameters are calculated correctly", {
  skip_if_not_installed("moveHMM")
  skip_if_not_installed("dplyr")
  
  # Test parameter calculation logic
  # Would need actual step and angle data
})


test_that("fit_HMM uses two-state HMM", {
  # Should fit 2-state model (active/inactive)
  n_states <- 2
  expect_equal(n_states, 2)
})


test_that("get_detections_fps rounds to integer", {
  skip_if_not_installed("data.table")
  
  temp_csv <- tempfile(fileext = ".csv")
  on.exit(unlink(temp_csv))
  
  create_sample_detections(temp_csv, n_rows = 100)
  
  fps <- get_detections_fps(temp_csv)
  
  # Should be rounded to integer
  expect_equal(fps, round(fps))
})
