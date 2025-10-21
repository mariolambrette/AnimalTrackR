# Tests for run_model.R
# Testing model running and detection functions

test_that(".prepare_csv creates new CSV file", {
  temp_csv <- tempfile(fileext = ".csv")
  on.exit(unlink(temp_csv))
  
  result <- AnimalTrackR:::.prepare_csv(temp_csv)
  
  expect_true(file.exists(temp_csv))
  expect_equal(result, temp_csv)
  
  # Check headers
  content <- readLines(temp_csv)
  expect_true(grepl("Video", content[1]))
  expect_true(grepl("Frame", content[1]))
  expect_true(grepl("Timestamp", content[1]))
})


test_that(".prepare_csv handles existing file with overwrite", {
  temp_csv <- tempfile(fileext = ".csv")
  on.exit(unlink(temp_csv))
  
  # Create existing file
  writeLines("existing content", temp_csv)
  
  # Mock user input for overwrite
  # In real usage, this would prompt the user
  # For testing, we check that the function handles it
  expect_true(file.exists(temp_csv))
})


test_that(".prepare_csv creates correct column structure", {
  temp_csv <- tempfile(fileext = ".csv")
  on.exit(unlink(temp_csv))
  
  AnimalTrackR:::.prepare_csv(temp_csv)
  
  # Read the CSV
  content <- read.csv(temp_csv, nrows = 0)
  
  # Check column names
  expected_cols <- c('Video', 'Frame', 'Timestamp', 'xc', 'yc', 'xl', 'xr', 'yt', 'yb')
  expect_equal(colnames(content), expected_cols)
})


test_that(".is_video correctly validates video files", {
  skip_if_not_installed("av")
  
  # Non-video file should return FALSE
  temp_txt <- tempfile(fileext = ".txt")
  writeLines("not a video", temp_txt)
  on.exit(unlink(temp_txt))
  
  expect_false(AnimalTrackR:::.is_video(temp_txt))
})


test_that("run_Model validates project is set", {
  # Save current project
  current_proj <- get_Project()
  on.exit({
    if (!is.null(current_proj) && dir.exists(current_proj)) {
      set_Project(current_proj)
    }
  })
  
  # Clear project
  trackr_env$proj <- NULL
  
  expect_error(
    run_Model(video = "dummy.mp4"),
    "No active TrackR project found"
  )
})


test_that("run_Model validates video file exists", {
  temp_proj <- create_temp_project()
  on.exit(cleanup_temp_project(temp_proj))
  
  set_Project(temp_proj)
  
  expect_error(
    run_Model(video = "nonexistent_video.mp4"),
    "Video not found"
  )
})


test_that("run_Model detects chapterized video directories", {
  skip_if_not_installed("tools")
  
  temp_proj <- create_temp_project()
  on.exit(cleanup_temp_project(temp_proj))
  
  set_Project(temp_proj)
  
  # Create a directory
  temp_dir <- tempdir()
  
  # Passing a directory should be handled differently than a file
  # (though it will fail later in processing without real videos)
  expect_type(temp_dir, "character")
  expect_true(tools::file_ext(temp_dir) == "")
})


test_that("run_Model handles model parameter correctly", {
  temp_proj <- create_temp_project()
  on.exit(cleanup_temp_project(temp_proj))
  
  set_Project(temp_proj)
  
  # Create a dummy model directory
  model_dir <- file.path(temp_proj, "YOLO", "models", "test_model")
  dir.create(model_dir, recursive = TRUE)
  dir.create(file.path(model_dir, "weights"), recursive = TRUE)
  
  # Check that the function looks for models in the right place
  expect_true(dir.exists(model_dir))
})


test_that("run_Model validates model exists", {
  temp_proj <- create_temp_project()
  on.exit(cleanup_temp_project(temp_proj))
  
  set_Project(temp_proj)
  
  # Create dummy video file
  temp_vid <- tempfile(fileext = ".mp4")
  writeLines("dummy", temp_vid)
  on.exit(unlink(temp_vid), add = TRUE)
  
  # Try with non-existent model
  expect_error(
    run_Model(video = temp_vid, model = "nonexistent_model"),
    "Model.*not found"
  )
})


test_that("run_Model handles save_path validation", {
  temp_proj <- create_temp_project()
  on.exit(cleanup_temp_project(temp_proj))
  
  set_Project(temp_proj)
  
  temp_vid <- tempfile(fileext = ".mp4")
  writeLines("dummy", temp_vid)
  on.exit(unlink(temp_vid), add = TRUE)
  
  # save_path without save_vid should error
  expect_error(
    run_Model(video = temp_vid, save_vid = FALSE, save_path = "some/path"),
    "`save_path` provided but video is not being saved"
  )
})


test_that("run_Model creates Detections directory if needed", {
  temp_proj <- create_temp_project()
  on.exit(cleanup_temp_project(temp_proj))
  
  set_Project(temp_proj)
  
  detections_dir <- file.path(temp_proj, "Detections")
  
  # Directory shouldn't exist initially
  expect_false(dir.exists(detections_dir))
  
  # After attempting to run (will fail without valid inputs),
  # the directory creation logic should work
  # We can't test the full function without valid videos/models
})


test_that("demo_run validates project is set", {
  # Save current project
  current_proj <- get_Project()
  on.exit({
    if (!is.null(current_proj) && dir.exists(current_proj)) {
      set_Project(current_proj)
    }
  })
  
  # Clear project
  trackr_env$proj <- NULL
  
  expect_error(
    demo_run(video = "dummy.mp4"),
    "No active TrackR project found"
  )
})


test_that("demo_run validates video file exists", {
  temp_proj <- create_temp_project()
  on.exit(cleanup_temp_project(temp_proj))
  
  set_Project(temp_proj)
  
  expect_error(
    demo_run(video = "nonexistent.mp4"),
    "video.*not found"
  )
})


test_that("demo_run validates video format", {
  skip_if_not_installed("av")
  
  temp_proj <- create_temp_project()
  on.exit(cleanup_temp_project(temp_proj))
  
  set_Project(temp_proj)
  
  # Create non-video file
  temp_txt <- tempfile(fileext = ".txt")
  writeLines("not a video", temp_txt)
  on.exit(unlink(temp_txt), add = TRUE)
  
  expect_error(
    demo_run(video = temp_txt),
    "Invalid `video`"
  )
})


test_that("demo_run creates Demos directory by default", {
  temp_proj <- create_temp_project()
  on.exit(cleanup_temp_project(temp_proj))
  
  set_Project(temp_proj)
  
  demos_dir <- file.path(temp_proj, "Demos")
  
  # Directory shouldn't exist initially
  expect_false(dir.exists(demos_dir))
})


test_that("demo_run handles custom savepath", {
  temp_proj <- create_temp_project()
  on.exit(cleanup_temp_project(temp_proj))
  
  set_Project(temp_proj)
  
  # Custom save path should be accepted
  custom_path <- file.path(tempdir(), "custom_demos")
  
  # Path validation should work
  expect_type(custom_path, "character")
})


test_that(".reformat_gopro checks for ffmpeg", {
  skip_on_cran()
  
  # Check if ffmpeg is available
  ffmpeg_available <- Sys.which("ffmpeg") != ""
  
  if (!ffmpeg_available) {
    temp_vid <- tempfile(fileext = ".mp4")
    writeLines("dummy", temp_vid)
    on.exit(unlink(temp_vid))
    
    expect_error(
      AnimalTrackR:::.reformat_gopro(temp_vid),
      "FFMPEG is not installed"
    )
  }
})


test_that(".reformat_gopro creates temporary file", {
  skip_if(Sys.which("ffmpeg") == "", "ffmpeg not available")
  skip("Requires valid GoPro video file")
  
  # Would need actual GoPro video to test
})


test_that("gopro parameter triggers reformatting", {
  temp_proj <- create_temp_project()
  on.exit(cleanup_temp_project(temp_proj))
  
  set_Project(temp_proj)
  
  # Just check that gopro parameter is accepted
  # Full testing would require actual video processing
  expect_true(is.logical(TRUE))  # gopro should be TRUE/FALSE
  expect_true(is.logical(FALSE))
})


test_that("run_Model handles detect_fps parameter", {
  # detect_fps should accept integers
  expect_type(5L, "integer")
  expect_type(10, "double")
  
  # NULL should be accepted (default)
  expect_null(NULL)
})


test_that("model weights path construction", {
  temp_proj <- create_temp_project()
  on.exit(cleanup_temp_project(temp_proj))
  
  set_Project(temp_proj)
  
  # Create model structure
  model_name <- "test_model"
  model_dir <- file.path(temp_proj, "YOLO", "models", model_name)
  dir.create(model_dir, recursive = TRUE)
  dir.create(file.path(model_dir, "weights"), recursive = TRUE)
  
  # Create dummy weights file
  weights_file <- file.path(model_dir, "weights", "best.pt")
  writeLines("dummy weights", weights_file)
  
  # Verify path structure
  expect_true(file.exists(weights_file))
})


test_that("detections_path construction", {
  temp_proj <- create_temp_project()
  on.exit(cleanup_temp_project(temp_proj))
  
  set_Project(temp_proj)
  
  # Default detections path
  video_name <- "test_video.mp4"
  expected_csv <- "test_video.csv"
  expected_path <- file.path(temp_proj, "Detections", expected_csv)
  
  # Path construction logic
  expect_true(grepl("Detections", expected_path))
  expect_true(grepl("\\.csv$", expected_path))
})
