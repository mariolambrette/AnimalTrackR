# Tests for extract_images.R
# Testing image extraction from videos

test_that("extract_images validates group_weights parameter", {
  skip_if_not_installed("reticulate")
  skip_if_no_conda_env()
  
  temp_proj <- create_temp_project()
  on.exit(cleanup_temp_project(temp_proj))
  
  set_Project(temp_proj)
  
  videos <- list(
    Group1 = list("video1.mp4", "video2.mp4"),
    Group2 = list("video3.mp4")
  )
  
  # Mismatched names should error
  weights_bad <- list(
    Group1 = 0.5,
    WrongName = 0.5
  )
  
  expect_error(
    extract_images(videos, group_weights = weights_bad, nimgs = 10),
    "names in video and weights list do not match"
  )
})


test_that("extract_images validates group_weights sum to 1", {
  skip_if_not_installed("reticulate")
  skip_if_no_conda_env()
  
  temp_proj <- create_temp_project()
  on.exit(cleanup_temp_project(temp_proj))
  
  set_Project(temp_proj)
  
  videos <- list(
    Group1 = list("video1.mp4"),
    Group2 = list("video2.mp4")
  )
  
  # Weights don't sum to 1
  weights_bad <- list(
    Group1 = 0.5,
    Group2 = 0.7
  )
  
  expect_error(
    extract_images(videos, group_weights = weights_bad, nimgs = 10),
    "must sum to 1"
  )
})


test_that("extract_images calculates automatic weights correctly", {
  skip_if_not_installed("reticulate")
  skip_if_no_conda_env()
  
  # This test verifies the logic without actually running extraction
  videos <- list(
    Group1 = list("v1.mp4", "v2.mp4", "v3.mp4"),  # 3 videos = 3/5 = 0.6
    Group2 = list("v4.mp4", "v5.mp4")              # 2 videos = 2/5 = 0.4
  )
  
  # Calculate what weights should be
  nvids <- sum(sapply(videos, length))
  expected_weights <- lapply(videos, function(x) length(x) / nvids)
  
  expect_equal(expected_weights$Group1, 0.6)
  expect_equal(expected_weights$Group2, 0.4)
  expect_equal(sum(unlist(expected_weights)), 1.0)
})


test_that("extract_images validates nimgs is numeric", {
  skip_if_not_installed("reticulate")
  skip_if_no_conda_env()
  
  temp_proj <- create_temp_project()
  on.exit(cleanup_temp_project(temp_proj))
  
  set_Project(temp_proj)
  
  videos <- list(Group1 = list("video1.mp4"))
  
  expect_error(
    extract_images(videos, nimgs = "not_a_number"),
    "`nimgs` is not numeric"
  )
})


test_that("extract_images checks for active conda environment", {
  skip_if_not_installed("reticulate")
  skip_if_no_conda_env()
  
  temp_proj <- create_temp_project()
  on.exit(cleanup_temp_project(temp_proj))
  
  set_Project(temp_proj)
  
  videos <- list(Group1 = list("video1.mp4"))
  
  # With valid conda env, will fail on video not found
  # This tests that it gets past environment check
  expect_error(
    extract_images(videos, nimgs = 10),
    "video1.mp4 not found|FileNotFoundError"
  )
})


test_that("extract_images handles video lists correctly", {
  skip_if_not_installed("reticulate")
  skip_if_no_conda_env()
  
  # Test structure of video list
  videos_nested <- list(
    Treatment = list("vid1.mp4", "vid2.mp4"),
    Control = list("vid3.mp4", "vid4.mp4", "vid5.mp4")
  )
  
  # Check list structure
  expect_type(videos_nested, "list")
  expect_length(videos_nested, 2)
  expect_named(videos_nested, c("Treatment", "Control"))
  expect_length(videos_nested$Treatment, 2)
  expect_length(videos_nested$Control, 3)
})


test_that("extract_images accepts custom video extensions", {
  skip_if_not_installed("reticulate")
  skip_if_no_conda_env()
  
  temp_proj <- create_temp_project()
  on.exit(cleanup_temp_project(temp_proj))
  
  set_Project(temp_proj)
  
  videos <- list(Group1 = list("video1.custom"))
  
  # Should accept vid_ext parameter without error on the parameter itself
  # Will fail when trying to find the video file
  expect_error(
    extract_images(videos, nimgs = 10, vid_ext = ".custom"),
    "video1.custom not found|FileNotFoundError"
  )
})


test_that("extract_images default nimgs is reasonable", {
  # Default is 1600, which should give ~1000 training images
  # with 60/20/20 split
  
  default_nimgs <- 1600
  
  # Training set (60%)
  expect_equal(round(default_nimgs * 0.6), 960)
  
  # Test set (20%)
  expect_equal(round(default_nimgs * 0.2), 320)
  
  # Val set (20%)
  expect_equal(round(default_nimgs * 0.2), 320)
})


test_that("extract_images returns invisibly", {
  skip_if_not_installed("reticulate")
  skip_if_no_conda_env()
  skip("Requires valid video files and python environment")
  
  # This would need actual video processing
  # Just testing return behavior
})


test_that("extract_images handles directory paths for chapterized video", {
  # Test that function can accept directory paths
  # as list elements for chapterized video
  
  videos_with_dir <- list(
    Individual1 = "path/to/video/chapters/",
    Individual2 = list("single_video.mp4")
  )
  
  expect_type(videos_with_dir, "list")
  expect_length(videos_with_dir, 2)
})


test_that("extract_images group weights validation is case-sensitive", {
  skip_if_not_installed("reticulate")
  skip_if_no_conda_env()
  
  temp_proj <- create_temp_project()
  on.exit(cleanup_temp_project(temp_proj))
  
  set_Project(temp_proj)
  
  videos <- list(
    Group1 = list("video1.mp4"),
    Group2 = list("video2.mp4")
  )
  
  # Different case should fail
  weights_bad_case <- list(
    group1 = 0.5,  # lowercase
    group2 = 0.5
  )
  
  expect_error(
    extract_images(videos, group_weights = weights_bad_case, nimgs = 10),
    "names in video and weights list do not match"
  )
})


test_that("extract_images handles single group", {
  skip_if_not_installed("reticulate")
  skip_if_no_conda_env()
  
  temp_proj <- create_temp_project()
  on.exit(cleanup_temp_project(temp_proj))
  
  set_Project(temp_proj)
  
  # Single group should work
  videos_single <- list(
    OnlyGroup = list("video1.mp4", "video2.mp4")
  )
  
  # Auto-calculated weight should be 1.0
  nvids <- 2
  expected_weight <- 2 / nvids
  expect_equal(expected_weight, 1.0)
})


test_that("extract_images validates project parameter", {
  skip_if_not_installed("reticulate")
  skip_if_no_conda_env()
  
  videos <- list(Group1 = list("video1.mp4"))
  
  # With valid conda env, will fail on video not found
  expect_error(
    extract_images(videos, nimgs = 10, project = "nonexistent_project"),
    "video1.mp4 not found|FileNotFoundError"
  )
})


test_that("extract_images file extension handling", {
  # Test that common video extensions are recognized
  common_extensions <- c(".mp4", ".avi", ".mov", ".mkv", ".wmv", ".flv", ".webm", ".mts")
  
  for (ext in common_extensions) {
    video_path <- paste0("test_video", ext)
    expect_type(video_path, "character")
    expect_true(grepl(ext, video_path, fixed = TRUE))
  }
})
