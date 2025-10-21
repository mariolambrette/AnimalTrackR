# Tests for utils.R
# Testing internal utility functions

test_that(".is_video correctly identifies video files", {
  skip_if_not_installed("av")
  
  # Create a temporary non-video file
  temp_txt <- tempfile(fileext = ".txt")
  writeLines("This is not a video", temp_txt)
  
  # Test non-video file
  expect_false(AnimalTrackR:::.is_video(temp_txt))
  
  # Test non-existent file
  expect_false(AnimalTrackR:::.is_video("nonexistent_file.mp4"))
  
  # Clean up
  unlink(temp_txt)
})


test_that(".is_video handles errors gracefully", {
  # Test with NULL
  expect_false(AnimalTrackR:::.is_video(NULL))
  
  # Test with invalid path
  expect_false(AnimalTrackR:::.is_video(""))
})


test_that(".create_YOLO_config creates valid configuration file", {
  # Create temp project
  temp_proj <- create_temp_project()
  on.exit(cleanup_temp_project(temp_proj))
  
  # Set the project
  set_Project(temp_proj)
  
  # Create config
  config_path <- AnimalTrackR:::.create_YOLO_config(temp_proj)
  
  # Check file was created
  expect_true(file.exists(config_path))
  
  # Read the config
  config_content <- readLines(config_path)
  
  # Check it contains expected fields
  expect_true(any(grepl("path:", config_content)))
  expect_true(any(grepl("train:", config_content)))
  expect_true(any(grepl("val:", config_content)))
  expect_true(any(grepl("test:", config_content)))
  expect_true(any(grepl("names:", config_content)))
  
  # Check the path is correct (YAML might use backslashes on Windows)
  yolo_path <- file.path(temp_proj, "YOLO")
  # Check if path appears in any form in the config
  expect_true(any(grepl(basename(yolo_path), config_content)) || 
              any(grepl("YOLO", config_content)))
})


test_that(".create_YOLO_config fails without active project", {
  # Save current project
  current_proj <- get_Project()
  on.exit({
    if (!is.null(current_proj) && dir.exists(current_proj)) {
      set_Project(current_proj)
    }
  })
  
  # Clear project
  trackr_env$proj <- NULL
  
  # Should throw an error
  expect_error(
    AnimalTrackR:::.create_YOLO_config(),
    "No active TrackR project found"
  )
})


test_that(".create_YOLO_config generates valid YAML structure", {
  skip_if_not_installed("yaml")
  
  temp_proj <- create_temp_project()
  on.exit(cleanup_temp_project(temp_proj))
  
  set_Project(temp_proj)
  config_path <- AnimalTrackR:::.create_YOLO_config(temp_proj)
  
  # Read and parse YAML
  config <- yaml::read_yaml(config_path)
  
  # Check structure
  expect_type(config, "list")
  expect_true("path" %in% names(config))
  expect_true("train" %in% names(config))
  expect_true("val" %in% names(config))
  expect_true("test" %in% names(config))
  expect_true("names" %in% names(config))
  
  # Check names are numeric keys (as strings in YAML)
  expect_true(all(c("0", "1", "2") %in% names(config$names)))
  expect_equal(config$names[["0"]], "Target")
  expect_equal(config$names[["1"]], "ZZZ")
  expect_equal(config$names[["2"]], "Empty")
})


test_that("restore_train_config restores default configuration", {
  temp_proj <- create_temp_project()
  on.exit(cleanup_temp_project(temp_proj))
  
  set_Project(temp_proj)
  
  config_path <- file.path(temp_proj, "YOLO", "configs", "train_config.yaml")
  
  # Remove config if it exists
  if (file.exists(config_path)) {
    unlink(config_path)
  }
  
  # Restore config
  result <- restore_train_config(temp_proj)
  
  # Check file was created
  expect_true(file.exists(config_path))
  
  # Check return value
  expect_true(result)
})


test_that("restore_train_config overwrites existing config", {
  temp_proj <- create_temp_project()
  on.exit(cleanup_temp_project(temp_proj))
  
  set_Project(temp_proj)
  
  config_path <- file.path(temp_proj, "YOLO", "configs", "train_config.yaml")
  
  # Create a dummy config
  writeLines("dummy: config", config_path)
  original_mtime <- file.info(config_path)$mtime
  
  # Wait a moment to ensure different timestamp
  Sys.sleep(0.1)
  
  # Restore config
  restore_train_config(temp_proj)
  
  # Check file was modified
  new_mtime <- file.info(config_path)$mtime
  expect_true(new_mtime > original_mtime)
  
  # Check content is not the dummy content
  content <- readLines(config_path)
  expect_false(any(grepl("dummy: config", content)))
})


test_that("restore_train_config uses active project by default", {
  temp_proj <- create_temp_project()
  on.exit(cleanup_temp_project(temp_proj))
  
  set_Project(temp_proj)
  
  # Call without specifying project
  result <- restore_train_config()
  
  config_path <- file.path(temp_proj, "YOLO", "configs", "train_config.yaml")
  expect_true(file.exists(config_path))
})


test_that("restore_train_config returns invisibly", {
  temp_proj <- create_temp_project()
  on.exit(cleanup_temp_project(temp_proj))
  
  set_Project(temp_proj)
  
  # Check that return is invisible
  result <- withVisible(restore_train_config(temp_proj))
  expect_false(result$visible)
  expect_true(result$value)
})
