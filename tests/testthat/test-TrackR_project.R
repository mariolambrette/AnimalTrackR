# Tests for TrackR_project.R
# Testing project management functions

test_that("init_Project creates valid project structure", {
  temp_dir <- tempfile(pattern = "test_init_")
  on.exit(cleanup_temp_project(temp_dir))
  
  # Initialize project
  suppressMessages(init_Project(temp_dir))
  
  # Check that main directory exists
  expect_true(dir.exists(temp_dir))
  
  # Check all expected subdirectories exist
  expected_dirs <- c(
    "ToAnnotate",
    file.path("YOLO", "Train", "images"),
    file.path("YOLO", "Train", "labels"),
    file.path("YOLO", "Test", "images"),
    file.path("YOLO", "Test", "labels"),
    file.path("YOLO", "Val", "images"),
    file.path("YOLO", "Val", "labels"),
    file.path("YOLO", "configs"),
    file.path("YOLO", "models")
  )
  
  for (dir in expected_dirs) {
    expect_true(dir.exists(file.path(temp_dir, dir)), 
                info = paste("Directory missing:", dir))
  }
  
  # Check that labels.txt was created
  expect_true(file.exists(file.path(temp_dir, "YOLO", "configs", "labels.txt")))
  
  # Check that train_config.yaml was created
  expect_true(file.exists(file.path(temp_dir, "YOLO", "configs", "train_config.yaml")))
})


test_that("init_Project sets new project as active", {
  # Save current project state
  current_proj <- get_Project()
  on.exit({
    if (!is.null(current_proj)) set_Project(current_proj)
  })
  
  temp_dir <- tempfile(pattern = "test_init_")
  on.exit(cleanup_temp_project(temp_dir), add = TRUE)
  
  suppressMessages(init_Project(temp_dir))
  
  # Check that the new project is now active
  expect_equal(normalizePath(get_Project(), winslash = "/"), 
               normalizePath(temp_dir, winslash = "/"))
})


test_that("init_Project increments name if directory exists", {
  base_dir <- tempfile(pattern = "test_init_base_")
  dir.create(base_dir)
  
  temp_dir <- file.path(base_dir, "Project")
  on.exit(unlink(base_dir, recursive = TRUE, force = TRUE))
  
  # Create first project
  suppressMessages(proj1 <- init_Project(temp_dir))
  
  # Create second project with same name
  suppressMessages(proj2 <- init_Project(temp_dir))
  
  # Check that second project has incremented name
  expect_true(dir.exists(paste0(temp_dir, "2")))
  # Normalize both paths consistently with forward slashes
  expect_equal(
    normalizePath(get_Project(), winslash = "/"),
    normalizePath(paste0(temp_dir, "2"), winslash = "/", mustWork = FALSE)
  )
})


test_that("init_Project returns invisibly", {
  temp_dir <- tempfile(pattern = "test_init_")
  on.exit(cleanup_temp_project(temp_dir))
  
  result <- suppressMessages(withVisible(init_Project(temp_dir)))
  
  expect_false(result$visible)
  expect_true(result$value)
})


test_that("set_Project activates valid project", {
  temp_proj <- create_temp_project()
  on.exit(cleanup_temp_project(temp_proj))
  
  # Set the project
  result <- set_Project(temp_proj)
  
  # Check project is set
  expect_equal(normalizePath(get_Project(), winslash = "/"), 
               normalizePath(temp_proj, winslash = "/"))
  expect_true(result)
})


test_that("set_Project rejects non-existent directory", {
  fake_path <- file.path(tempdir(), "nonexistent_project_12345")
  
  expect_error(
    set_Project(fake_path),
    "does not exist"
  )
})


test_that("set_Project rejects invalid project structure", {
  # Create directory without proper structure
  temp_dir <- tempfile(pattern = "invalid_proj_")
  dir.create(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE, force = TRUE))
  
  expect_error(
    set_Project(temp_dir),
    "does not meet the requirements"
  )
})


test_that("set_Project returns invisibly", {
  temp_proj <- create_temp_project()
  on.exit(cleanup_temp_project(temp_proj))
  
  result <- withVisible(set_Project(temp_proj))
  
  expect_false(result$visible)
  expect_true(result$value)
})


test_that("get_Project returns active project path", {
  temp_proj <- create_temp_project()
  on.exit(cleanup_temp_project(temp_proj))
  
  set_Project(temp_proj)
  
  result <- get_Project()
  
  expect_equal(normalizePath(result, winslash = "/"), 
               normalizePath(temp_proj, winslash = "/"))
})


test_that("get_Project returns NULL when no project set", {
  # Save current state
  current_proj <- get_Project()
  on.exit({
    if (!is.null(current_proj) && dir.exists(current_proj)) {
      set_Project(current_proj)
    }
  })
  
  # Clear project
  trackr_env$proj <- NULL
  
  expect_null(get_Project())
})


test_that(".check_Project validates complete project structure", {
  temp_proj <- create_temp_project()
  on.exit(cleanup_temp_project(temp_proj))
  
  # Valid project should pass
  expect_true(AnimalTrackR:::.check_Project(temp_proj))
})


test_that(".check_Project detects missing directories", {
  temp_proj <- create_temp_project()
  on.exit(cleanup_temp_project(temp_proj))
  
  # Remove a required directory
  unlink(file.path(temp_proj, "YOLO", "Train", "images"), recursive = TRUE)
  
  # Should fail validation
  expect_warning(
    result <- AnimalTrackR:::.check_Project(temp_proj),
    "missing"
  )
  expect_false(result)
})


test_that(".check_Project detects missing labels.txt", {
  temp_proj <- create_temp_project()
  on.exit(cleanup_temp_project(temp_proj))
  
  # Remove labels.txt
  unlink(file.path(temp_proj, "YOLO", "configs", "labels.txt"))
  
  # Should fail validation
  expect_warning(
    result <- AnimalTrackR:::.check_Project(temp_proj),
    "labels.txt"
  )
  expect_false(result)
})


test_that(".check_Project handles non-existent directory", {
  fake_path <- file.path(tempdir(), "nonexistent_12345")
  
  expect_warning(
    result <- AnimalTrackR:::.check_Project(fake_path),
    "does not exist"
  )
  expect_false(result)
})


test_that(".check_Project normalizes paths correctly", {
  temp_proj <- create_temp_project()
  on.exit(cleanup_temp_project(temp_proj))
  
  # Test with different path representations
  expect_true(AnimalTrackR:::.check_Project(temp_proj))
  expect_true(AnimalTrackR:::.check_Project(normalizePath(temp_proj)))
})


test_that("project workflow integration test", {
  # Test complete workflow: init -> set -> get -> check
  temp_dir <- tempfile(pattern = "workflow_test_")
  on.exit(cleanup_temp_project(temp_dir))
  
  # Initialize
  suppressMessages(init_Project(temp_dir))
  
  # Should be automatically set
  current_proj <- get_Project()
  expect_equal(normalizePath(current_proj, winslash = "/"), 
               normalizePath(temp_dir, winslash = "/"))
  
  # Should pass validation
  expect_true(AnimalTrackR:::.check_Project(current_proj))
  
  # Should be able to re-set
  expect_silent(set_Project(temp_dir))
})


test_that("multiple projects can be switched between", {
  temp_proj1 <- create_temp_project()
  temp_proj2 <- create_temp_project()
  on.exit({
    cleanup_temp_project(temp_proj1)
    cleanup_temp_project(temp_proj2)
  })
  
  # Set first project
  set_Project(temp_proj1)
  expect_equal(normalizePath(get_Project(), winslash = "/"), 
               normalizePath(temp_proj1, winslash = "/"))
  
  # Switch to second project
  set_Project(temp_proj2)
  expect_equal(normalizePath(get_Project(), winslash = "/"), 
               normalizePath(temp_proj2, winslash = "/"))
  
  # Switch back to first
  set_Project(temp_proj1)
  expect_equal(normalizePath(get_Project(), winslash = "/"), 
               normalizePath(temp_proj1, winslash = "/"))
})
