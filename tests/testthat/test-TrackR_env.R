# Tests for TrackR_env.R
# Testing conda environment management functions

test_that("check_TrackR_env validates environment correctly", {
  skip_if_not_installed("reticulate")
  skip_if_no_conda_env()
  
  # Should return TRUE for properly configured environment
  result <- check_TrackR_env("animaltrackr")
  
  # Result should be logical
  expect_type(result, "logical")
})


test_that("check_TrackR_env returns FALSE for non-existent environment", {
  skip_if_not_installed("reticulate")
  
  # Save current environment
  current_env <- tryCatch({
    basename(reticulate::py_discover_config()$pythonhome)
  }, error = function(e) NULL)
  
  # Try to use a non-existent environment (won't actually activate it)
  # check_TrackR_env checks the currently active environment
  # So if animaltrackr is active and valid, it will return TRUE
  # This test is environment-dependent
  skip("Environment-dependent - depends on active Python environment")
})


test_that("check_TrackR_env checks for required modules", {
  skip_if_not_installed("reticulate")
  skip_if_no_conda_env()
  
  # The function checks for ultralytics and cv2
  # If environment is valid, it should have these
  if (check_TrackR_env("animaltrackr")) {
    expect_true(reticulate::py_module_available("ultralytics"))
    expect_true(reticulate::py_module_available("cv2"))
  }
})


test_that("check_gpu returns logical value", {
  skip_if_not_installed("reticulate")
  skip_if_no_conda_env()
  
  result <- check_gpu("animaltrackr")
  
  expect_type(result, "logical")
  expect_length(result, 1)
})


test_that("check_gpu fails with non-existent environment", {
  skip_if_not_installed("reticulate")
  
  expect_error(
    check_gpu("nonexistent_env_12345"),
    "No conda environment found"
  )
})


test_that("set_TrackR_env rejects invalid environment", {
  skip_if_not_installed("reticulate")
  
  expect_error(
    set_TrackR_env("nonexistent_env_12345"),
    "No conda environments with this name"
  )
})


test_that("set_TrackR_env returns invisibly", {
  skip_if_not_installed("reticulate")
  skip_if_no_conda_env()
  
  result <- withVisible(set_TrackR_env("animaltrackr"))
  
  expect_false(result$visible)
  expect_true(result$value)
})


test_that("set_TrackR_env validates environment requirements", {
  skip_if_not_installed("reticulate")
  
  # If we have a test environment that doesn't meet requirements
  # we should get an error
  # This test would need a specifically configured bad environment
  skip("Requires test environment with missing dependencies")
})


test_that("create_TrackR_env handles existing environment", {
  skip_if_not_installed("reticulate")
  skip("Interactive test - requires user input")
  
  # This function uses menu() which requires user interaction
  # In a real test environment, we would mock this
})


test_that("configure_cuda validates cuda version parameter", {
  skip_if_not_installed("reticulate")
  skip_if_no_conda_env()
  skip_if_no_gpu()
  
  # This would require a GPU environment
  # Test that the function accepts numeric and character versions
  expect_error(
    configure_cuda(cuda.version = "invalid"),
    NA  # Should not error with string
  )
  
  expect_error(
    configure_cuda(cuda.version = 12.1),
    NA  # Should not error with numeric
  )
})


test_that("environment check integration", {
  skip_if_not_installed("reticulate")
  skip_if_no_conda_env()
  
  # Test that we can check and set environment in sequence
  env_valid <- check_TrackR_env("animaltrackr")
  
  if (env_valid) {
    expect_silent(set_TrackR_env("animaltrackr"))
    
    # After setting, environment should still be valid
    expect_true(check_TrackR_env("animaltrackr"))
  }
})


test_that("check_TrackR_env checks Python version", {
  skip_if_not_installed("reticulate")
  skip_if_no_conda_env()
  
  # AnimalTrackR requires Python 3.11
  # The check function should verify this
  
  if (check_TrackR_env("animaltrackr")) {
    py_version <- as.numeric(substr(reticulate::py_config()$version, 1, 4))
    expect_equal(py_version, 3.11)
  }
})


test_that("GPU check handles missing pytorch", {
  skip_if_not_installed("reticulate")
  
  # If environment doesn't have pytorch, check_gpu should handle it
  # This is hard to test without a specific test environment
  skip("Requires test environment without pytorch")
})


test_that("environment functions work without active conda", {
  skip_if_not_installed("reticulate")
  
  # Test behavior when no conda installation exists
  # This is environment-specific and hard to test reliably
  skip("Environment-specific test")
})


test_that("check_TrackR_env handles reticulate not loaded", {
  # Test that functions handle missing reticulate gracefully
  # Since reticulate is imported in NAMESPACE, this is tricky to test
  skip("Requires mocking reticulate availability")
})


test_that("environment name validation", {
  skip_if_not_installed("reticulate")
  
  # Test that environment names are validated properly
  # Empty string for check_TrackR_env checks current environment
  result <- check_TrackR_env("")
  expect_type(result, "logical")
  
  # check_gpu with empty string should error
  expect_error(
    check_gpu(""),
    "No conda environment found"
  )
})


test_that("conda environment listing works", {
  skip_if_not_installed("reticulate")
  
  # Test that we can list conda environments
  envs <- tryCatch({
    reticulate::conda_list()
  }, error = function(e) {
    NULL
  })
  
  if (!is.null(envs)) {
    expect_true(is.data.frame(envs))
    expect_true("name" %in% colnames(envs))
    expect_true("python" %in% colnames(envs))
  }
})


test_that("Python module availability checks work", {
  skip_if_not_installed("reticulate")
  skip_if_no_conda_env()
  
  # Test basic module checking functionality
  # NumPy should be available in any Python environment
  numpy_available <- reticulate::py_module_available("numpy")
  expect_type(numpy_available, "logical")
})


test_that("environment functions preserve state", {
  skip_if_not_installed("reticulate")
  skip_if_no_conda_env()
  
  # Get current environment
  current_env <- tryCatch({
    basename(reticulate::py_discover_config()$pythonhome)
  }, error = function(e) {
    NULL
  })
  
  if (!is.null(current_env)) {
    # Set animaltrackr environment
    set_TrackR_env("animaltrackr")
    
    # Verify it's set
    new_env <- basename(reticulate::py_discover_config()$pythonhome)
    expect_equal(new_env, "animaltrackr")
  }
})
