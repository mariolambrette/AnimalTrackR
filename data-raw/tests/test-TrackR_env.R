test_that("default env created and activated correctly", {

  expect_no_error(create_TrackR_env("testenv_trackr"))

  env_exists  <- reticulate::condaenv_exists("testenv_trackr")

  # Ensure py_discover_config() is not NULL before checking pythonhome
  py_config   <- reticulate::py_discover_config()
  env_active  <- !is.null(py_config$pythonhome) && basename(py_config$pythonhome) == "testenv_trackr"

  env_correct <- check_TrackR_env("testenv_trackr")
  gpu_enabled <- check_gpu("testenv_trackr")

  expect_true(env_exists)
  expect_true(env_active)
  expect_true(env_correct)

  reticulate::conda_remove("testenv_trackr")

  rstudioapi::restartSession()

})

test_that("default gpu env created and activated correctly", {

  # Check if CUDA 12.1 is available
  cuda_available <- system("nvidia-smi", intern = TRUE, ignore.stderr = TRUE)

  # Extract CUDA version from nvidia-smi output
  cuda_version <- as.numeric(sub(".*CUDA Version: ([0-9]+\\.[0-9]+).*", "\\1",
                                 paste(cuda_available, collapse = " ")))

  if (is.na(cuda_version) || cuda_version != 12.1) {
    skip("Skipping test: CUDA 12.1 is not available on this machine.")
  }

  # Ensure environment creation runs without error
  expect_no_error(create_TrackR_env("testenv_trackr", cuda.version = 12.1))
  env_exists  <- reticulate::condaenv_exists("testenv_trackr")

  # Ensure py_discover_config() is not NULL before checking pythonhome
  py_config   <- reticulate::py_discover_config()
  env_active  <- !is.null(py_config$pythonhome) && basename(py_config$pythonhome) == "testenv_trackr"

  env_correct <- check_TrackR_env("testenv_trackr")
  gpu_enabled <- check_gpu("testenv_trackr")

  expect_true(env_exists)
  expect_true(env_active)
  expect_true(env_correct)
  expect_true(gpu_enabled)

  reticulate::conda_remove("testenv_trackr")

  rstudioapi::restartSession()
})

test_that("configure_cuda() works on non-GPU enabled environments", {
  # Check if CUDA 12.1 is available
  cuda_available <- system("nvidia-smi", intern = TRUE, ignore.stderr = TRUE)

  # Extract CUDA version from nvidia-smi output
  cuda_version <- as.numeric(sub(".*CUDA Version: ([0-9]+\\.[0-9]+).*", "\\1",
                                 paste(cuda_available, collapse = " ")))

  if (is.na(cuda_version) || cuda_version != 12.1) {
    skip("Skipping test: CUDA 12.1 is not available on this machine.")
  }

  # Ensure environment creation runs without error
  expect_no_error(create_TrackR_env("testenv_trackr"))
  env_exists  <- reticulate::condaenv_exists("testenv_trackr")

  expect_false(check_gpu("testenv_trackr"))

  configure_cuda("testenv_trackr", cuda.version = 12.1)

  # Ensure py_discover_config() is not NULL before checking pythonhome
  py_config   <- reticulate::py_discover_config()
  env_active  <- !is.null(py_config$pythonhome) && basename(py_config$pythonhome) == "testenv_trackr"

  env_correct <- check_TrackR_env("testenv_trackr")
  gpu_enabled <- check_gpu("testenv_trackr")

  expect_true(env_exists)
  expect_true(env_active)
  expect_true(env_correct)
  expect_true(gpu_enabled)

  reticulate::conda_remove("testenv_trackr")

  rstudioapi::restartSession()
})
