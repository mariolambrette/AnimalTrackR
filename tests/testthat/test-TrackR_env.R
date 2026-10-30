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

  expect_error(
    check_TrackR_env("nonexistent_env_12345"),
    "No conda environment"
  )
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


# test_that("create_TrackR_env handles existing environment", {
#   skip_if_not_installed("reticulate")
#   skip("Interactive test - requires user input")
#
#   # This function uses menu() which requires user interaction
#   # In a real test environment, we would mock this
# })


test_that("CUDA versions are parsed and validated", {
  v <- AnimalTrackR:::.parse_cuda_version(13.1)
  expect_equal(c(v$major, v$minor), c(13L, 1L))
  v <- AnimalTrackR:::.parse_cuda_version("12.8")
  expect_equal(c(v$major, v$minor), c(12L, 8L))
  v <- AnimalTrackR:::.parse_cuda_version(12)
  expect_equal(c(v$major, v$minor), c(12L, 0L))
  expect_error(AnimalTrackR:::.parse_cuda_version("invalid"), "nvidia-smi")
  expect_error(AnimalTrackR:::.parse_cuda_version(c(12.1, 12.4)), "nvidia-smi")
  expect_error(AnimalTrackR:::.parse_cuda_version(10.2), "too old")
})


test_that("PyTorch CUDA builds are tried newest first, never newer than the driver", {
  tags <- AnimalTrackR:::.cuda_tags(AnimalTrackR:::.parse_cuda_version(13.1))
  expect_equal(utils::head(tags, 4), c("cu131", "cu130", "cu129", "cu128"))
  expect_equal(utils::tail(tags, 1), "cu118")
  expect_false(any(c("cu132", "cu117") %in% tags))
  expect_equal(AnimalTrackR:::.cuda_tags(AnimalTrackR:::.parse_cuda_version(11.8)), "cu118")
})


# Simulated pip: records calls; `builds` are the index tags that have torch 2.15.0
.fake_pip <- function(builds, install_ok = TRUE) {
  calls <- list()
  fn <- function(python, args, echo = FALSE) {
    calls[[length(calls) + 1]] <<- args
    if (length(args) == 1) {  # version script
      return(list(status = 0L, output = c("2.15.0+cpu", "0.29.2+cpu")))
    }
    if (identical(args[2:4], c("pip", "index", "versions"))) {
      tag <- basename(args[length(args)])
      if (tag %in% builds) {
        return(list(status = 0L, output = c(
          paste0("torch (2.15.0+", tag, ")"),
          paste0("Available versions: 2.15.0+", tag, ", 2.14.0+", tag))))
      }
      return(list(status = 1L, output = "ERROR: No matching distribution found for torch"))
    }
    if (args[3] == "install") {
      ok <- install_ok || !("--index-url" %in% args)
      return(list(status = if (ok) 0L else 1L, output = ""))
    }
    list(status = 0L, output = "")
  }
  list(fn = fn, calls = function() calls)
}


test_that("configure_cuda installs the newest matching GPU build of the same PyTorch", {
  pip <- .fake_pip(builds = c("cu130", "cu128"))
  local_mocked_bindings(condaenv_exists = function(...) TRUE, .package = "reticulate")
  local_mocked_bindings(.run_py = pip$fn, .env_python = function(...) "python",
                        check_gpu = function(...) TRUE, .package = "AnimalTrackR")

  expect_true(suppressMessages(configure_cuda(cuda.version = 13.1)))

  calls <- pip$calls()
  install <- Filter(function(a) length(a) > 2 && a[3] == "install", calls)
  expect_length(install, 1)
  expect_true(all(c("torch==2.15.0", "torchvision==0.29.2") %in% install[[1]]))
  expect_true("https://download.pytorch.org/whl/cu130" %in% install[[1]])
  # Removed the CPU build first
  expect_true(any(vapply(calls, function(a) length(a) > 2 && a[3] == "uninstall", logical(1))))
})


test_that("configure_cuda leaves the environment unchanged if no GPU build exists", {
  pip <- .fake_pip(builds = character(0))
  local_mocked_bindings(condaenv_exists = function(...) TRUE, .package = "reticulate")
  local_mocked_bindings(.run_py = pip$fn, .env_python = function(...) "python",
                        check_gpu = function(...) FALSE, .package = "AnimalTrackR")

  expect_message(res <- configure_cuda(cuda.version = 12.1), "has not been changed")
  expect_false(res)
  changed <- vapply(pip$calls(), function(a) length(a) > 2 && a[3] %in% c("install", "uninstall"),
                    logical(1))
  expect_false(any(changed))
})


test_that("configure_cuda restores the original PyTorch if the GPU install fails", {
  pip <- .fake_pip(builds = "cu128", install_ok = FALSE)
  local_mocked_bindings(condaenv_exists = function(...) TRUE, .package = "reticulate")
  local_mocked_bindings(.run_py = pip$fn, .env_python = function(...) "python",
                        check_gpu = function(...) FALSE, .package = "AnimalTrackR")

  expect_message(res <- configure_cuda(cuda.version = 12.8), "restored")
  expect_false(res)
  installs <- Filter(function(a) length(a) > 2 && a[3] == "install", pip$calls())
  expect_length(installs, 2)
  expect_false("--index-url" %in% installs[[2]])  # restore from the standard index
  expect_true(all(c("torch==2.15.0", "torchvision==0.29.2") %in% installs[[2]]))
})


test_that(".run_py runs a real Python subprocess, including paths with spaces", {
  python <- Sys.which(c("python3", "python"))
  python <- python[nzchar(python)][1]
  skip_if(is.na(python), "No Python found on PATH")

  dir <- file.path(tempdir(), "folder with spaces")
  dir.create(dir, showWarnings = FALSE)
  script <- file.path(dir, "print version.py")
  writeLines("import sys; print('ok', sys.version_info[0])", script)
  on.exit(unlink(dir, recursive = TRUE))

  res <- AnimalTrackR:::.run_py(unname(python), script)
  expect_equal(res$status, 0L)
  expect_equal(res$output, "ok 3")
})


test_that("configure_cuda rejects invalid CUDA versions before changing anything", {
  expect_error(configure_cuda(cuda.version = "invalid"), "nvidia-smi")
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


# test_that("environment functions work without active conda", {
#   skip_if_not_installed("reticulate")
#
#   # Test behavior when no conda installation exists
#   # This is environment-specific and hard to test reliably
#   skip("Environment-specific test")
# })


# test_that("check_TrackR_env handles reticulate not loaded", {
#   # Test that functions handle missing reticulate gracefully
#   # Since reticulate is imported in NAMESPACE, this is tricky to test
#   skip("Requires mocking reticulate availability")
# })


# test_that("environment name validation", {
#   skip_if_not_installed("reticulate")
#
#   # Test that environment names are validated properly
#   # Empty string for check_TrackR_env checks current environment
#   result <- check_TrackR_env("")
#   expect_type(result, "logical")
#
#   # check_gpu with empty string should error
#   expect_error(
#     check_gpu(""),
#     "No conda environment found"
#   )
# })


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


test_that("every Python package imported by AnimalTrackR is installed and checked", {
  # Third-party modules imported anywhere in the package's Python code
  py_files <- list.files(system.file("python", package = "AnimalTrackR"),
                         pattern = "\\.py$", full.names = TRUE)
  imports <- unlist(lapply(py_files, function(f) {
    l <- readLines(f, warn = FALSE)
    l <- l[grepl("^(import|from) ", l)]
    sub("^(import|from) ([A-Za-z0-9_]+).*$", "\\2", l)
  }))
  std_lib <- c("os", "sys", "csv", "math", "random", "time", "platform")
  # Installed automatically as dependencies of ultralytics
  via_ultralytics <- c("numpy", "torch", "yaml")
  needed <- setdiff(unique(imports), c(std_lib, via_ultralytics))

  expect_setequal(needed, names(AnimalTrackR:::.trackr_py_modules()))
  expect_true(all(AnimalTrackR:::.trackr_py_modules() %in%
                    AnimalTrackR:::.trackr_py_packages()))
})
