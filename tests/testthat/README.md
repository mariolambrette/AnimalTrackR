# AnimalTrackR Test Suite

This directory contains comprehensive unit tests for the AnimalTrackR package. The test suite covers all major functions and their internal helper functions.

## Test Organization

Tests are organized by source file:

- **`helper-test_utils.R`**: Helper functions for creating test fixtures, temporary projects, sample data, and managing test environments
- **`test-utils.R`**: Tests for `utils.R` (`.is_video()`, `.create_YOLO_config()`, `restore_train_config()`)
- **`test-TrackR_project.R`**: Tests for `TrackR_project.R` (`init_Project()`, `set_Project()`, `get_Project()`, `.check_Project()`)
- **`test-TrackR_env.R`**: Tests for `TrackR_env.R` (conda environment management functions)
- **`test-save_annotations.R`**: Tests for `save_annotations.R` (annotation processing and YOLO format conversion)
- **`test-extract_images.R`**: Tests for `extract_images.R` (video frame extraction logic)
- **`test-run_model.R`**: Tests for `run_model.R` (`run_Model()`, `demo_run()`, helper functions)
- **`test-train_model.R`**: Tests for `train_model.R` (model training validation)
- **`test-fit_HMM.R`**: Tests for `fit_HMM.R` (HMM fitting and detection processing)
- **`test-BehaviourVis.R`**: Tests for `BehaviourVis.R` (behaviour visualization)

## Running Tests

### Run all tests
```r
devtools::test()
```

### Run specific test file
```r
testthat::test_file("tests/testthat/test-utils.R")
```

### Run tests with coverage
```r
covr::package_coverage()
```

## Test Design Philosophy

The test suite is designed with the following principles:

1. **Minimal External Dependencies**: Tests use mock data and temporary files to minimize reliance on large external files (videos, images, trained models)

2. **Conditional Skipping**: Tests that require specific environments (conda, GPU, actual video files) are skipped gracefully when those resources aren't available

3. **Isolated Testing**: Each test creates its own temporary project/data and cleans up afterward to prevent test interference

4. **Focus on Logic**: Tests focus on validation logic, parameter handling, and error checking rather than full end-to-end workflows that require heavy resources

5. **No R/ File Editing**: Tests are designed to validate existing functionality without modifying source files

## Test Fixtures

The `helper-test_utils.R` file provides utilities for:

- **`create_temp_project()`**: Creates a temporary TrackR project structure
- **`create_dummy_images()`**: Creates minimal test image files
- **`create_dummy_annotations()`**: Creates YOLO format annotation files
- **`create_sample_detections()`**: Creates detection CSV files
- **`create_sample_makesense_csv()`**: Creates MakeSense annotation CSV files
- **`skip_if_no_conda_env()`**: Skips tests when conda environment unavailable
- **`skip_if_no_gpu()`**: Skips tests when GPU unavailable
- **`cleanup_temp_project()`**: Safely removes temporary test projects

## Common Test Patterns

### Testing with Temporary Projects
```r
test_that("function works with project", {
  temp_proj <- create_temp_project()
  on.exit(cleanup_temp_project(temp_proj))
  
  set_Project(temp_proj)
  
  # Your test here
})
```

### Testing with Mock Data
```r
test_that("function processes detections", {
  temp_csv <- tempfile(fileext = ".csv")
  on.exit(unlink(temp_csv))
  
  create_sample_detections(temp_csv, n_rows = 100)
  
  # Your test here
})
```

### Conditional Testing
```r
test_that("function uses conda environment", {
  skip_if_not_installed("reticulate")
  skip_if_no_conda_env()
  
  # Test that requires conda environment
})
```

## Notes on Test Coverage

Some functions cannot be fully tested without:
- Actual video files (large binary files)
- Trained YOLO models (large model weights)
- GPU hardware
- Interactive input (menu selections)
- Full Python/conda environment setup

In these cases, tests focus on:
- Parameter validation
- Error handling
- Path construction
- Data format checking
- Logic components that can be isolated

## Continuous Integration

When setting up CI/CD:
- Install test dependencies: `testthat`, `covr`, `data.table`, `dplyr`, `yaml`, `av`, `moveHMM`
- Some tests will be skipped automatically if conda/GPU not available
- Consider separate test suites for quick checks vs. full integration tests

## Contributing Tests

When adding new tests:
1. Use existing helper functions where possible
2. Clean up temporary files/projects with `on.exit()`
3. Use `skip_if_not_installed()` for optional dependencies
4. Use `skip_if_no_conda_env()` for conda-dependent tests
5. Focus tests on the specific function behavior, not external dependencies
6. Add descriptive test names that explain what is being tested
