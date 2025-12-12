# Test Helper Functions
# This file contains utility functions to support testing

#' Create a temporary TrackR project for testing
#'
#' @param add_images Logical. Should sample images be added to the project?
#' @param add_annotations Logical. Should sample annotations be added?
#' @return Path to temporary project directory
create_temp_project <- function(add_images = FALSE, add_annotations = FALSE) {
  # Create temporary directory
  temp_dir <- tempfile(pattern = "trackr_test_")
  dir.create(temp_dir)
  
  # Create project structure
  dir.create(file.path(temp_dir, "ToAnnotate"))
  dir.create(file.path(temp_dir, "YOLO"))
  dir.create(file.path(temp_dir, "YOLO", "Train"))
  dir.create(file.path(temp_dir, "YOLO", "Test"))
  dir.create(file.path(temp_dir, "YOLO", "Val"))
  dir.create(file.path(temp_dir, "YOLO", "configs"))
  dir.create(file.path(temp_dir, "YOLO", "models"))
  dir.create(file.path(temp_dir, "YOLO", "Train", "images"))
  dir.create(file.path(temp_dir, "YOLO", "Test", "images"))
  dir.create(file.path(temp_dir, "YOLO", "Val", "images"))
  dir.create(file.path(temp_dir, "YOLO", "Train", "labels"))
  dir.create(file.path(temp_dir, "YOLO", "Test", "labels"))
  dir.create(file.path(temp_dir, "YOLO", "Val", "labels"))
  
  # Create labels.txt
  writeLines(c("Target", "ZZZ", "Empty"), 
             file.path(temp_dir, "YOLO", "configs", "labels.txt"))
  
  # Copy training configuration file if available
  train_config <- system.file("config", "train_config.yaml", package = "AnimalTrackR")
  if (file.exists(train_config)) {
    file.copy(
      from = train_config,
      to = file.path(temp_dir, "YOLO", "configs", "train_config.yaml")
    )
  }
  
  # Add sample images if requested
  if (add_images) {
    create_dummy_images(temp_dir)
  }
  
  # Add sample annotations if requested
  if (add_annotations) {
    create_dummy_annotations(temp_dir)
  }
  
  return(temp_dir)
}


#' Create dummy images for testing
#'
#' Creates minimal image files in the ToAnnotate folder
#'
#' @param project_path Path to project directory
#' @param n_images Number of images to create
create_dummy_images <- function(project_path, n_images = 3) {
  # Create minimal 10x10 pixel images
  for (i in 1:n_images) {
    img_path <- file.path(project_path, "ToAnnotate", paste0("test_img_", i, ".jpg"))
    
    # Create a minimal valid JPEG (just write a file marker for now)
    # In real tests, we'd use a package like 'png' or 'jpeg' but we want to minimize dependencies
    writeLines(paste0("dummy_image_", i), img_path)
  }
}


#' Create dummy annotation data
#'
#' Creates sample YOLO format annotations
#'
#' @param project_path Path to project directory
#' @param set Which set to add to (Train, Test, or Val)
#' @param n_annotations Number of annotation files to create
create_dummy_annotations <- function(project_path, set = "Train", n_annotations = 5) {
  # Create dummy images
  for (i in 1:n_annotations) {
    img_name <- paste0("test_img_", i, ".jpg")
    img_path <- file.path(project_path, "YOLO", set, "images", img_name)
    writeLines(paste0("dummy_image_", i), img_path)
    
    # Create corresponding label file
    label_name <- paste0("test_img_", i, ".txt")
    label_path <- file.path(project_path, "YOLO", set, "labels", label_name)
    # YOLO format: class_id center_x center_y width height
    writeLines("0 0.5 0.5 0.3 0.3", label_path)
  }
}


#' Create a sample detections CSV file
#'
#' @param path Where to save the CSV
#' @param n_rows Number of detection rows
#' @param add_state Should a State column be included?
#' @return Path to created CSV
create_sample_detections <- function(path, n_rows = 100, add_state = FALSE) {
  # Create sample detection data
  detections <- data.frame(
    Video = rep("test_video.mp4", n_rows),
    Frame = seq(1, n_rows * 10, by = 10),
    Timestamp = seq(0, (n_rows - 1) * 100, by = 100),
    xc = runif(n_rows, 100, 500),
    yc = runif(n_rows, 100, 500),
    xl = runif(n_rows, 50, 300),
    xr = runif(n_rows, 150, 600),
    yt = runif(n_rows, 50, 300),
    yb = runif(n_rows, 150, 600)
  )
  
  if (add_state) {
    detections$State <- sample(1:2, n_rows, replace = TRUE)
  }
  
  write.csv(detections, path, row.names = FALSE)
  return(path)
}


#' Create a sample MakeSense annotation CSV
#'
#' @param path Where to save the CSV
#' @param image_names Vector of image names
#' @return Path to created CSV
create_sample_makesense_csv <- function(path, image_names = c("img1.jpg", "img2.jpg", "img3.jpg")) {
  n_images <- length(image_names)
  
  # Create sample MakeSense format data
  annotations <- data.frame(
    label_name = rep(c("Target", "Empty"), each = n_images),
    bbox_x = rep(c(10, 20, 15), 2),
    bbox_y = rep(c(10, 20, 15), 2),
    bbox_width = rep(c(100, 120, 110), 2),
    bbox_height = rep(c(100, 120, 110), 2),
    image_width = rep(640, n_images * 2),
    image_height = rep(480, n_images * 2),
    image_name = rep(image_names, 2)
  )
  
  write.csv(annotations, path, row.names = FALSE)
  return(path)
}


#' Create a minimal test video file path
#'
#' Note: This doesn't create an actual video, just returns a path
#' Tests using this should be skipped if actual video processing is needed
#'
#' @param dir Directory to place the video
#' @return Path to video file
create_dummy_video_path <- function(dir = tempdir()) {
  file.path(dir, "test_video.mp4")
}


#' Check if conda environment exists for tests
#'
#' @param envname Name of conda environment
#' @return Logical indicating if environment exists
has_test_conda_env <- function(envname = "animaltrackr") {
  if (!requireNamespace("reticulate", quietly = TRUE)) {
    return(FALSE)
  }
  
  tryCatch({
    reticulate::condaenv_exists(envname)
  }, error = function(e) {
    FALSE
  })
}


#' Skip test if conda environment not available
skip_if_no_conda_env <- function(envname = "animaltrackr") {
  if (!has_test_conda_env(envname)) {
    testthat::skip(paste0("Conda environment '", envname, "' not available"))
  }
}


#' Skip test if no GPU available
skip_if_no_gpu <- function() {
  if (!requireNamespace("reticulate", quietly = TRUE)) {
    testthat::skip("reticulate not available")
  }
  
  tryCatch({
    if (!check_gpu()) {
      testthat::skip("GPU not available")
    }
  }, error = function(e) {
    testthat::skip("Cannot check GPU availability")
  })
}


#' Clean up temporary project
#'
#' @param project_path Path to temporary project
cleanup_temp_project <- function(project_path) {
  if (dir.exists(project_path) && grepl("trackr_test_", project_path)) {
    unlink(project_path, recursive = TRUE, force = TRUE)
  }
}


# Classification Testing Helpers ------------------------------------------

#' Create sample movement data for classification tests
#'
#' @param n_obs Number of observations
#' @param n_states Number of distinct behavioral states to simulate
#' @param add_noise Logical. Should random noise be added?
#' @return data.frame with movement features
create_sample_movement_data <- function(n_obs = 200, n_states = 2, add_noise = TRUE) {
  # Create state assignments
  state_length <- n_obs %/% n_states
  states <- rep(1:n_states, each = state_length)[1:n_obs]
  
  # Initialize data frame
  data <- data.frame(
    Video = rep("test_video.mp4", n_obs),
    Frame = seq(1, n_obs * 10, by = 10),
    Timestamp = seq(0, (n_obs - 1) * 100, by = 100)
  )
  
  # Create distinct movement patterns for each state
  for (i in 1:n_states) {
    idx <- which(states == i)
    
    if (i == 1) {
      # Low activity state
      data$speed[idx] <- rnorm(length(idx), mean = 2, sd = 0.5)
      data$acceleration[idx] <- rnorm(length(idx), mean = 0, sd = 0.2)
      data$turning_angle[idx] <- rnorm(length(idx), mean = 0, sd = 15)
    } else {
      # High activity state
      data$speed[idx] <- rnorm(length(idx), mean = 8, sd = 1.5)
      data$acceleration[idx] <- rnorm(length(idx), mean = 0.5, sd = 0.5)
      data$turning_angle[idx] <- rnorm(length(idx), mean = 0, sd = 45)
    }
  }
  
  # Add additional movement features
  data$distance <- data$speed * 0.1  # Assuming 100ms between frames
  data$angular_velocity <- data$turning_angle / 0.1
  data$jerk <- c(0, diff(data$acceleration))
  
  # Add noise if requested
  if (add_noise) {
    data$speed <- pmax(0, data$speed + rnorm(n_obs, 0, 0.1))
    data$acceleration <- data$acceleration + rnorm(n_obs, 0, 0.05)
    data$turning_angle <- data$turning_angle + rnorm(n_obs, 0, 5)
  }
  
  return(data)
}


#' Create detections data suitable for feature calculation
#'
#' @param n_obs Number of observations
#' @param add_trajectory_id Logical. Should trajectory_id column be added?
#' @param video_col_name Name for the video column (default "Video")
#' @return data.frame with detection coordinates
create_sample_detections_data <- function(n_obs = 100, add_trajectory_id = FALSE, video_col_name = "Video") {
  # Create a realistic trajectory with varying speed
  t <- seq(0, 2 * pi, length.out = n_obs)
  
  # Add speed variation - alternate between slow and fast movement
  speed_mult <- ifelse(t < pi, 0.5, 2.0)  # Slow first half, fast second half
  
  detections <- data.frame(
    Frame = seq(1, n_obs * 10, by = 10),
    Timestamp = seq(0, (n_obs - 1) * 100, by = 100),
    xc = 320 + 100 * cos(t) * speed_mult + rnorm(n_obs, 0, 10),
    yc = 240 + 100 * sin(t) * speed_mult + rnorm(n_obs, 0, 10),
    xl = 0,
    xr = 640,
    yt = 0,
    yb = 480
  )
  
  # Add video column with specified name
  detections[[video_col_name]] <- rep("test_video.mp4", n_obs)
  
  if (add_trajectory_id) {
    detections$trajectory_id <- rep("track_1", n_obs)
  }
  
  return(detections)
}


#' Check if optional classification package is available
#'
#' @param package Package name (e.g., "mclust", "changepoint", "hmmTMB")
#' @return Logical
has_classification_package <- function(package) {
  requireNamespace(package, quietly = TRUE)
}


#' Skip test if classification package not available
#'
#' @param package Package name
skip_if_no_package <- function(package) {
  if (!has_classification_package(package)) {
    testthat::skip(paste0("Package '", package, "' not available"))
  }
}


#' Create minimal feature data frame for testing
#'
#' @return data.frame with movement features
create_minimal_features <- function() {
  data.frame(
    speed = c(1, 2, 3, 2, 1, 5, 6, 7, 6, 5),
    acceleration = c(0, 1, 1, -1, -1, 4, 1, 1, -1, -1),
    turning_angle = c(0, 10, 20, 10, 0, 30, 40, 50, 40, 30),
    distance = c(0.1, 0.2, 0.3, 0.2, 0.1, 0.5, 0.6, 0.7, 0.6, 0.5)
  )
}
