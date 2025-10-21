# Tests for save_annotations.R
# Testing annotation saving and conversion functions

test_that("save_annotations reads and processes CSV correctly", {
  skip_if_not_installed("data.table")
  skip_if_not_installed("dplyr")
  
  temp_proj <- create_temp_project(add_images = TRUE)
  on.exit(cleanup_temp_project(temp_proj))
  
  set_Project(temp_proj)
  
  # Create sample images in ToAnnotate
  for (i in 1:3) {
    img_path <- file.path(temp_proj, "ToAnnotate", paste0("img", i, ".jpg"))
    writeLines(paste0("dummy_image_", i), img_path)
  }
  
  # Create MakeSense CSV
  csv_path <- tempfile(fileext = ".csv")
  create_sample_makesense_csv(
    csv_path, 
    image_names = c("img1.jpg", "img2.jpg", "img3.jpg")
  )
  
  # Save annotations
  result <- suppressMessages(save_annotations(csv_path))
  
  # Check return value
  expect_true(result)
  
  # Clean up
  unlink(csv_path)
})


test_that("save_annotations normalizes coordinates correctly", {
  skip_if_not_installed("data.table")
  skip_if_not_installed("dplyr")
  
  temp_proj <- create_temp_project()
  on.exit(cleanup_temp_project(temp_proj))
  
  set_Project(temp_proj)
  
  # Create a single test image
  img_path <- file.path(temp_proj, "ToAnnotate", "test.jpg")
  writeLines("dummy", img_path)
  
  # Create CSV with known coordinates
  csv_path <- tempfile(fileext = ".csv")
  annots <- data.frame(
    label_name = "Target",
    bbox_x = 100,
    bbox_y = 100,
    bbox_width = 200,
    bbox_height = 200,
    image_width = 640,
    image_height = 480,
    image_name = "test.jpg"
  )
  write.csv(annots, csv_path, row.names = FALSE)
  
  # Process annotations
  suppressMessages(save_annotations(csv_path))
  
  # Check that label file was created in one of the sets
  label_found <- FALSE
  for (set in c("Train", "Test", "Val")) {
    label_path <- file.path(temp_proj, "YOLO", set, "labels", "test.txt")
    if (file.exists(label_path)) {
      label_found <- TRUE
      
      # Read and check normalized coordinates
      coords <- readLines(label_path)
      coords_split <- strsplit(coords, " ")[[1]]
      
      # Check format: class_id center_x center_y width height
      expect_length(coords_split, 5)
      
      # Check class is 0 (Target)
      expect_equal(coords_split[1], "0")
      
      # Check coordinates are normalized (0-1)
      expect_true(all(as.numeric(coords_split[2:5]) >= 0))
      expect_true(all(as.numeric(coords_split[2:5]) <= 1))
      
      break
    }
  }
  
  expect_true(label_found, info = "Label file should be created in Train, Test, or Val")
  
  unlink(csv_path)
})


test_that("save_annotations distributes images across sets", {
  skip_if_not_installed("data.table")
  skip_if_not_installed("dplyr")
  skip_if_not_installed("plyr")
  
  temp_proj <- create_temp_project()
  on.exit(cleanup_temp_project(temp_proj))
  
  set_Project(temp_proj)
  
  # Create multiple images with single Target annotations
  n_images <- 30
  image_names <- paste0("img", 1:n_images, ".jpg")
  
  for (name in image_names) {
    writeLines("dummy", file.path(temp_proj, "ToAnnotate", name))
  }
  
  # Create annotations for all images (single Target per image)
  csv_path <- tempfile(fileext = ".csv")
  annots <- data.frame(
    label_name = rep("Target", n_images),
    bbox_x = rep(100, n_images),
    bbox_y = rep(100, n_images),
    bbox_width = rep(50, n_images),
    bbox_height = rep(50, n_images),
    image_width = rep(640, n_images),
    image_height = rep(480, n_images),
    image_name = image_names
  )
  write.csv(annots, csv_path, row.names = FALSE)
  
  # Save annotations
  suppressMessages(save_annotations(csv_path))
  
  # Count images in each set
  train_count <- length(list.files(file.path(temp_proj, "YOLO", "Train", "images")))
  test_count <- length(list.files(file.path(temp_proj, "YOLO", "Test", "images")))
  val_count <- length(list.files(file.path(temp_proj, "YOLO", "Val", "images")))
  
  # All images should be distributed
  expect_equal(train_count + test_count + val_count, n_images)
  
  # With 30 images, highly likely to have at least some in train
  expect_true(train_count > 0)
  
  unlink(csv_path)
})


test_that("save_annotations removes images from ToAnnotate folder", {
  skip_if_not_installed("data.table")
  skip_if_not_installed("dplyr")
  skip_if_not_installed("plyr")
  
  temp_proj <- create_temp_project()
  on.exit(cleanup_temp_project(temp_proj))
  
  set_Project(temp_proj)
  
  # Create test image
  img_name <- "test_remove.jpg"
  img_path <- file.path(temp_proj, "ToAnnotate", img_name)
  writeLines("dummy", img_path)
  
  # Verify image exists
  expect_true(file.exists(img_path))
  
  # Create and save annotations with single Target
  csv_path <- tempfile(fileext = ".csv")
  annots <- data.frame(
    label_name = "Target",
    bbox_x = 100,
    bbox_y = 100,
    bbox_width = 50,
    bbox_height = 50,
    image_width = 640,
    image_height = 480,
    image_name = img_name
  )
  write.csv(annots, csv_path, row.names = FALSE)
  suppressMessages(save_annotations(csv_path))
  
  # Image should be removed from ToAnnotate
  expect_false(file.exists(img_path))
  
  unlink(csv_path)
})


test_that("save_annotations skips images with multiple targets", {
  skip_if_not_installed("data.table")
  skip_if_not_installed("dplyr")
  
  temp_proj <- create_temp_project()
  on.exit(cleanup_temp_project(temp_proj))
  
  set_Project(temp_proj)
  
  # Create test image
  img_name <- "multi_target.jpg"
  writeLines("dummy", file.path(temp_proj, "ToAnnotate", img_name))
  
  # Create CSV with multiple Target annotations
  csv_path <- tempfile(fileext = ".csv")
  annots <- data.frame(
    label_name = c("Target", "Target"),
    bbox_x = c(100, 300),
    bbox_y = c(100, 300),
    bbox_width = c(50, 50),
    bbox_height = c(50, 50),
    image_width = c(640, 640),
    image_height = c(480, 480),
    image_name = c(img_name, img_name)
  )
  write.csv(annots, csv_path, row.names = FALSE)
  
  # Should skip with message
  expect_message(
    save_annotations(csv_path),
    "more than 1 target"
  )
  
  # Image should still exist in ToAnnotate
  expect_true(file.exists(file.path(temp_proj, "ToAnnotate", img_name)))
  
  unlink(csv_path)
})


test_that("save_annotations skips Empty with other labels", {
  skip_if_not_installed("data.table")
  skip_if_not_installed("dplyr")
  
  temp_proj <- create_temp_project()
  on.exit(cleanup_temp_project(temp_proj))
  
  set_Project(temp_proj)
  
  # Create test image
  img_name <- "empty_mixed.jpg"
  writeLines("dummy", file.path(temp_proj, "ToAnnotate", img_name))
  
  # Create CSV with Empty and another label
  csv_path <- tempfile(fileext = ".csv")
  annots <- data.frame(
    label_name = c("Empty", "Target"),
    bbox_x = c(100, 300),
    bbox_y = c(100, 300),
    bbox_width = c(50, 50),
    bbox_height = c(50, 50),
    image_width = c(640, 640),
    image_height = c(480, 480),
    image_name = c(img_name, img_name)
  )
  write.csv(annots, csv_path, row.names = FALSE)
  
  # Should skip with message
  expect_message(
    save_annotations(csv_path),
    "'Empty' label is not alone"
  )
  
  # Image should still exist in ToAnnotate
  expect_true(file.exists(file.path(temp_proj, "ToAnnotate", img_name)))
  
  unlink(csv_path)
})


test_that("save_annotations handles all three label types", {
  skip_if_not_installed("data.table")
  skip_if_not_installed("dplyr")
  
  temp_proj <- create_temp_project()
  on.exit(cleanup_temp_project(temp_proj))
  
  set_Project(temp_proj)
  
  # Create images for each label type
  for (label in c("Target", "ZZZ", "Empty")) {
    img_name <- paste0(label, ".jpg")
    writeLines("dummy", file.path(temp_proj, "ToAnnotate", img_name))
  }
  
  # Create annotations
  csv_path <- tempfile(fileext = ".csv")
  annots <- data.frame(
    label_name = c("Target", "ZZZ", "Empty"),
    bbox_x = c(100, 100, 100),
    bbox_y = c(100, 100, 100),
    bbox_width = c(50, 50, 50),
    bbox_height = c(50, 50, 50),
    image_width = c(640, 640, 640),
    image_height = c(480, 480, 480),
    image_name = c("Target.jpg", "ZZZ.jpg", "Empty.jpg")
  )
  write.csv(annots, csv_path, row.names = FALSE)
  
  # Process annotations
  suppressMessages(save_annotations(csv_path))
  
  # Check that all label files were created
  labels_found <- 0
  for (set in c("Train", "Test", "Val")) {
    labels_found <- labels_found + 
      length(list.files(file.path(temp_proj, "YOLO", set, "labels")))
  }
  
  expect_equal(labels_found, 3)
  
  unlink(csv_path)
})


test_that("save_annotations returns invisibly", {
  skip_if_not_installed("data.table")
  skip_if_not_installed("dplyr")
  
  temp_proj <- create_temp_project()
  on.exit(cleanup_temp_project(temp_proj))
  
  set_Project(temp_proj)
  
  # Create minimal test case
  img_name <- "test.jpg"
  writeLines("dummy", file.path(temp_proj, "ToAnnotate", img_name))
  
  csv_path <- tempfile(fileext = ".csv")
  create_sample_makesense_csv(csv_path, image_names = img_name)
  
  result <- suppressMessages(withVisible(save_annotations(csv_path)))
  
  expect_false(result$visible)
  expect_true(result$value)
  
  unlink(csv_path)
})


test_that(".save_image creates files in correct locations", {
  temp_proj <- create_temp_project()
  on.exit(cleanup_temp_project(temp_proj))
  
  set_Project(temp_proj)
  
  # Create test image in ToAnnotate
  img_name <- "test_save_image.jpg"
  img_path <- file.path(temp_proj, "ToAnnotate", img_name)
  writeLines("dummy", img_path)
  
  # Create annotation data
  annos <- data.frame(lines = "0 0.5 0.5 0.3 0.3")
  
  # Call internal function for Train set
  AnimalTrackR:::.save_image(img_name, "Train", annos)
  
  # Check image was copied
  expect_true(file.exists(
    file.path(temp_proj, "YOLO", "Train", "images", img_name)
  ))
  
  # Check label was created
  expect_true(file.exists(
    file.path(temp_proj, "YOLO", "Train", "labels", "test_save_image.txt")
  ))
  
  # Check original was removed
  expect_false(file.exists(img_path))
})
