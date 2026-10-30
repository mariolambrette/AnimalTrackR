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


# ---- Grouped (by-video) splitting ------------------------------------------

# Helper: write a MakeSense-style CSV for a set of image names and create the
# matching dummy images in ToAnnotate
.make_session <- function(proj, image_names) {
  for (name in image_names) writeLines("dummy", file.path(proj, "ToAnnotate", name))
  csv_path <- tempfile(fileext = ".csv")
  n <- length(image_names)
  write.csv(data.frame(
    label_name = rep("Target", n), bbox_x = 100, bbox_y = 100,
    bbox_width = 50, bbox_height = 50, image_width = 640, image_height = 480,
    image_name = image_names
  ), csv_path, row.names = FALSE)
  csv_path
}

# Helper: which set each image ended up in
.allocated_sets <- function(proj) {
  do.call(rbind, lapply(c("Train", "Val", "Test"), function(s) {
    f <- list.files(file.path(proj, "YOLO", s, "images"))
    if (length(f) == 0) return(NULL)
    data.frame(image_name = f, set = s)
  }))
}

test_that(".image_groups parses extract_images() file names", {
  nm <- c("Group1_3_1200.jpg", "Treatment_A_12_55.jpg", "G2_4c1_900.jpg",
          "G2_4c2_10.jpg", "img1.jpg")
  expect_equal(
    AnimalTrackR:::.image_groups(nm),
    c("Group1_3", "Treatment_A_12", "G2_4", "G2_4", NA)
  )
})

test_that(".image_groups uses group_pattern when supplied", {
  nm <- c("01_B1-02-2023-11-02-151006-154006.mp4_0010000.jpg",
          "01_B1-13-2023-11-03-071002-074002.mp4_0000000.jpg")
  expect_equal(
    AnimalTrackR:::.image_groups(nm, "^[0-9]+_B1-[0-9]+"),
    c("01_B1-02", "01_B1-13")
  )
})

test_that("save_annotations keeps all frames from a video in one set", {
  temp_proj <- create_temp_project()
  on.exit(cleanup_temp_project(temp_proj))
  set_Project(temp_proj)

  imgs <- as.vector(outer(0:9, 1:12, function(v, f) paste0("Group1_", v, "_", f * 100, ".jpg")))
  csv_path <- .make_session(temp_proj, imgs)
  suppressMessages(save_annotations(csv_path, seed = 1))

  out <- .allocated_sets(temp_proj)
  out$group <- AnimalTrackR:::.image_groups(out$image_name)
  expect_equal(nrow(out), length(imgs))
  expect_true(all(tapply(out$set, out$group, function(x) length(unique(x))) == 1))
  expect_setequal(unique(out$set), c("Train", "Val", "Test"))
  # 10 equal-sized videos with a 60/20/20 split -> 6/2/2 videos
  expect_equal(as.vector(table(factor(out$set, c("Train", "Val", "Test")))), c(72, 24, 24))
  unlink(csv_path)
})

test_that("videos keep their set across annotation sessions", {
  temp_proj <- create_temp_project()
  on.exit(cleanup_temp_project(temp_proj))
  set_Project(temp_proj)

  s1 <- as.vector(outer(0:9, 1:5, function(v, f) paste0("Group1_", v, "_", f, ".jpg")))
  s2 <- as.vector(outer(0:9, 6:10, function(v, f) paste0("Group1_", v, "_", f, ".jpg")))
  csv1 <- .make_session(temp_proj, s1)
  suppressMessages(save_annotations(csv1, seed = 1))
  csv2 <- .make_session(temp_proj, s2)
  suppressMessages(save_annotations(csv2, seed = 99))

  out <- .allocated_sets(temp_proj)
  out$group <- AnimalTrackR:::.image_groups(out$image_name)
  expect_equal(nrow(out), 100)
  expect_true(all(tapply(out$set, out$group, function(x) length(unique(x))) == 1))
  unlink(c(csv1, csv2))
})

test_that("the same seed gives the same split", {
  imgs <- as.vector(outer(0:19, 1:5, function(v, f) paste0("Group1_", v, "_", f, ".jpg")))
  run <- function(seed) {
    temp_proj <- create_temp_project()
    on.exit(cleanup_temp_project(temp_proj))
    set_Project(temp_proj)
    csv <- .make_session(temp_proj, imgs)
    suppressMessages(save_annotations(csv, seed = seed))
    out <- .allocated_sets(temp_proj)
    out[order(out$image_name), ]
  }
  expect_identical(run(42), run(42))
})

test_that("setting a seed does not change the user's random number stream", {
  temp_proj <- create_temp_project()
  on.exit(cleanup_temp_project(temp_proj))
  set_Project(temp_proj)

  set.seed(123); expected <- runif(1)
  set.seed(123)
  suppressMessages(AnimalTrackR:::.assign_sets(
    paste0("Group1_", 0:9, "_1.jpg"), "video",
    c(Train = 0.6, Val = 0.2, Test = 0.2), seed = 5, project = temp_proj
  ))
  expect_equal(runif(1), expected)
})

test_that("split_by = 'image' still allocates images individually", {
  temp_proj <- create_temp_project()
  on.exit(cleanup_temp_project(temp_proj))
  set_Project(temp_proj)

  imgs <- paste0("Group1_0_", 1:60, ".jpg")
  csv_path <- .make_session(temp_proj, imgs)
  suppressMessages(save_annotations(csv_path, split_by = "image", seed = 1))
  out <- .allocated_sets(temp_proj)
  # One video, but images spread across sets
  expect_gt(length(unique(out$set)), 1)
  unlink(csv_path)
})

test_that("invalid split specifications are rejected", {
  expect_error(AnimalTrackR:::.check_split(c(Train = 0.5, Val = 0.2, Test = 0.2)), "sum to 1")
  expect_error(AnimalTrackR:::.check_split(c(0.6, 0.2, 0.2)), "named numeric")
})


# ---- import_annotations ---------------------------------------------------

test_that("import_annotations copies YOLO data and groups by pattern", {
  temp_proj <- create_temp_project()
  on.exit(cleanup_temp_project(temp_proj))
  set_Project(temp_proj)

  src <- tempfile("yolo_src_")
  dir.create(file.path(src, "images"), recursive = TRUE)
  dir.create(file.path(src, "labels"))
  tanks <- sprintf("01_B1-%02d", 2:13)
  imgs <- as.vector(outer(tanks, 1:4, function(t, f) paste0(t, "-2023-11-02.mp4_", sprintf("%07d", f), ".jpg")))
  for (i in imgs) {
    writeLines("dummy", file.path(src, "images", i))
    writeLines(c("0 0.5 0.5 0.1 0.1", "1 0.9 0.9 0.1 0.1"),
               file.path(src, "labels", sub("\\.jpg$", ".txt", i)))
  }
  # One image with no label
  writeLines("dummy", file.path(src, "images", "01_B1-02-unlabelled.mp4_0000099.jpg"))

  expect_message(
    sets <- import_annotations(file.path(src, "images"), file.path(src, "labels"),
                               group_pattern = "^[0-9]+_B1-[0-9]+", seed = 1),
    "no matching label"
  )

  out <- .allocated_sets(temp_proj)
  out$group <- substr(out$image_name, 1, 8)
  expect_equal(nrow(out), length(imgs))
  expect_true(all(tapply(out$set, out$group, function(x) length(unique(x))) == 1))
  # Originals untouched
  expect_true(all(file.exists(file.path(src, "images", imgs))))

  # Labels copied unchanged
  lab <- readLines(file.path(temp_proj, "YOLO", out$set[1], "labels",
                             sub("\\.jpg$", ".txt", out$image_name[1])))
  expect_equal(lab, c("0 0.5 0.5 0.1 0.1", "1 0.9 0.9 0.1 0.1"))
  unlink(src, recursive = TRUE)
})

test_that("import_annotations remaps classes with class_map", {
  temp_proj <- create_temp_project()
  on.exit(cleanup_temp_project(temp_proj))
  set_Project(temp_proj)

  src <- tempfile("yolo_src_")
  dir.create(file.path(src, "images"), recursive = TRUE)
  dir.create(file.path(src, "labels"))
  writeLines("dummy", file.path(src, "images", "Group1_0_1.jpg"))
  writeLines(c("3 0.5 0.5 0.1 0.1", "7 0.2 0.2 0.1 0.1"),
             file.path(src, "labels", "Group1_0_1.txt"))

  suppressMessages(import_annotations(
    file.path(src, "images"), file.path(src, "labels"),
    class_map = c("3" = 0), seed = 1
  ))
  out <- .allocated_sets(temp_proj)
  lab <- readLines(file.path(temp_proj, "YOLO", out$set[1], "labels", "Group1_0_1.txt"))
  expect_equal(lab, "0 0.5 0.5 0.1 0.1")
  unlink(src, recursive = TRUE)
})
