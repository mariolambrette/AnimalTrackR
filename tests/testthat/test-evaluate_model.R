# Tests for evaluate_model.R

test_that("evaluate_Model validates active project", {
  current_proj <- get_Project()
  on.exit({
    if (!is.null(current_proj) && dir.exists(current_proj)) set_Project(current_proj)
  })
  trackr_env$proj <- NULL
  expect_error(evaluate_Model("m1"), "No active TrackR project")
})

test_that("evaluate_Model validates conf", {
  temp_proj <- create_temp_project()
  on.exit(cleanup_temp_project(temp_proj))
  set_Project(temp_proj)
  expect_error(evaluate_Model("m1", conf = 1.5), "between 0 and 1")
  expect_error(evaluate_Model("m1", conf = c(0.5, 0.6)), "between 0 and 1")
})

test_that("evaluate_Model errors when the model has no weights", {
  temp_proj <- create_temp_project()
  on.exit(cleanup_temp_project(temp_proj))
  set_Project(temp_proj)
  expect_error(evaluate_Model("not_a_model"), "No trained weights found")
})

test_that("evaluate_Model errors when there are no test images", {
  temp_proj <- create_temp_project()
  on.exit(cleanup_temp_project(temp_proj))
  set_Project(temp_proj)
  dir.create(file.path(temp_proj, "YOLO", "models", "m1", "weights"), recursive = TRUE)
  writeLines("dummy", file.path(temp_proj, "YOLO", "models", "m1", "weights", "best.pt"))
  expect_error(evaluate_Model("m1"), "no test images")
})

test_that(".split_summary counts images, groups and classes", {
  temp_proj <- create_temp_project()
  on.exit(cleanup_temp_project(temp_proj))

  add <- function(set, name, lines) {
    writeLines("dummy", file.path(temp_proj, "YOLO", set, "images", paste0(name, ".jpg")))
    writeLines(lines, file.path(temp_proj, "YOLO", set, "labels", paste0(name, ".txt")))
  }
  add("Train", "Group1_0_1", c("0 0.5 0.5 0.1 0.1", "1 0.9 0.9 0.1 0.1"))
  add("Train", "Group1_0_2", "0 0.5 0.5 0.1 0.1")
  add("Train", "Group1_1_1", "2 0.5 0.5 0.1 0.1")
  add("Test", "Group1_2_1", "0 0.5 0.5 0.1 0.1")

  s <- AnimalTrackR:::.split_summary(temp_proj)
  expect_equal(s$set, c("Train", "Val", "Test"))
  expect_equal(s$n_images, c(3, 0, 1))
  expect_equal(s$n_groups, c(2, 0, 1))
  expect_equal(s$n_target, c(2, 0, 1))
  expect_equal(s$n_zzz, c(1, 0, 0))
  expect_equal(s$n_empty, c(1, 0, 0))
})

test_that("deployment scoring rules are correct", {
  skip_if_not_installed("reticulate")
  skip_if_not(reticulate::py_module_available("ultralytics"),
              "ultralytics not available")
  ev <- reticulate::import_from_path(
    "evaluate_model_py", system.file("python", package = "AnimalTrackR")
  )

  row <- function(n_gt, conf, iou) {
    reticulate::dict(n_gt_target = n_gt, pred_conf = conf, iou = iou)
  }
  # Target present, confident well-placed detection -> TP
  expect_equal(unlist(ev$`_outcome`(row(1L, 0.9, 0.8), 0.75)), c(1, 0, 0))
  # Target present, confident misplaced detection -> FP and FN
  expect_equal(unlist(ev$`_outcome`(row(1L, 0.9, 0.2), 0.75)), c(0, 1, 1))
  # Target present, detection below threshold -> FN
  expect_equal(unlist(ev$`_outcome`(row(1L, 0.5, 0.8), 0.75)), c(0, 0, 1))
  # No target, confident detection -> FP
  expect_equal(unlist(ev$`_outcome`(row(0L, 0.9, 0), 0.75)), c(0, 1, 0))
  # No target, no detection -> nothing
  expect_equal(unlist(ev$`_outcome`(row(0L, NULL, 0), 0.75)), c(0, 0, 0))

  # Wilson interval contains the estimate and stays within [0, 1]
  w <- unlist(ev$`_wilson`(19L, 20L))
  expect_true(w[1] < 0.95 && w[2] > 0.95 && w[2] <= 1)
})
