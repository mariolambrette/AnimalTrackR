#' Evaluate a trained detection model on the test set
#'
#' @description
#' `evaluate_Model()` tests a trained model on the images in the project's
#' `YOLO/Test` folder, which are not used at any point during training. It is
#' run automatically at the end of [train_Model()], so most users will not need
#' to call it directly. It is useful for re-evaluating a model, for example at
#' a different confidence threshold, or for models trained with an earlier
#' version of AnimalTrackR.
#'
#' @details
#' **Two kinds of evaluation**
#'
#' *Standard detection metrics* (precision, recall, F1, mAP@0.5 and
#' mAP@0.5:0.95 for each class) are calculated by Ultralytics. These are the
#' figures usually reported for object detection models and allow comparison
#' with other studies. Ultralytics reports precision and recall at the
#' confidence threshold that maximises F1.
#'
#' *Deployment metrics* mirror how [run_Model()] uses the model to track an
#' animal: on each image only the single most confident `Target` detection is
#' kept, and it is used only if its confidence is at least `conf`. A detection
#' is a true positive (TP) if it overlaps the annotated box with an
#' intersection-over-union of at least 0.5; a detection on an image with no
#' annotated target, or that does not overlap the annotated target, is a false
#' positive (FP); a missed or misplaced target is a false negative (FN).
#' Precision, recall and F1 are reported with two 95% confidence intervals:
#' a Wilson interval, which treats images as independent, and a bootstrap
#' interval that resamples whole videos (or other groups, see `group_pattern`),
#' which allows for images from the same video being similar. For true
#' positives, the distance between the centres of the predicted and annotated
#' boxes is reported in pixels and relative to the length of the annotated box
#' (its longer side), as a measure of how accurately the animal's position is
#' located.
#'
#' **Outputs**
#'
#' All outputs are written to `YOLO/models/<model_name>/evaluation/`:
#'
#' | File | Contents |
#' |---|---|
#' | `test_metrics.csv` | Standard detection metrics, overall and per class |
#' | `deployment_metrics.csv` | Deployment metrics with confidence intervals, box-centre error and the confidence threshold giving the highest F1 |
#' | `threshold_sweep.csv` | Deployment precision, recall and F1 across confidence thresholds from 0.05 to 0.95 |
#' | `per_group_metrics.csv` | Deployment metrics for each video (or other group) in the test set |
#' | `test_predictions.csv` | Image-by-image results (annotated and predicted boxes, confidence, IoU, outcome), useful for finding and inspecting errors |
#' | `split_summary.csv` | Number of images, videos and annotations of each class in the training, validation and test sets |
#' | `model_info.yaml` | Training settings, epochs run, best epoch, training time, inference speed, hardware and software versions |
#' | `plots/` | Precision-recall, F1, precision and recall curves and confusion matrices from Ultralytics |
#'
#' @param model_name Name of the model to evaluate, as passed to [train_Model()].
#' @param conf Confidence threshold used when deploying the model with
#'   [run_Model()]. Default 0.75.
#' @param group_pattern Optional regular expression identifying the video (or
#'   other group) each test image came from, used for the per-group results and
#'   bootstrap confidence intervals. Not needed for images extracted with
#'   [extract_images()]. See "Choosing a `group_pattern`" in
#'   [save_annotations()].
#' @param n_boot Number of bootstrap resamples for confidence intervals.
#' @param seed Random seed for the bootstrap.
#' @param training_time Optional training time in hours, recorded in
#'   `model_info.yaml`. Supplied automatically by [train_Model()].
#' @param project The path to the TrackR project directory. By default this is
#'  supplied by `get_Project()`.
#'
#' @return Invisibly, a list of data frames: `test_metrics`,
#'   `deployment_metrics`, `threshold_sweep`, `per_group_metrics`,
#'   `split_summary`, and `model_info` (a list).
#' @export
#'
#' @examples
#' \dontrun{
#' # Normally run automatically by train_Model(), but can be re-run:
#' evaluate_Model("model1")
#'
#' # Re-evaluate at a lower deployment confidence threshold
#' evaluate_Model("model1", conf = 0.5)
#' }
evaluate_Model <- function(model_name,
                           conf = 0.75,
                           group_pattern = NULL,
                           n_boot = 1000,
                           seed = 0,
                           training_time = NULL,
                           project = get_Project()) {

  if (is.null(project)) {
    stop("No active TrackR project. Activate a project using `set_Project()`.")
  }
  if (!is.numeric(conf) || length(conf) != 1 || conf <= 0 || conf >= 1) {
    stop("`conf` must be a single number between 0 and 1.")
  }

  model_dir <- file.path(project, "YOLO", "models", model_name)
  weights <- file.path(model_dir, "weights", "best.pt")
  if (!file.exists(weights)) {
    stop("No trained weights found for model '", model_name, "' (expected ",
         weights, ").")
  }

  test_images <- list.files(file.path(project, "YOLO", "Test", "images"),
                            pattern = "\\.(jpe?g|png)$", ignore.case = TRUE)
  if (length(test_images) == 0) {
    stop("There are no test images in the project, so the model cannot be evaluated.")
  }

  eval_dir <- file.path(model_dir, "evaluation")
  dir.create(eval_dir, showWarnings = FALSE, recursive = TRUE)

  # Split summary
  split_summary <- .split_summary(project, group_pattern)
  utils::write.csv(split_summary, file.path(eval_dir, "split_summary.csv"),
                   row.names = FALSE)

  groups <- .image_groups(test_images, group_pattern)
  groups[is.na(groups)] <- test_images[is.na(groups)]

  sys_info <- py_evaluate_model$system_info()
  gpu <- isTRUE(sys_info$cuda_available)

  cat("Evaluating '", model_name, "' on ", length(test_images),
      " test images from ", length(unique(groups)), " videos/groups...\n", sep = "")

  res <- py_evaluate_model$evaluate(
    weights     = weights,
    data_config = .create_YOLO_config(project),
    project_dir = project,
    eval_dir    = eval_dir,
    test_images = as.list(test_images),
    groups      = as.list(groups),
    conf        = conf,
    n_boot      = as.integer(n_boot),
    seed        = as.integer(seed),
    gpu         = gpu
  )

  # Model information
  train_info <- py_evaluate_model$training_summary(model_dir)
  if (!is.null(training_time)) {
    train_info$training_time_hours <- round(training_time, 3)
  }
  model_info <- list(
    model_name = model_name,
    evaluated = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    deployment_conf = conf,
    training = train_info,
    inference_speed_ms_per_image = res$speed_ms,
    software = c(
      list(
        AnimalTrackR = as.character(utils::packageVersion("AnimalTrackR")),
        R = paste(R.version$major, R.version$minor, sep = "."),
        reticulate = as.character(utils::packageVersion("reticulate"))
      ),
      sys_info[intersect(names(sys_info),
                         c("python", "torch", "cuda_version", "ultralytics",
                           "opencv", "numpy"))]
    ),
    hardware = sys_info[intersect(names(sys_info),
                                  c("os", "processor", "cpu_count", "ram_gb",
                                    "cuda_available", "gpus"))]
  )
  yaml::write_yaml(model_info, file.path(eval_dir, "model_info.yaml"),
                   handlers = list(logical = yaml::verbatim_logical))

  out <- list(
    test_metrics       = utils::read.csv(file.path(eval_dir, "test_metrics.csv")),
    deployment_metrics = utils::read.csv(file.path(eval_dir, "deployment_metrics.csv")),
    threshold_sweep    = utils::read.csv(file.path(eval_dir, "threshold_sweep.csv")),
    per_group_metrics  = utils::read.csv(file.path(eval_dir, "per_group_metrics.csv")),
    split_summary      = split_summary,
    model_info         = model_info
  )

  .print_evaluation(out, eval_dir)

  invisible(out)
}



#' INTERNAL Count images, groups and annotations in each split
#' @noRd
.split_summary <- function(project, group_pattern = NULL) {
  do.call(rbind, lapply(c("Train", "Val", "Test"), function(s) {
    imgs <- list.files(file.path(project, "YOLO", s, "images"),
                       pattern = "\\.(jpe?g|png)$", ignore.case = TRUE)
    grp <- .image_groups(imgs, group_pattern)
    grp[is.na(grp)] <- imgs[is.na(grp)]
    labs <- file.path(project, "YOLO", s, "labels",
                      paste0(tools::file_path_sans_ext(imgs), ".txt"))
    cls <- unlist(lapply(labs[file.exists(labs)], function(f) {
      l <- readLines(f, warn = FALSE)
      sub("\\s.*$", "", l[nzchar(trimws(l))])
    }))
    data.frame(
      set = s,
      n_images = length(imgs),
      n_groups = length(unique(grp)),
      n_target = sum(cls == "0"),
      n_zzz = sum(cls == "1"),
      n_empty = sum(cls == "2")
    )
  }))
}



#' INTERNAL Print a short summary of an evaluation
#' @noRd
.print_evaluation <- function(out, eval_dir) {
  tm <- out$test_metrics
  dm <- stats::setNames(out$deployment_metrics$value, out$deployment_metrics$metric)
  dmr <- out$deployment_metrics
  ci <- function(m) {
    r <- dmr[dmr$metric == m, ]
    if (nrow(r) == 0 || is.na(r$boot_lower)) return("")
    sprintf(" (95%% CI %.3f-%.3f)", r$boot_lower, r$boot_upper)
  }
  cat("\n---- Test set evaluation ----\n")
  cat("Standard detection metrics:\n")
  print(tm, row.names = FALSE)
  cat(sprintf(
    "\nDeployment (confidence >= %s):\n  Precision %.3f%s\n  Recall    %.3f%s\n  F1        %.3f%s\n",
    dm[["conf_threshold"]],
    dm[["precision"]], ci("precision"),
    dm[["recall"]], ci("recall"),
    dm[["F1"]], ci("F1")
  ))
  if (!is.na(dm[["centre_error_px_median"]])) {
    cat(sprintf("  Median box-centre error: %.1f px (%.1f%% of box length)\n",
                dm[["centre_error_px_median"]],
                100 * dm[["centre_error_rel_median"]]))
  }
  if (!is.na(dm[["best_F1_conf"]]) && dm[["best_F1_conf"]] != dm[["conf_threshold"]]) {
    cat(sprintf("  Highest F1 (%.3f) at confidence %.2f - see threshold_sweep.csv\n",
                dm[["best_F1"]], dm[["best_F1_conf"]]))
  }
  cat("\nFull results saved to ", eval_dir, "\n", sep = "")
}
