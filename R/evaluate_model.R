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
#' A summary is printed in the R session, covering the data in each set, how
#' training went, the detection and tracking metrics, any test videos where
#' the model performs less well, and inference speed. A summary figure is
#' also drawn (see `plot`).
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
#' | `evaluation_summary.png` | Summary figure: precision, recall and F1 across confidence thresholds, F1 for each test video, and position error |
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
#' @param plot Logical. Draw the summary figure in the R session? Defaults to
#'   `TRUE` in interactive sessions. The figure is always saved as
#'   `evaluation_summary.png`.
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
                           plot = interactive(),
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

  # Summary figure: always saved, and drawn in the R session if requested
  grDevices::png(file.path(eval_dir, "evaluation_summary.png"),
                 width = 12, height = 4.2, units = "in", res = 150)
  tryCatch(.plot_evaluation(out, eval_dir), finally = grDevices::dev.off())
  if (plot) .plot_evaluation(out, eval_dir)

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



#' INTERNAL Print a summary of an evaluation to the console
#' @noRd
.print_evaluation <- function(out, eval_dir) {

  line <- function(title) cat("\n-- ", title, " ", strrep("-", max(0, 60 - nchar(title))), "\n", sep = "")
  dmr <- out$deployment_metrics
  dm <- stats::setNames(dmr$value, dmr$metric)
  ci <- function(m) {
    r <- dmr[dmr$metric == m, ]
    if (nrow(r) == 0 || is.na(r$boot_lower)) return("")
    sprintf("  (95%% CI %.3f-%.3f)", r$boot_lower, r$boot_upper)
  }
  info <- out$model_info

  cat("\n== Evaluation of '", info$model_name, "' ", strrep("=", max(0, 46 - nchar(info$model_name))), "\n", sep = "")

  # Data
  line("Data")
  ss <- out$split_summary
  for (i in seq_len(nrow(ss))) {
    cat(sprintf("  %-6s %5d images from %3d video%s  (Target %d, ZZZ %d, Empty %d)\n",
                ss$set[i], ss$n_images[i], ss$n_groups[i],
                ifelse(ss$n_groups[i] == 1, " ", "s"),
                ss$n_target[i], ss$n_zzz[i], ss$n_empty[i]))
  }

  # Training
  tr <- info$training
  if (!is.null(tr$epochs_run)) {
    line("Training")
    epochs <- tr$settings$epochs
    cat(sprintf("  Epochs run: %d of %s%s; best epoch: %s\n",
                as.integer(tr$epochs_run), ifelse(is.null(epochs), "?", epochs),
                ifelse(isTRUE(tr$stopped_early), " (stopped early)", ""),
                ifelse(is.null(tr$best_epoch), "?", tr$best_epoch)))
    if (!is.null(tr$training_time_hours)) {
      cat(sprintf("  Training time: %.2f hours\n", tr$training_time_hours))
    }
  }

  # Standard metrics
  line("Detection metrics (test set)")
  tm <- out$test_metrics
  tm[] <- lapply(tm, function(x) if (is.numeric(x)) sprintf("%.3f", x) else x)
  names(tm) <- c("Class", "Precision", "Recall", "F1", "mAP@0.5", "mAP@0.5:0.95")[seq_along(tm)]
  print(tm, row.names = FALSE, right = TRUE)

  # Deployment
  line(sprintf("Tracking performance (confidence >= %s)", dm[["conf_threshold"]]))
  cat(sprintf("  Precision %.3f%s\n", dm[["precision"]], ci("precision")))
  cat(sprintf("  Recall    %.3f%s\n", dm[["recall"]], ci("recall")))
  cat(sprintf("  F1        %.3f%s\n", dm[["F1"]], ci("F1")))
  cat(sprintf("  %d test images (%d with an annotated target): %d correct, %d false or misplaced, %d missed\n",
              as.integer(dm[["n_test_images"]]), as.integer(dm[["TP"]] + dm[["FN"]]),
              as.integer(dm[["TP"]]), as.integer(dm[["FP"]]), as.integer(dm[["FN"]])))
  if (!is.na(dm[["centre_error_px_median"]])) {
    cat(sprintf("  Position error: median %.1f px (%.1f%% of box length), 95%% of detections within %.1f px\n",
                dm[["centre_error_px_median"]], 100 * dm[["centre_error_rel_median"]],
                dm[["centre_error_px_p95"]]))
  }
  if (!is.na(dm[["best_F1_conf"]]) && dm[["best_F1"]] > dm[["F1"]] + 1e-9) {
    cat(sprintf("  Note: F1 is highest (%.3f) at confidence %.2f; see threshold_sweep.csv\n",
                dm[["best_F1"]], dm[["best_F1_conf"]]))
  }
  cat(" ", .fmt_ci_note(dmr), "\n")

  # Videos to check
  pg <- out$per_group_metrics
  if (nrow(pg) > 1) {
    line("Test videos")
    pg <- pg[order(pg$F1, -pg$n_images), ]
    weak <- pg[!is.na(pg$F1) & pg$F1 < 0.95, ]
    if (nrow(weak) == 0) {
      cat(sprintf("  All %d test videos have F1 >= 0.95\n", nrow(pg)))
    } else {
      cat(sprintf("  %d of %d test videos have F1 < 0.95:\n", nrow(weak), nrow(pg)))
      show <- utils::head(weak, 5)
      for (i in seq_len(nrow(show))) {
        cat(sprintf("    %-25s F1 %.3f  (%d images: %d missed, %d false)\n",
                    show$group[i], show$F1[i], show$n_images[i], show$FN[i], show$FP[i]))
      }
      if (nrow(weak) > 5) cat("    ... see per_group_metrics.csv\n")
    }
  }

  # Speed
  sp <- info$inference_speed_ms_per_image
  if (!is.null(sp$inference)) {
    line("Speed")
    total <- sum(unlist(sp[c("preprocess", "inference", "postprocess")]), na.rm = TRUE)
    cat(sprintf("  %.1f ms per image on %s (about %.0f frames per second)\n",
                total, toupper(sp$device), 1000 / total))
  }

  line("Next steps")
  cat("  - Look at the images the model got wrong: test_predictions.csv (outcome column)\n")
  cat("  - Check the model visually on your videos with demo_run()\n")
  cat("  - Full results: ", eval_dir, "\n", sep = "")
  invisible(NULL)
}



#' INTERNAL One-line explanation of the confidence intervals
#' @noRd
.fmt_ci_note <- function(dmr) {
  n <- dmr$value[dmr$metric == "n_test_groups"]
  if (length(n) == 0 || is.na(n) || n < 2) {
    return("(Confidence intervals need test images from 2 or more videos.)")
  }
  sprintf("(CIs from resampling the %d test videos; see deployment_metrics.csv)", as.integer(n))
}



#' INTERNAL Summary figure of an evaluation
#'
#' @description
#' Three panels: precision, recall and F1 across confidence thresholds; F1 for
#' each test video; and the distribution of box-centre errors.
#'
#' @noRd
.plot_evaluation <- function(out, eval_dir) {

  sw <- out$threshold_sweep
  pg <- out$per_group_metrics
  dm <- stats::setNames(out$deployment_metrics$value, out$deployment_metrics$metric)
  conf <- dm[["conf_threshold"]]
  pred <- utils::read.csv(file.path(eval_dir, "test_predictions.csv"))
  err <- 100 * pred$centre_error_rel[pred$outcome == "TP"]
  err <- err[!is.na(err)]

  cols <- c(P = "#1b6ca8", R = "#d1495b", F1 = "#2e2e2e")

  op <- graphics::par(mfrow = c(1, 3), mar = c(4.5, 4.5, 3, 1), las = 1)
  on.exit(graphics::par(op), add = TRUE)

  # 1. Threshold sweep
  yl <- range(c(sw$precision, sw$recall, sw$F1), na.rm = TRUE)
  yl <- c(max(0, min(yl[1], 0.8)), 1)
  graphics::plot(sw$conf, sw$F1, type = "l", lwd = 2.5, col = cols["F1"],
                 ylim = yl, xlim = c(0, 1),
                 xlab = "Confidence threshold", ylab = "Score",
                 main = "Performance by confidence")
  graphics::lines(sw$conf, sw$precision, lwd = 2, col = cols["P"])
  graphics::lines(sw$conf, sw$recall, lwd = 2, col = cols["R"])
  graphics::abline(v = conf, lty = 2, col = "grey40")
  graphics::text(conf, yl[1], paste0(" conf = ", conf), adj = c(0, 0), cex = 0.8, col = "grey30")
  graphics::legend("bottomleft", c("Precision", "Recall", "F1"),
                   col = cols[c("P", "R", "F1")], lwd = 2, bty = "n", cex = 0.85,
                   inset = c(0, 0.08))

  # 2. Per-video F1
  pg <- pg[order(pg$F1), ]
  if (nrow(pg) > 0 && any(!is.na(pg$F1))) {
    labs <- if (nrow(pg) <= 25) pg$group else rep("", nrow(pg))
    graphics::dotchart(pg$F1, labels = labs, pch = 19,
                       col = ifelse(pg$F1 < 0.95, cols["R"], cols["F1"]),
                       xlim = c(min(0.8, pg$F1, na.rm = TRUE), 1),
                       xlab = "F1", main = "F1 by test video", cex = 0.75)
    graphics::abline(v = 0.95, lty = 3, col = "grey60")
  } else {
    graphics::plot.new(); graphics::title("F1 by test video")
  }

  # 3. Position error
  if (length(err) > 0) {
    graphics::hist(err, breaks = 20, col = "grey80", border = "white",
                   xlab = "Box-centre error (% of box length)",
                   ylab = "Images", main = "Position error")
    graphics::abline(v = stats::median(err), lwd = 2, col = cols["P"])
  } else {
    graphics::plot.new(); graphics::title("Position error")
  }

  invisible(NULL)
}
