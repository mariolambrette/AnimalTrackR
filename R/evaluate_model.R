#' Test how well a trained model finds your animals
#'
#' @description
#' `evaluate_Model()` checks how well a trained model performs on the images in
#' your project's test set. These images are held back and never used during
#' training, and all frames from a given video are kept together (see
#' [save_annotations()]), so the results show how the model is likely to
#' perform on new footage.
#'
#' You will not usually need to call this function yourself: [train_Model()]
#' runs it automatically when training finishes. Use it to re-check a model,
#' for example with a different confidence threshold (`conf`), or to evaluate a
#' model trained with an older version of AnimalTrackR.
#'
#' @details
#' **Reading the results**
#'
#' The model gives every detection a *confidence* score between 0 and 1.
#' [run_Model()] only uses detections whose confidence is at least `conf`
#' (0.75 by default), and only the single most confident detection in each
#' frame. The *tracking performance* figures show how the model performs when
#' used in exactly this way, so these are the most relevant figures for
#' tracking animals:
#'
#' * **Precision**: of the detections the model made, the proportion that were
#'   in the right place. Low precision means the model is detecting things that
#'   are not your animal (e.g. reflections, equipment or animals in
#'   neighbouring tanks), which will create false jumps in your tracks.
#' * **Recall**: of the images where the animal was visible, the proportion
#'   where the model found it. Low recall means gaps in your tracks.
#' * **F1**: a single score combining precision and recall. 1 is perfect.
#' * **Position error**: the distance between the centre of the model's box and
#'   the centre of the box you drew, in pixels and as a percentage of the
#'   length of your box. This shows how precisely the animal's position is
#'   measured. Ideally it should be small compared with how far your animals
#'   typically move between the frames you analyse.
#'
#' A detection counts as "in the right place" if its box overlaps the box you
#' drew by at least half (technically, an intersection-over-union of 0.5 or
#' more).
#'
#' Each of precision, recall and F1 is given with a 95% *confidence interval*:
#' the range the true value is likely to lie in. The interval is wide when the
#' test set contains few images or videos. At least two test videos are needed
#' to calculate it.
#'
#' The *detection metrics* table gives the standard figures used to report
#' object detection models in scientific papers, for each class (your animal,
#' `Target`, and neighbouring animals, `ZZZ`):
#'
#' * **Precision, recall and F1**, as above, but counting every detection and
#'   using the confidence threshold that gives the highest F1.
#' * **mAP@0.5** (mean average precision): a summary of precision and recall
#'   across all confidence thresholds. 1 is perfect.
#' * **mAP@0.5:0.95**: as mAP@0.5, but stricter about how tightly the boxes
#'   fit the animal. It is always lower than mAP@0.5.
#'
#' These are useful for reporting and for comparing models. For judging
#' whether a model is good enough for tracking, the tracking performance figures
#' are more directly relevant.
#'
#' **Is my model good enough?**
#'
#' Values close to 1 for precision and recall are the aim, but what is good
#' enough depends on your study. It is always worth looking at the model's
#' output on your own videos with [demo_run()]. If the results are not good
#' enough:
#'
#' * *Low recall (animals missed)*: annotate more images, particularly from the
#'   videos or conditions where animals are missed (in
#'   `test_predictions.csv`, images with outcome `FN` or `FP+FN` are the ones
#'   where the animal was missed or detected in the wrong place). A lower
#'   confidence threshold also increases recall, at some cost to precision.
#' * *Low precision (false detections)*: if the false detections are animals in
#'   neighbouring tanks, annotate them with the `ZZZ` class so the model learns
#'   to ignore them. Raising the confidence threshold also reduces false
#'   detections, at some cost to recall.
#' * *Choosing a confidence threshold*: the left-hand panel of the summary
#'   figure shows precision, recall and F1 at each threshold. A threshold where
#'   F1 is high and the lines are fairly flat is a good choice.
#' * *One or two poor videos*: the "Test videos" section of the summary lists
#'   any videos with F1 below 0.95. These often have unusual conditions
#'   (lighting, background, camera position). Adding training images from
#'   similar footage usually helps.
#'
#' **Outputs**
#'
#' A summary is printed in the R session covering: how many images and videos
#' are in each set, how training went, the detection metrics, tracking
#' performance, any test videos where the model does less well, and how fast
#' the model runs. A summary figure is also drawn (see `plot`). Everything is
#' saved in `YOLO/models/<model_name>/evaluation/`:
#'
#' | File | Contents |
#' |---|---|
#' | `evaluation_summary.png` | Summary figure: precision, recall and F1 at each confidence threshold; F1 for each test video; and position error |
#' | `deployment_metrics.csv` | Tracking performance: precision, recall and F1 with confidence intervals, counts of correct, false and missed detections, position error, and the confidence threshold giving the highest F1 |
#' | `test_metrics.csv` | Detection metrics for each class |
#' | `threshold_sweep.csv` | Tracking precision, recall and F1 at confidence thresholds from 0.05 to 0.95 |
#' | `per_group_metrics.csv` | Tracking performance for each test video |
#' | `test_predictions.csv` | Results for every test image: the box you drew, the model's box and confidence, and the outcome (`TP` correct, `FP` false detection, `FN` missed, `FP+FN` detected in the wrong place, `TN` correctly found nothing). Useful for finding the images the model gets wrong |
#' | `split_summary.csv` | Number of images, videos and annotations of each class in the training, validation and test sets |
#' | `model_info.yaml` | A record of the training settings, how many epochs were run, training time, speed, and the computer and software versions used. Useful for the methods section of a paper |
#' | `plots/` | Further plots from Ultralytics (the software that trains the model), including precision-recall curves and confusion matrices |
#'
#' **Testing a model on other footage**
#'
#' Give `evaluate_Model()` the path to another model's weights file to test
#' that model on the current project's test images. For example, a model
#' trained on one experiment can be tested on footage from another:
#'
#' ```
#' set_Project("path/to/zebrafish_project")
#' evaluate_Model("path/to/trout_project/YOLO/models/trout_model/weights/best.pt")
#' ```
#'
#' The results are saved in `YOLO/evaluations/trout_model/` in the zebrafish
#' project. Only the classes both projects share are meaningful; for example, a
#' project with no `ZZZ` annotations has no `ZZZ` results.
#'
#' **Technical details**
#'
#' Two confidence intervals are saved in `deployment_metrics.csv`. The
#' interval printed in the summary (`boot_lower`, `boot_upper`) comes from
#' resampling whole test videos with replacement (`n_boot` times), which allows
#' for frames from the same video being similar to each other. A Wilson
#' interval, which treats every image as independent and so is usually
#' narrower, is also given (`wilson_lower`, `wilson_upper`). Images with a
#' detection that overlaps no annotated target, or overlaps it by less than
#' 0.5, count as a false positive; where a target was annotated, a misplaced
#' detection also counts as a false negative. The detection metrics are
#' calculated by Ultralytics' validator on the test set, scoring only the
#' classes the model was trained on (the `Empty` label is excluded).
#'
#' @param model_name Name of the model to evaluate, as given to
#'   [train_Model()]. Alternatively, the path to a model's weights file
#'   (`.pt`), for example a model trained in a different project. This lets
#'   you test how well a model works on another set of footage (see
#'   "Testing a model on other footage" below).
#' @param conf Confidence threshold that will be used when tracking with
#'   [run_Model()] (its `conf` argument). Detections with lower confidence are
#'   ignored. Default 0.75.
#' @param group_pattern Optional. Identifies which video each test image came
#'   from, for the per-video results and confidence intervals. Not needed for
#'   images extracted with [extract_images()]. See "Choosing a `group_pattern`"
#'   in [save_annotations()].
#' @param n_boot Number of times the test videos are resampled to calculate
#'   confidence intervals. The default (1000) is fine for almost all uses.
#' @param seed Random seed, so the confidence intervals are the same each time.
#' @param plot Logical. Draw the summary figure in the R session? Defaults to
#'   `TRUE` when working interactively. The figure is always saved as
#'   `evaluation_summary.png`.
#' @param device Where to run the model: `"auto"` (the default: the GPU if
#'   there is one, otherwise the CPU), `"cpu"`, `"gpu"`, or a GPU number (e.g.
#'   `1`). Running on `"cpu"` on a computer with a GPU shows how fast the model
#'   would run for users without one.
#' @param output_dir Folder to save the results in. By default,
#'   `YOLO/models/<model_name>/evaluation` for models in this project, or
#'   `YOLO/evaluations/<model folder name>` for models from elsewhere.
#' @param training_time Training time in hours, recorded in `model_info.yaml`.
#'   Filled in automatically by [train_Model()]; you do not need to supply it.
#' @param project The path to the TrackR project folder. By default the active
#'   project (from `get_Project()`), which is right for almost all uses.
#'
#' @return Invisibly, a list containing the results tables (`test_metrics`,
#'   `deployment_metrics`, `threshold_sweep`, `per_group_metrics`,
#'   `split_summary`) as data frames, and `model_info`. The same information is
#'   saved in the files listed above.
#' @export
#'
#' @examples
#' \dontrun{
#' # Normally run automatically by train_Model(), but can be re-run:
#' evaluate_Model("model1")
#'
#' # Re-evaluate at a lower deployment confidence threshold
#' evaluate_Model("model1", conf = 0.5)
#'
#' # Measure how fast the model runs without a GPU
#' evaluate_Model("model1", device = "cpu",
#'                output_dir = file.path(get_Project(), "YOLO", "models",
#'                                       "model1", "evaluation_cpu"))
#'
#' # Test a model trained in another project on this project's test images
#' evaluate_Model("path/to/other_project/YOLO/models/model1/weights/best.pt")
#' }
evaluate_Model <- function(model_name,
                           conf = 0.75,
                           group_pattern = NULL,
                           n_boot = 1000,
                           seed = 0,
                           training_time = NULL,
                           plot = interactive(),
                           device = "auto",
                           output_dir = NULL,
                           project = get_Project()) {

  if (is.null(project)) {
    stop("No active TrackR project. Activate a project using `set_Project()`.")
  }
  .check_conf(conf)
  .check_device(device, allow_auto = TRUE)

  if (tolower(tools::file_ext(model_name)) == "pt") {
    # A model from elsewhere, e.g. trained in another project
    weights <- model_name
    if (!file.exists(weights)) {
      stop("Model weights not found: ", weights)
    }
    model_dir <- dirname(dirname(weights))
    label <- basename(model_dir)
    default_dir <- file.path(project, "YOLO", "evaluations", label)
  } else {
    model_dir <- file.path(project, "YOLO", "models", model_name)
    weights <- file.path(model_dir, "weights", "best.pt")
    if (!file.exists(weights)) {
      stop("No trained weights found for model '", model_name, "' (expected ",
           weights, ").")
    }
    label <- model_name
    default_dir <- file.path(model_dir, "evaluation")
  }

  test_images <- list.files(file.path(project, "YOLO", "Test", "images"),
                            pattern = "\\.(jpe?g|png)$", ignore.case = TRUE)
  if (length(test_images) == 0) {
    stop("There are no test images in the project, so the model cannot be evaluated.")
  }

  eval_dir <- if (is.null(output_dir)) default_dir else output_dir
  dir.create(eval_dir, showWarnings = FALSE, recursive = TRUE)

  # Split summary
  split_summary <- .split_summary(project, group_pattern)
  utils::write.csv(split_summary, file.path(eval_dir, "split_summary.csv"),
                   row.names = FALSE)

  groups <- .image_groups(test_images, group_pattern)
  groups[is.na(groups)] <- test_images[is.na(groups)]

  sys_info <- py_evaluate_model$system_info()
  gpu <- isTRUE(sys_info$cuda_available)
  py_device <- if (identical(device, "auto")) {
    if (gpu) 0L else "cpu"
  } else if (identical(device, "cpu")) {
    "cpu"
  } else {
    if (!gpu) {
      stop("A GPU was requested but none is available. Use `device = \"cpu\"`, ",
           "or see `?configure_cuda`.")
    }
    if (identical(device, "gpu")) 0L else as.integer(device)
  }

  cat("Evaluating '", label, "' on ", length(test_images),
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
    gpu         = gpu,
    device      = py_device
  )

  # Model information
  train_info <- py_evaluate_model$training_summary(model_dir)
  if (!is.null(training_time)) {
    train_info$training_time_hours <- round(training_time, 3)
  }
  run_file <- file.path(model_dir, "animaltrackr_run.yaml")
  if (file.exists(run_file)) {
    run <- yaml::read_yaml(run_file)
    train_info[c("n_train_used", "n_train_available", "n_train_seed")] <-
      run[c("n_train_used", "n_train_available", "n_train_seed")]
    train_info <- Filter(Negate(is.null), train_info)
  }
  model_info <- list(
    model_name = label,
    model_weights = normalizePath(weights, winslash = "/"),
    evaluated_on_project = normalizePath(project, winslash = "/"),
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
    if (!is.null(tr$n_train_used) && !is.null(tr$n_train_available) &&
        tr$n_train_used < tr$n_train_available) {
      cat(sprintf("  Trained on %d of the %d training images (subset seed %s)\n",
                  as.integer(tr$n_train_used), as.integer(tr$n_train_available),
                  ifelse(is.null(tr$n_train_seed), "?", tr$n_train_seed)))
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
