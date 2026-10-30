#' Train a detection model to find your animals
#'
#' @description
#' `train_Model()` uses the images you have annotated in your project to train a
#' YOLO11 object detection model to find your animal in video frames. When
#' training finishes, the model is automatically tested on images it has not
#' seen (see "Checking the model" below), and a summary of how well it performs
#' is shown in your R session.
#'
#' @details
#' **What happens during training**
#'
#' Training starts from a model that has already learnt to recognise everyday
#' objects (the standard [YOLO11s](https://docs.ultralytics.com/models/yolo11/)
#' model) and adapts it to your animal using your annotated images. Your
#' images are used in three groups, set up by [save_annotations()]:
#'
#' * the *training set*, which the model learns from;
#' * the *validation set*, which is used during training to check progress and
#'   decide when to stop;
#' * the *test set*, which is only used once training has finished, to give a
#'   fair measure of how well the model performs on new footage.
#'
#' The model works through the whole training set many times. Each pass is
#' called an *epoch*. Training runs for up to 200 epochs, but stops early if
#' the model has not improved on the validation set for 50 epochs. The version
#' of the model from its best epoch is kept. Training is much faster on a
#' computer with a suitable graphics card (GPU), which is used automatically if
#' available; on a standard computer it can take several hours or more.
#'
#' **Checking the model**
#'
#' Once training is complete the model is tested on the test set using
#' [evaluate_Model()]. A summary is printed in the R session and a summary
#' figure drawn, showing how often the model finds your animal (recall), how
#' often its detections are correct (precision), how precisely it locates the
#' animal, and any test videos where it does less well. Full results, and a
#' record of the training settings, hardware and software used, are saved in
#' `YOLO/models/<model_name>/evaluation/`. See [evaluate_Model()] for how to
#' read the results and what to do if the model is not good enough.
#'
#' **Have I annotated enough images?**
#'
#' Train a few models on increasing numbers of your training images using
#' `n_train`, and compare their test results. If performance is still
#' improving as images are added, annotating more images is likely to help; if
#' it has levelled off, you probably have enough. For example, with 600
#' training images:
#'
#' ```
#' for (n in c(150, 300, 450)) {
#'   train_Model(paste0("model_n", n), n_train = n)
#' }
#' train_Model("model_all")
#' ```
#'
#' Repeating each size with a few different `n_train_seed` values shows how much
#' the results depend on which images happen to be chosen.
#'
#' **Changing the training settings**
#'
#' The default settings work well in most cases and most users will not need
#' to change them. They are stored in `YOLO/configs/train_config.yaml` in your
#' project: an image size of 640 pixels, 16 images processed at a time (batch
#' size), colour and rotation changes to the images switched off, and a fixed
#' random seed (0) so that training is as repeatable as possible. Experienced
#' users can edit this file to change these or add other settings (see the
#' [Ultralytics documentation](https://docs.ultralytics.com/modes/train/#train-settings)),
#' but changes should be made with care as they can make results worse.
#' [restore_train_config()] restores the defaults.
#'
#' @param model_name A name for the model, e.g. `"model1"`. Use a new name each
#'  time you train, so you can compare models. If the name is already in use,
#'  a number is added to the end of it and a message tells you.
#' @param project The path to the TrackR project folder. By default the active
#'  project (from `get_Project()`), which is right for almost all uses.
#' @param config Leave as `NULL` (the default) for almost all uses: AnimalTrackR
#'  creates the file describing where your images are. Experienced users can
#'  supply the path to their own YOLO dataset `.yaml` file.
#' @param target_only Logical. If `TRUE`, annotations of neighbouring animals
#'  (the `ZZZ` class) are ignored during training and the model learns only
#'  your target animal. This can help if you have very few `ZZZ` annotations.
#'  Default `FALSE`.
#' @param evaluate Logical. Test the model on the test set when training
#'  finishes? Default `TRUE`. See [evaluate_Model()].
#' @param conf The confidence threshold you will use when tracking with
#'  [run_Model()]: detections with lower confidence are ignored. Used when
#'  testing the model. Default 0.75.
#' @param group_pattern Optional. Identifies which video each image came from,
#'  for the per-video results. Not needed for images extracted with
#'  [extract_images()]. See "Choosing a `group_pattern`" in
#'  [save_annotations()].
#' @param plot Logical. Draw the summary figure in the R session? Defaults to
#'  `TRUE` when working interactively. The figure is always saved.
#' @param n_train Optional. Train on a random selection of this many of the
#'  training images instead of all of them. Useful for checking whether you
#'  have annotated enough images (see "Have I annotated enough images?"
#'  below). The validation and test sets are unchanged.
#' @param n_train_seed Random seed used to choose the `n_train` images, so the
#'  same images are chosen each time. Use different seeds to see how much
#'  results vary with the images chosen.
#' @param seed Optional random seed for training, overriding the one in
#'  `train_config.yaml` (0 by default). Training with different seeds shows how
#'  much results vary from run to run.
#' @param device Optional. Where to train: `"cpu"`, `"gpu"`, or a GPU number
#'  (e.g. `0` or `1`) on a computer with more than one GPU. By default the GPU
#'  is used if there is one. On a computer with two GPUs, you can train two
#'  models at once in separate R sessions, one with `device = 0` and one with
#'  `device = 1`.
#'
#' @return Invisibly, the test results (see [evaluate_Model()]), or `TRUE` if
#'  `evaluate = FALSE`. The trained model is saved in
#'  `YOLO/models/<model_name>/`, with the model itself at
#'  `weights/best.pt`.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' # After images have been annotated, set the active project
#' library(AnimalTrackR)
#' set_Project("path/to/TrackR_project")
#'
#' # Train a model on your annotations. A summary of how well it performs on
#' # the test set is printed when training finishes.
#' results <- train_Model("model1")
#'
#' # The model is saved in 'YOLO/models/model1' and the test results in
#' # 'YOLO/models/model1/evaluation'
#' }
#'
#' @import reticulate
#' @importFrom tools file_ext
#'

train_Model <- function(model_name, project = get_Project(), config = NULL,
                        target_only = FALSE, evaluate = TRUE, conf = 0.75,
                        group_pattern = NULL, plot = interactive(),
                        n_train = NULL, n_train_seed = 0, seed = NULL,
                        device = NULL) {

  # Check that there is an active project
  if (is.null(project)) {
    stop("No active TrackR project. Activate a project using `set-Project()`.")
  }

  .check_device(device)
  if (identical(device, "gpu")) device <- 0
  if (!is.null(seed) && (!is.numeric(seed) || length(seed) != 1 || seed < 0 ||
                         seed != round(seed))) {
    stop("`seed` must be a single whole number of 0 or more.")
  }
  if (!is.null(n_train) && !is.null(config)) {
    stop("`n_train` cannot be used with a custom `config` file.")
  }

  # Take the config argument and either create a suitable config file or
  # load a suplied file depending on its value
  subset <- NULL
  if (is.null(config) && !is.null(n_train)) {
    subset <- .train_subset(project, model_name, n_train, n_train_seed)
    config <- subset$config
  } else if (is.null(config)) {
    config <- .create_YOLO_config()
  } else {

    if (!is.character(config)) {
      stop("Invalid config argument. config must either be left as the default (NULL) or be a valid file path to a YOLO configuration yaml file.")
    }

    if (file.exists(config)) {
      if (tools::file_ext(config) != "yaml") {
        stop("Supplied config file must be a .yaml file. See https://docs.ultralytics.com/datasets/detect/ for details on the format.\n You can also change the `config` argument to NULL and the function will create a suitable config file automatically.")
      }
    } else {
      stop("Invalid config argument. config must either be left as the default (NULL) or be a valid file path to a YOLO configuration yaml file.")
    }
  }

  # Check that there are images available in the relevant directories
  n_train_imgs <- length(list.files(file.path(project, "YOLO", "Train", "images")))
  n_test <- length(list.files(file.path(project, "YOLO", "Test", "images")))
  n_val <- length(list.files(file.path(project, "YOLO", "Val", "images")))

  # Throw an error if any of the directories are empty
  if (n_train_imgs == 0) {
    stop("There are no training images in your current project directory. Please annotate some images and add them to the directory with `save_annotations()`.")
  }
  if (n_val == 0) {
    stop("There are no validation images in your current project directory. Please annotate some images and add them to the directory with `save_annotations()`.")
  }
  if (n_test == 0) {
    stop("There are no testing images in your current project directory. Please annotate some images and add them to the directory with `save_annotations()`.")
  }

  # Warn users if there are few training images
  if (sum(n_train_imgs, n_test, n_val) < 100) {
    warning("Low number (<100) images found in the project directory. Training results may be very poor with this few images.")
  }

  # Locate the training hyperparamter file and chekc it exists
  param_file <- file.path(project, "YOLO", "configs", "train_config.yaml")

  if (!file.exists(param_file)) {
    stop("Training configuration file not found. Recreate a default file with `restore_train_config()`")
  }

  # Check that there is an active conda environment and that it is correctly set up
  envname <- basename(reticulate::py_discover_config()$pythonhome)
  gpu_available <- check_gpu(envname)

  if (!check_TrackR_env(envname)) {
    stop(paste0("Active conda envionrment (", envname, ") does not meet TrackR requirements.\n Please activate or create a suitable environment with `set_TrackR_env()` or `create_TrackR_env()`.\n  You may need to restart your R session."))
  }

  if (is.numeric(device) && !gpu_available) {
    stop("GPU ", device, " was requested but no GPU is available in this environment. ",
         "See `?configure_cuda`.")
  }
  on_gpu <- if (is.null(device)) gpu_available else !identical(device, "cpu")
  if (on_gpu) {
    cat("Beginning training with GPU acceleration...")
  } else {
    cat("Beginning training on CPU...")
  }

  start_time <- Sys.time()

  save_dir <- py_train_model$train_model(
    config_file = reticulate::r_to_py(config),
    project_dir = reticulate::r_to_py(project),
    model_name  = reticulate::r_to_py(model_name),
    gpu         = reticulate::r_to_py(gpu_available),
    param_file  = reticulate::r_to_py(param_file),
    t_only      = reticulate::r_to_py(target_only),
    device      = if (is.null(device)) NULL else
      if (is.numeric(device)) as.integer(device) else device,
    seed        = if (is.null(seed)) NULL else as.integer(seed)
  )

  training_hours <- as.numeric(difftime(Sys.time(), start_time, units = "hours"))

  # Ultralytics adds a number to the name if a model of that name already exists
  saved_name <- basename(save_dir)
  if (!identical(saved_name, model_name)) {
    message("A model called '", model_name, "' already exists, so this model ",
            "was saved as '", saved_name, "'.")
  }

  # Record how this model was trained, alongside Ultralytics' own records
  run_info <- list(
    model_name = saved_name,
    n_train_available = n_train_imgs,
    n_train_used = n_train_imgs,
    n_train_seed = NULL,
    seed = seed,
    device = if (is.null(device)) (if (gpu_available) "gpu 0" else "cpu") else
      if (is.numeric(device)) paste("gpu", device) else device,
    target_only = target_only,
    training_time_hours = round(training_hours, 3)
  )
  if (!is.null(subset)) {
    run_info$n_train_used <- length(subset$images)
    run_info$n_train_seed <- n_train_seed
    writeLines(basename(subset$images), file.path(save_dir, "train_images.txt"))
  }
  yaml::write_yaml(Filter(Negate(is.null), run_info),
                   file.path(save_dir, "animaltrackr_run.yaml"),
                   handlers = list(logical = yaml::verbatim_logical))

  if (!evaluate) {
    return(invisible(TRUE))
  }

  evaluate_Model(
    model_name = saved_name,
    device = if (is.null(device)) "auto" else device,
    conf = conf,
    group_pattern = group_pattern,
    training_time = training_hours,
    plot = plot,
    project = project
  )

}



#' INTERNAL Check a device argument: NULL, "cpu" or a GPU index
#' @noRd
.check_device <- function(device, allow_auto = FALSE) {
  if (is.null(device)) return(invisible(TRUE))
  if (allow_auto && identical(device, "auto")) return(invisible(TRUE))
  if (identical(device, "cpu") || identical(device, "gpu")) return(invisible(TRUE))
  if (is.numeric(device) && length(device) == 1 && !is.na(device) &&
      device >= 0 && device == round(device)) {
    return(invisible(TRUE))
  }
  stop("`device` must be \"cpu\", \"gpu\" or a GPU number (e.g. 0 or 1)",
       if (allow_auto) ", or \"auto\"" else "", ".")
}



#' INTERNAL Choose a random subset of the training images for one model
#'
#' Writes the list of chosen images and a dataset config pointing at it, both
#' named after the model so that several models can be trained at once.
#'
#' @return list(config, images)
#' @noRd
.train_subset <- function(project, model_name, n_train, n_train_seed = 0) {
  if (!is.numeric(n_train) || length(n_train) != 1 || is.na(n_train) ||
      n_train < 1 || n_train != round(n_train)) {
    stop("`n_train` must be a single whole number of images (1 or more).")
  }
  imgs <- list.files(file.path(project, "YOLO", "Train", "images"),
                     pattern = "\\.(jpe?g|png)$", ignore.case = TRUE,
                     full.names = TRUE)
  if (n_train > length(imgs)) {
    stop("`n_train` (", n_train, ") is more than the ", length(imgs),
         " images in the training set.")
  }

  # Local, reproducible sampling that leaves the user's random numbers unchanged
  if (exists(".Random.seed", envir = .GlobalEnv)) {
    old_seed <- get(".Random.seed", envir = .GlobalEnv)
    on.exit(assign(".Random.seed", old_seed, envir = .GlobalEnv), add = TRUE)
  } else {
    on.exit(rm(".Random.seed", envir = .GlobalEnv), add = TRUE)
  }
  set.seed(n_train_seed)
  chosen <- sort(sample(imgs, n_train))

  list_file <- file.path(project, "YOLO", "configs",
                         paste0("train_images_", model_name, ".txt"))
  writeLines(normalizePath(chosen, winslash = "/"), list_file)

  config <- .create_YOLO_config(
    project,
    train = normalizePath(list_file, winslash = "/"),
    file_name = paste0("YOLO_config_", model_name, ".yaml")
  )
  message("Training on ", n_train, " of ", length(imgs), " training images.")
  list(config = config, images = chosen)
}
