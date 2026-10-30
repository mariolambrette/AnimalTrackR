#' Train a YOLOv11s model with data held in a TrackR project directory
#'
#' @description
#' This function takes the annotated data in a TrackR project directory and uses
#' it to train a YOLO object detection model. This model can then be evaluated
#' and used to track animals in a broader dataset.
#'
#' @details
#' The base weights used for training are the default [YOLO11s detection weights](https://docs.ultralytics.com/models/yolo11/#supported-tasks-and-modes).
#' Training is configured to run for up to 200 epochs, with an image size of 640
#' and a batch size of 16. Training stops early if performance on the
#' validation set has not improved for 50 epochs, and the weights from the
#' best-performing epoch are kept. In almost all cases users will not need to worry about
#' these parameters, with sufficient training data they should enable up to 100%
#' accurate detection models to be trained.
#'
#' For advanced users, or if you have a specific requirement for your model,
#' these training hyperparamters can be modified. The training hyperparamters
#' are specified in the 'train_config.yaml' file stored in the 'YOLO/configs/'
#' directory of a TrackR project. users can navigate to this file and modify the
#' already specified hyperparameters or add any other hyperparameters they wish.
#' This should be done with caution as it could significantly harm training
#' results but may be essential in some cases. For a full description of
#' possible hyperparameters see the [ultralytics documentation](https://docs.ultralytics.com/modes/train/#train-settings)
#'
#' Once training is complete the model is automatically tested on the images
#' in the project's test set, which are not used at any point during training,
#' and the results are saved alongside the model weights in
#' 'YOLO/models/\{model_name\}/evaluation'. These include standard detection
#' metrics, metrics that reflect how [run_Model()] will use the model, a record
#' of the training settings and time, and the hardware and software used. See
#' [evaluate_Model()] for full details.
#'
#' @param model_name Character string indicating the name of the model to train
#'  (used to distinguish between multiple iterations during development)
#' @param project The path to the TrackR project directory. By default this is
#'  supplied by `get_Project()`, most users will never need to change this.
#' @param config `NULL` or the path to a YOLO dataset configuration yaml file.
#'  The default (`NULL`) automatically creates a configuration file using the
#'  TrackR project architecture. Most users will never need to change this.
#' @param target_only Boolean indicating whether non target annotated objects
#'  should be included in model training. This refers to the 'ZZZ' class which
#'  contains non-target but visually similar objects. There may be cases where
#'  including these in model training actually hampers performance (e.g. where
#'  there are not very many of these objects). This parameters allows those
#'  annotations to be excluded from model training.
#'
#' @param evaluate Logical. Should the trained model be evaluated on the test
#'  set once training is complete? Default `TRUE`.
#' @param conf Confidence threshold that will be used when deploying the model
#'  with [run_Model()], used in the evaluation. Default 0.75.
#' @param group_pattern Optional regular expression identifying the video (or
#'  other group) each image came from, used in the evaluation. Not needed for
#'  images extracted with [extract_images()]. See "Choosing a `group_pattern`"
#'  in [save_annotations()].
#'
#' @return Invisibly, the evaluation results (see [evaluate_Model()]), or `TRUE`
#'  if `evaluate = FALSE`. Training outputs, including the model weights, are
#'  saved to 'YOLO/models/\{model_name\}'.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' # After images have been annotated, set the active project
#' library(AnimaltrackR)
#' set_Project("path/to/TackR_project")
#'
#' # Train model on annotations
#' train_Model("model1")
#'
#' # Training results will be available in the 'YOLO/models/model1' directory
#' # and test-set evaluation results in 'YOLO/models/model1/evaluation'
#' }
#'
#' @import reticulate
#' @importFrom tools file_ext
#'

train_Model <- function(model_name, project = get_Project(), config = NULL,
                        target_only = FALSE, evaluate = TRUE, conf = 0.75,
                        group_pattern = NULL) {

  # Check that there is an active project
  if (is.null(project)) {
    stop("No active TrackR project. Activate a project using `set-Project()`.")
  }

  # Take the config argument and either create a suitable config file or
  # load a suplied file depending on its value
  if (is.null(config)) {
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
  n_train <- length(list.files(file.path(project, "YOLO", "Train", "images")))
  n_test <- length(list.files(file.path(project, "YOLO", "Test", "images")))
  n_val <- length(list.files(file.path(project, "YOLO", "Val", "images")))

  # Throw an error if any of the directories are empty
  if (n_train == 0) {
    stop("There are no training images in your current project directory. Please annotate some images and add them to the directory with `save_annotations()`.")
  }
  if (n_val == 0) {
    stop("There are no validation images in your current project directory. Please annotate some images and add them to the directory with `save_annotations()`.")
  }
  if (n_test == 0) {
    stop("There are no testing images in your current project directory. Please annotate some images and add them to the directory with `save_annotations()`.")
  }

  # Warn users if there are few training images
  if (sum(n_train, n_test, n_val) < 100) {
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

  if (gpu_available) {
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
    t_only      = reticulate::r_to_py(target_only)
  )

  training_hours <- as.numeric(difftime(Sys.time(), start_time, units = "hours"))

  # Ultralytics adds a number to the name if a model of that name already exists
  saved_name <- basename(save_dir)
  if (!identical(saved_name, model_name)) {
    message("A model called '", model_name, "' already exists, so this model ",
            "was saved as '", saved_name, "'.")
  }

  if (!evaluate) {
    return(invisible(TRUE))
  }

  evaluate_Model(
    model_name = saved_name,
    conf = conf,
    group_pattern = group_pattern,
    training_time = training_hours,
    project = project
  )

}
