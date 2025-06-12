#' Train a YOLOv11s model with data held in a TrackR project directory
#'
#' @description
#' This function takes the annotated data in a TrackR project directory and uses
#' it to train a YOLO object detection model. This model can then be evaluated
#' and used to track animals in a broader dataset.
#'
#' @details
#' The base weights used for training are the default [YOLO11s detection weights](https://docs.ultralytics.com/models/yolo11/#supported-tasks-and-modes).
#' Training is configured to run for 200 epochs, with an image size of 640 and a
#' batch size of 16. In almost all cases users will not need to worry about
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
#' @param model_name Character string indicating the name of the model to train
#'  (used to distinguish between multiple iterations during development)
#' @param project The path to the TrackR project directory. By default this is
#'  supplied by `get_Project()`, most users will never need to change this.
#' @param config `NULL` or the path to a YOLO dataset configuration yaml file.
#'  The default (`NULL`) automatically creates a configuration file using the
#'  TrackR project architecture. Most users will never need to change this.
#'
#' @return invisibly returns TRUE when training is complete. The model training
#'  results will be available to view at 'YOLO/model/{model_name}'
#'
#' @export
#'
#' @examples
#' /dontrun{
#'
#' # After images have been annotated, set the active project
#' library(AnimaltrackR)
#' set_Project("path/to/TackR_project")
#'
#' # Train model on annotations
#' train_Model("model1")
#'
#' # Training results will be available in the 'YOLO/model/model1' directory
#' }
#'
#' @import reticulate
#' @importFrom tools file_ext
#'

train_Model <- function(model_name, project = get_Project(), config = NULL,
                        target_only = FALSE) {

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

  py_train_model$train_model(
    config_file = reticulate::r_to_py(config),
    project_dir = reticulate::r_to_py(project),
    model_name  = reticulate::r_to_py(model_name),
    gpu         = reticulate::r_to_py(gpu_available),
    param_file  = reticulate::r_to_py(param_file),
    t_only      = reticulate::r_to_py(target_only)
  )

  return(invisible(T))

}
