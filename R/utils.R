# Global varibales for `devtools::check()`
utils::globalVariables(c("V1", "Type", "num", ".", "bbox_x", "image_width",
                         "bbox_y", "image_height", "bbox_height", "bbox_width",
                         "image_name", "index", "lines"))


#' INTERNAL Check whether a file path points to a video file
#'
#' @param path file path to test
#'
#' @return T/F depending on whether file is a video
#' @noRd
#'
#' @examples
#' \dontrun{
#'  .is_video("path/to/file.ext")
#' }
#'
#' @importFrom av av_media_info

.is_video <- function(path) {
  tryCatch({
    info <- av::av_media_info(path)
    has_video <- !is.null(info$video) && length(info$video) > 0
    has_duration <- !is.null(info$duration) && info$duration > 0
    return(has_video && has_duration)
  }, error = function(e) {
    return(FALSE)
  })
}


#' INTERNAL Create a YOLO configuration file
#'
#' @description
#' An internal function to create a YOLO config file for a given project
#' directory retrieved by `get_Project()`
#'
#' @param project path to a TrackR project (retrieved via `get_Project()` by
#'  default)
#'
#' @return invisiby return TRUE
#' @noRd
#'
#' @importFrom yaml as.yaml
#' @importFrom stringr str_replace_all

.create_YOLO_config <- function(project = get_Project()){

  if (is.null(get_Project())) {
    stop("No active TrackR project found. Please activate a project with `set_Project()` or create one with `init_Project()`")
  }

  # Create the names list of configurations
  config <- list(
    path = file.path(project, "YOLO"),
    train = "Train/images",
    val = "Val/images",
    test = "Test/images",
    names = list(
      "0" = "Target",
      "1" = "ZZZ",
      "2" = "Empty"
    )
  )

  # Construct the savepath
  yaml_path <- file.path(project, "YOLO", "configs", "YOLO_config.yaml")

  # Convert list to yaml
  yaml_text <- yaml::as.yaml(config)
  yaml_text <- stringr::str_replace_all(yaml_text, "'(\\d+)':", "\\1:")

  # Save the file
  writeLines(yaml_text, yaml_path)

  return(yaml_path)
}


#' Restore default training hyperparameter configuration file
#'
#' @description
#' This function is used to restore the default training hyperparameter
#' configuration file after it has been deleted or modified
#'
#' @param project Path to a TrackR project. By default the active project is
#'  retrieved with `get_Project()`
#'
#' @return invisibly return TRUE upon completion
#' @export
#'
#' @examples
#' \dontrun{
#' library(AnimaltrackR)
#' set_Project("path/to/TrackR_project")
#' restore_train_config()
#' }
restore_train_config <- function(project = get_Project()) {
  # Copy training configuration file
  file.copy(
    from = system.file("config", "train_config.yaml", package = "AnimalTrackR"),
    to = file.path(project, "YOLO", "configs", "train_config.yaml"),
    overwrite = TRUE
  )

  cat("Hyperparameter file restored to default.")

  return(invisible(T))
}

