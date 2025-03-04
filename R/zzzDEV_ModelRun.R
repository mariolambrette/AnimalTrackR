### MODEL RUN FUNCTIONS ###

DemoRun <- function(){

}


#' Run AnimalTrackR detection model
#'
#' @param video Path to the video on which to run inference. This can either be a path to a single video file or a path to a directory containing chapterised video.
#' @param detections_path File path at which to save the detections. This is an optional parameter. By default the detections will be saved at "videoPath/videoName.csv".
#' @param model The detection model to use for detections. If you have only trained one model in this project you can leave the default behaviour, which uses the first model that appears in the "YOLO/models" directory in the project folder. Otherwise, the name of the desired model folder should be provided. See details for more information.
#' @param save_vid Boolean. Defaults to `FALSE`, if set to `TRUE` a copy of the video is saved with predicted bounding boxes plotted onto it. This is useful for validation purposes.
#' @param save_path The path at which to save the video with overlaid detections. Only used if `save_vid` is `TRUE`.
#'
#' @details
#' ## Add info on specifiying model directory
#' ## Add recommendation for saving detections in project directory
#'
#'
#' @return Returns the filepath to the detections file
#' @export
#'
#' @examples
#' \dontrun{
#'  run_model(
#'    video = "path/to/video.mp4",
#'  )
#' }
#'
#'
#' @importFrom tools file_ext file_path_sans_ext
#' @importFrom plyr llply
#' @importFrom reticulate r_to_py


## 1. Error when specified 'video' is not a video file
## 2. Check for active project

run_Model <- function(
    video,
    detections_path = NULL,
    model = list.files(file.path(get_Project(), "YOLO", "models")[1]),
    save_vid = F,
    save_path = NULL) {

  ## Handle the video file path
  # Check video exists
  if (!file.exists(video)) {
    stop("Video not found at specified path.\n
          Please check the `video` paramter and try again.")
  }

  # Check if the path supplied is a single file or directory
  if (tools::file_ext(video) == "") {

    # If the path is a directory, list the files within it
    videos <- list.files(video, full.names = T)

    # Check that all files in the directory are video files
    if (!any(sapply(videos, .is_video))) {
      stop("Non-video files found at specified path.\n
            Please ensure the path points only to video files")
    }
  } else {
    # Change the name of video to videos for consistency downstream
    videos <- video
  }

  ## Handle save path
  if (!is.null(save_path)) {
    if (!save_vid) {
      warning("`save_path` provided but video is not being saved. \n
               Did you mean to set `save_vid` to TRUE?")
    }

    if (!file.exists(dirname(save_path))) {
      stop("Directory for `save_path` does not exist.\n
            Please save video into an existing directory.\n
            You can create the directory by running `dir.create(dirname(save_path))`")
    }
  }

  ## Get model weights path
  if (tools::file_ext(model) == "pt") {
    weights <- model
  } else {
    weights <- file.path(get_Project(), "YOLO", "models", model, "weights", "best.pt")
  }

  # Check if weights exist
  if (!file.exists(weights)) {
    stop("Model weights not found.\n
          Please check model name and try again. Weights should be saved at the
          default location of 'model/weights/best.pt' unless a full filepath is
          specified")
  }

  ## Handle detections path

  # Construct detections path if it is not provided
  if (is.null(detections_path)) {
    detections_path <- video %>%
      tools::file_path_sans_ext(.) %>%
      paste0(., ".csv")
  }

  if (!file.exists(dirname(detections_path))) {
    stop("Directory for `detections_path` does not exist.\n
         Please save detections into an existing directory.\n
         You can create the directory by running `dir.create(dirname(detections_path))`")
  }

  if (tools::file_ext(detections_path) != "csv") {
    stop("`detections_path` does not have a .csv extension.\n
         Please check the file path and try again.")
  }

  # if (file.exists(detections_path)) {
  #   stop("Detection file already exists at specified path. \n
  #        Please use a different file path or delete the existing file.")
  # }

  invisible(prepare_csv(detections_path))

  invisible(
    plyr::llply(
      videos,
      function(v) {
        ## Run inference on each video
        py_run_model$run_model(
          vid        = reticulate::r_to_py(v),
          weights    = reticulate::r_to_py(weights),
          detections = reticulate::r_to_py(detections_path)
        )
      }
    )
  )
}




#' Prepare detections csv file
#'
#' @description
#' Internal function for preparing detections csv file
#'
#' @param path Path to detections file
#' @noRd
#'
#' @return Path to detections file
#'
#' @examples
#' \dontrun{
#' prepare_csv(detections_path)
#' }

prepare_csv <- function(path) {
  # Check if the file exists
  if (file.exists(path)) {
    repeat {
      ui <- readline(prompt = "Output file for this folder/video already exists. Press 'o' to overwrite file, 'a' to append a new identifier to file name or 'q' to quit: ")

      if (ui == 'o') {
        file.remove(path)
        break

      } else if (ui == 'a') {
        # Append a new identifier to the file name
        directory <- dirname(path)
        file_name <- basename(path)
        base_name <- tools::file_path_sans_ext(file_name)
        extension <- tools::file_ext(file_name)

        # Scan the directory for files with the same base name (excluding suffixes)
        matching_files <- list.files(directory, pattern = paste0("^", base_name))

        # Count the number of matching files
        num_matching_files <- length(matching_files)

        # Construct the new file name with a suffix one higher than the count of matching files
        new_file_name <- paste0(base_name, "_", num_matching_files + 1, ".", extension)

        # Construct the new output path
        path <- file.path(directory, new_file_name)

        # Print the new output path
        cat("New output path:", path, "\n")
        break

      } else if (ui == 'q') {
        stop("Process terminated by user.")  # Stop execution of the nested function
      } else {
        cat("Invalid input. Please try again\n")
      }
    }
  }

  # Define column headings
  columns <- c('Video', 'Frame', 'Timestamp', 'xc', 'yc', 'xl', 'xr', 'yt', 'yb')

  # Write column headings to the new or existing CSV file
  write.table(t(columns), file = path, sep = ",", col.names = FALSE, row.names = FALSE, quote = FALSE, append = TRUE)

  return(path)
}




