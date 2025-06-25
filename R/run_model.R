### MODEL RUN FUNCTIONS ###

#' Run an AnimalTrackR detection model in demo mode
#'
#' @description
#' This function allows use to run a detection model on clips of their footage
#' and visualise the bounding box outputs. This is useful for model validation.
#'
#' @details
#' This is a useful function for running detections on example clips to
#' visualise bounding boxes and visually verify their accuracy. However, it
#' should not be user's primary method for validating models, for this users
#' should also look at the
#'
#' ### Important for GoPro users
#' **`gopro` details:** Modern GoPro footage is unfortunately incompatible with
#' this software (see [here](https://stackoverflow.com/questions/78039408/cv2-ffmpeg-grabframe-packet-read-max-attempts-exceeded-error-after-exactly-rea) for details).
#' If you try to run a detection model on GoPro footage you may get this error:
#' `[ WARN:0@2245.518] global cap_ffmpeg_impl.hpp:1595 grabFrame packet read max attempts exceeded`.
#'
#' If this happens, set the GoPro parameter to TRUE. This reformats the footage
#' to remove the audio stream. so that it can be processed by the software. The
#' reformatted file is stored in a temporary location and the original video
#' file you provide is not modified. This reformatting slows the overall
#' processing speed. unfortunately this is currently unavoidable, but we are
#' monitoring updates this software's underlying computer vision framework
#' (`opencv`) for a more comprehensive solution.
#'
#' Note that this reformatting requires an additional software library, 'FFMPEG'.
#' This is a free video processing library that can be installed [here](https://ffmpeg.org/download.html)
#'
#' @param video Path to video clip to use for detections
#' @param model (optional) Name of the TrackR model to use for detections, or a full path to yolov8-11 model weights. NULL, the default, uses the best weights from the first model in the YOLO/models/ directory
#' @param savepath Path to a directory into which to save the video detections. NULL, the default, saves the video into the 'Demos' directory of the active TrackR project.
#' @param gopro T/F. The default is FALSE but this must be set to TRUE if footage was recorded on a gopro. See details for more information.
#'
#' @return invisibly returns TRUE upon completion
#' @export
#'
#' @examples
#' \dontrun{
#' demo_run("path/to/short_clip.mp4")
#' }
#'
#' @importFrom tools file_ext file_path_sans_ext
#' @import reticulate

demo_run <- function(video, model = NULL, savepath = NULL, gopro = FALSE){
  # Check for an active project
  if (is.null(get_Project())) {
    stop("No active TrackR project found.\n  Set an active project with `set_Project()` then try again.")
  }

  if (!file.exists(video)) {
    stop("`video` path not found. \n  Check the specified video path is correct and try again.")
  }

  if (!.is_video(video)) {
    stop("Invalid `video` argument.\n  Check the specified video path is a supported video format and try again.")
  }

  ## Get model weights path ----
  if (is.null(model)) {
    model <- list.files(file.path(get_Project(), "YOLO", "models"))[1]
  }

  if (tools::file_ext(model) == "") { # If a model name is supplied get the corresponding model weights
    proj <- get_Project()

    model_dir <- file.path(proj, "YOLO", "models", model)

    if (!dir.exists(model_dir)) {
      available_models <- list.files(file.path(proj, "YOLO", "models"))
      available_models <- available_models[tools::file_ext(available_models) == ""]
      available_models <- paste(available_models, collapse = ", ")
      stop(paste0("Model '", model, "' not found.\n Ensure the name supplied matches the name of a trained model available in the poject 'YOLO/models/' directory.\n Available options are: ", available_models))
    }

    weights <- file.path(model_dir, "weights", "best.pt")

    if (!file.exists(weights)) {
      stop(paste0("Model weights '", weights, "' not found.\n  Check the model directory for the model weights and try again.\n  You may need to provide the path to the '.pt' weights file directly if you moved or renamed it."))
    }

  } else if (tools::file_ext(model) == "pt") {
    weights <- model

    if (!file.exists(weights)) {
      stop(paste0("Model weights '", weights, "' not found.\n Check the filepath supplied and try again."))
    }
  } else {
    available_models <- list.files(file.path(proj, "YOLO", "models"))
    available_models <- available_models[tools::file_ext(available_models) == ""]
    available_models <- paste(available_models, collapse = ", ")
    stop(paste0("Invalid `model` argument.\n  Available models are: ", available_models, ".\n  Please choose one of these or specify the path to a '.pt' model weights file directly"))
  }

  ## Handle save path ----
  if (is.null(savepath)) { # Construct default save path if none provided
    project = file.path(get_Project(), "Demos")
    name = tools::file_path_sans_ext(basename(video))

    if (!dir.exists(project)) {
      cat(paste0("Creating directory: '", project, "' to save video detections"))
      dir.create(project)
    }

  } else if (tools::file_ext(savepath) != "") {
    stop("Invalid `savepath` argument.\n  `savepath` must be a valid file path to a directory into which the output video will be saved.")
  } else {
    if (!dir.exists(savepath)) {
      cat(paste0("Creating directory: '", savepath, "' to save video detections"))
      dir.create(savepath)
    }

    # Create project and name variables
    project = savepath
    name = tools::file_path_sans_ext(basename(video))
  }

  if (gopro) {
    video <- .reformat_gopro(vid = video)
  }


  py_run_model$demo_run(
    weights = reticulate::r_to_py(weights),
    video   = reticulate::r_to_py(video),
    project = reticulate::r_to_py(project),
    name    = reticulate::r_to_py(name)
  )

  invisible(T)

}


#' Run AnimalTrackR detection model
#'
#' @param video Path to the video on which to run inference. This can either be a path to a single video file or a path to a directory containing chapterised video.
#' @param detections_path File path at which to save the detections. This is an optional parameter. By default the detections will be saved at "{project_path}/Detections/VideoName.csv".
#' @param model The detection model to use for detections. If you have only trained one model in this project you can leave the default behaviour, which uses the first model that appears in the "YOLO/models" directory in the project folder. Otherwise, the name of the desired model folder should be provided. See details for more information.
#' @param save_vid Boolean. Defaults to `FALSE`, if set to `TRUE` a copy of the video is saved with predicted bounding boxes plotted onto it. This is useful for validation purposes though for long videos can significantly increase processing times. We recommend using [`demo_run()`] for this type of validation.
#' @param save_path The path at which to save the video with overlaid detections. Only used if `save_vid` is `TRUE`.
#' @param gopro T/F. The default is FALSE but this must be set to TRUE if footage was recorded on a gopro. See details for more information.
#'
#' @details
#' **`model` details:** In principle, any YOLOv8-v11 model weights can be used here.
#' For most users, providing the model name specified in [`train_Model()`] will
#' be the right thing to do here. However, users wishing to experiment with
#' other model weights can do so by providing the full path to those weights
#' here.
#'
#' ### Important for GoPro users
#' **`gopro` details:** Modern GoPro footage is unfortunately incompatible with
#' this software (see [here](https://stackoverflow.com/questions/78039408/cv2-ffmpeg-grabframe-packet-read-max-attempts-exceeded-error-after-exactly-rea) for details).
#' If you try to run a detection model on GoPro footage you may get this error:
#' "[ WARN:0@2245.518] global cap_ffmpeg_impl.hpp:1595 grabFrame packet read max attempts exceeded".
#'
#' If this happens, set the GoPro parameter to TRUE. This reformats the footage
#' to remove the audio stream. so that it can be processed by the software. The
#' reformatted file is stored in a temporary location and the original video
#' file you provide is not modified. This reformatting slows the overall
#' processing speed. unfortunately this is currently unavoidable, but we are
#' monitoring updates in this software's underlying computer vision framework
#' (`opencv`) for a more friendly solution.
#'
#' Note that this reformatting requires an additional software library, 'FFMPEG'.
#' This is a free video processing library that can be installed [here](https://ffmpeg.org/download.html)
#'
#' @return Returns the filepath to the detections file
#' @export
#'
#' @examples
#' \dontrun{
#' run_model(video = "path/to/video.mp4")
#' }
#'
#' @importFrom tools file_ext file_path_sans_ext
#' @importFrom plyr llply
#' @import reticulate
#' @import magrittr

run_Model <- function(
    video,
    detections_path = NULL,
    model = NULL,
    save_vid = F,
    save_path = NULL,
    gopro = FALSE) {

  # Check for an active project
  if (is.null(get_Project())) {
    stop("No active TrackR project found.\n  Set an active project with `set_Project()` then try again.")
  }

  ## Handle the video file path ----
  # Check video exists
  if (!file.exists(video)) {
    stop("Video not found at specified path.\n  Please check the `video` parameter and try again.")
  }

  # Check if the path supplied is a single file or directory
  if (tools::file_ext(video) == "") {

    # If the path is a directory, list the files within it
    videos <- list.files(video, full.names = T)

    # Check that all files in the directory are video files
    if (!any(sapply(videos, .is_video))) {
      stop("Non-video files found at specified path.\n  Please ensure the path points only to video files")
    }
  } else {
    # Change the name of video to videos for consistency downstream
    videos <- video
  }

  ## Handle save path ----
  if (!is.null(save_path)) {
    if (!save_vid) {
      stop("`save_path` provided but video is not being saved.\n  Did you mean to set `save_vid` to TRUE?")
    }

    if (!file.exists(dirname(save_path))) {
      stop("Directory for `save_path` does not exist.\n Please save video into an existing directory.\n You can create the directory by running `dir.create(dirname(save_path))`")
    }
  }

  ## Get model weights path
  if (is.null(model)) {
    model <- list.files(file.path(get_Project(), "YOLO", "models"))[1]
  }

  if (tools::file_ext(model) == "") { # If a model name is supplied get the corresponding model weights
    proj <- get_Project()

    model_dir <- file.path(proj, "YOLO", "models", model)

    if (!dir.exists(model_dir)) {
      available_models <- list.files(file.path(proj, "YOLO", "models"))
      available_models <- available_models[tools::file_ext(available_models) == ""]
      available_models <- paste(available_models, collapse = ", ")
      stop(paste0("Model '", model, "' not found.\n Ensure the name supplied matches the name of a trained model available in the poject 'YOLO/models/' directory.\n Available options are: ", available_models))
    }

    weights <- file.path(model_dir, "weights", "best.pt")

    if (!file.exists(weights)) {
      stop(paste0("Model weights '", weights, "' not found.\n  Check the model directory for the model weights and try again.\n  You may need to provide the path to the '.pt' weights file directly if you moved or renamed it."))
    }

  } else if (tools::file_ext(model) == "pt") {
    weights <- model

    if (!file.exists(weights)) {
      stop(paste0("Model weights '", weights, "' not found.\n Check the filepath supplied and try again."))
    }
  } else {
    available_models <- list.files(file.path(proj, "YOLO", "models"))
    available_models <- available_models[tools::file_ext(available_models) == ""]
    available_models <- paste(available_models, collapse = ", ")
    stop(paste0("Invalid `model` argument.\n  Available models are: ", available_models, ".\n  Please choose one of these or specify the path to a '.pt' model weights file directly"))
  }


  ## Handle detections path ----


  if (is.null(detections_path)) { # Construct detections path if it is not provided (DEFAULT)
    detections_dir <- file.path(get_Project(), "Detections")

    if (!dir.exists(detections_dir)) {
      cat("Creating detections directory at '{project}/Detections'...")
      dir.create(detections_dir)
    }

    detections_csv <- video %>%
      basename() %>%
      tools::file_path_sans_ext(.) %>%
      paste0(., ".csv")

    detections_path <- file.path(detections_dir, detections_csv)
  } else if (tools::file_ext(detections_path) == "csv") { # If a csv file is provided warn users if saving at non default location and use path

    if (dirname(detections_path) == ".") { # If just a filename is provided, construct default project path

      detections_dir <- file.path(get_Project(), "Detections")

      if (!dir.exists(detections_dir)) {
        cat("Creating detections directory at '{project}/Detections'...")
        dir.create(detections_dir)
      }

      detections_path <- file.path(detections_dir, detections_path)

    } else { # If a full path is provided use it with a warning is it is a non-default location

      if (!dir.exists(dirname(detections_path))) {
        cat(paste0("Creating detections directory at '", normalizePath(dirname(detections_path), mustWork = FALSE), "'..."))
        dir.create(dirname(detections_path))
      }

      if (normalizePath(dirname(detections_path)) != normalizePath(file.path(get_Project(), "Detections"))) {
        warning("Supplied detections directory is in a non-standard location.")
      }

      detections_path <- normalizePath(detections_path)
    }

  } else {
    stop("Invalid `detections_path` argument.\n  Please leave the default (`NULL`) to save detections in the default way or provide the name of a csv file where detections should be saved.")
  }

  ## Run model ----
  invisible(.prepare_csv(detections_path))

  invisible(
    plyr::llply(
      videos,
      function(v, g = gopro) {
        # Handle gopro video
        if (g) {
          v <- .reformat_gopro(vid = v)
        }

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


#' INTERNAL Prepare detections csv file
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
#'
#' @importFrom utils write.table
#' @importFrom tools file_path_sans_ext file_ext

.prepare_csv <- function(path) {
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
  utils::write.table(t(columns), file = path, sep = ",", col.names = FALSE, row.names = FALSE, quote = FALSE, append = TRUE)

  return(path)
}


#' INTERNAL reformat gopro video
#'
#' @description
#' This is an internal function to reformat gopro videos by removing the audio
#' to allow them to be read by opencv. Saves the reformatted video to a
#' specified location which by default is a temporary file.
#'
#' @param vid Filepath to the video to be reformatted
#' @param reformatted Filepath at which to save reformatted video. Temporary
#'  file location by default
#'
#' @return Path to the temporary video file
#' @noRd

.reformat_gopro <- function(vid, reformatted = paste0(tempfile(), ".mp4")) {

  if (Sys.which("ffmpeg") == "") {
    stop("FFMPEG is not installed or not found on system PATH.\n  Please install it and try again.\n  FFMPEG download: https://ffmpeg.org/download.html")
  }

  cat("Reformatting video...")

  invisible(system(
    paste0("ffmpeg -i ", normalizePath(vid), " -y -map 0:v -c copy ", reformatted),
    intern = F, show.output.on.console = F
  ))

  if (!file.exists(reformatted)) {
    stop("Error in video reformatting.")
  }

  return(reformatted)
}
