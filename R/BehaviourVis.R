#' Visualise behavioural states via coloured bounding boxes
#'
#' @description
#' Using a custom classification function, classify detections into behavioural
#' states and visualise these states in the original video via coloured bounding
#' boxes representing each detection.
#'
#' This is a useful tool for visualising behavioural state classifications
#'
#'
#' @param detections_path Path to an AnimalTrackR detection file
#' @param vid_path Path to the video file corresponding to the detections file
#' @param output_path File path at which to save the output visualisation (MP4 format)
#' @param classification_function A custom function to classify detections into
#'    behavioural states for visualisation.
#'
#'    The function may have as many inputs as needed, however, one must be
#'    `detections`, taking an R dataframe corresponding to an AnimalTrackR detection
#'    file. For testing purposes, the dataframe is read internally using
#'    `data.table::fread(detections_path)`.
#'
#'    The output of the `classification_function` should be a dataframe of the same
#'    format as the input dataframe but with additional columns. One of these
#'    should be a categorical column representing the behavioural state. Up to 10
#'    behavioural states are supported. The name of this categorical column must
#'    be specified in the `class_column` parameter.
#'
#'    Optionally, if the saved detection file has already been classified this
#'    parameter can be ignored and the the column name can be specified in the
#'    `class_column` parameter.
#'
#'    You must ensure that all dependencies required by the classification function
#'    are loaded into your R session.
#'
#' @param class_column character string. The name of the column in the detections
#'    dataframe containing the behavioural states (either as an output of the
#'    `classification_function` or a previously computed column saved in the
#'    specified csv file). The default - "State" is the column computed by the
#'    provided function `fit_HMM()`.
#' @param ... Additional parameters to be passed to the `classification_function`
#'
#' @return Returns the detections data as a dataframe with the class column added
#' @export
#'
#' @examples
#' \dontrun{
#'
#'   classifier <- function(detection){
#'     # Custom classification function
#'     # e.g. assign points to clusters
#'   }
#'
#'   behaviour_viz(
#'     detections_path = "path/to/TrackRDetections.csv",
#'     vid_path = "path/to/video",
#'     output_path = "path/to/save/output/mp4",
#'     classification_function = classifier
#'   )
#' }
#'
#' @importFrom data.table fread
#' @import reticulate

behaviour_viz <- function(
    detections_path,
    vid_path,
    output_path,
    classification_function = NULL,
    class_column,
    ...
  ){

  # Read detections
  dets <- data.table::fread(detections_path)

  # Classify the detections if necessary
  if (!is.null(classification_function)) {
    print("classifying detections")
    dets <- do.call(
      classification_function,
      args = c(list(detections = dets), ...)
    )
  }

  # Error if class_column doesn't exist
  if (!(class_column %in% colnames(dets))) {
    stop("`class_column` not found in detections data.
          Please check the `class_column` parameter and try again.")
  }

  # Error if class_column has more than 10 categories
  if (length(unique(dets[[class_column]])) > 10) {
    stop("More than 10 unique behavioural categories found in `class_column`.")
  }

  # Error if output path directory does not exist
  if (!dir.exists(dirname(output_path))) {
    stop("Ouput path directory does not exist.
          Please use an existing directory for the output video and try again")
  }

  # Error if video file does not exist
  if (!file.exists(vid_path)) {
    stop("Video `vid_path` not found.
          Please check the video path and try again.")
  }

  # Use the dedicated python function to produce a video with coloured bounding boxes
  py_BehaviourVis$behaviour_vis(
    detections = reticulate::r_to_py(dets),
    video_path = reticulate::r_to_py(vid_path),
    output_path = reticulate::r_to_py(output_path),
    class_column = reticulate::r_to_py(class_column)
  )

  return(dets)
}
