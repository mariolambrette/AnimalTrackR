#' Fit a HMM to classify behavioural state of AnimalTrackR detections
#'
#' @description
#' This function takes an AnimalTrackR detection file and fits a Hidden Markov
#' Model to the bounding coordinates within it to classify behaviour into active
#' and inactive states. Users supply the path to a detection file and a desired
#' resolution for behavioural state classification. The updated detection file
#' with behavioural states supplied in the "State" column is then saved in the
#' original location.
#'
#' To update a detection file where behavioural states have already been
#' calculated, set the `overwrite` parameter to TRUE. Otherwise, the function
#' will not refit states and will throw a warning.
#'
#'
#' @param detections path to an AnimalTrackR detection file. These are the csv
#'  files stored in the Detections/ directory of an AnimalTrackR project.
#' @param state_fps The frame rate at which behavioural classification should
#'  occur. The default is 3. Some applications where behaviour is more finely
#'  resolved than this may benefit from a higher classification frame rate but
#'  in most cases this will add unnecessary noise.
#' @param overwrite T/F. If TRUE, existing behavioural state classifications for
#'  the provided detection file will be overwritten.
#'
#' @returns a dataframe of detections with a 'state' column showing the
#'  behavioural state. Also saves the updated detections file with a 'State'
#'  column into the original location.
#' @export
#'
#' @examples
#' \dontrun{
#'  fit_HMM(
#'    detections = "Detections/Test_detections",
#'    state_fps = 3
#'  )
#' }
#'
#' @import magrittr
#' @import moveHMM
#' @importFrom dplyr filter pull mutate
#' @importFrom tidyr fill
#' @importFrom stats sd
#' @importFrom utils write.csv

fit_HMM <- function(detections, state_fps = 3, overwrite = F) {


  # 1. Read detection file ----

  # read detections
  dets_raw <- .read_detections(detections)

  if ("State" %in% colnames(dets_raw)) {
    if (!overwrite) {
      warning(
        "Supplied detection file already contains behavioural states. ",
        "To overwrite these set the `overwrite` parameter to TRUE.", call. = FALSE)
      return(invisible(NULL))
    }
  }

  # Add identifier column
  dets_raw <- dets_raw %>%
    dplyr::mutate(id = seq(1:nrow(dets_raw)))


  # 2. Down sample to required rate ----

  # Calculate frame rate of detections
  dets_fps <- get_detections_fps(dets_raw)

  hmm.data <- .downsample_to_fps(
    df = dets_raw,
    frame_col = "Frame",
    dets_fps = dets_fps,
    state_fps = state_fps
  )


  # 3. Prep data for HMM fitting ----

  # Format data
  hmm.data <- moveHMM::prepData(
    trackData = hmm.data,
    type = 'UTM',
    coordNames = c('xc', 'yc'),
    LLangle = FALSE
  ) %>%
    dplyr::filter(step != 0)

  # Calculate the mean and standard deviation for step length and turning angle for
  # data above and and below the threshold (1 = under threshold, 2 = over threshold)

  threshold <- 100

  st.mu.1 <- hmm.data %>%
    filter(step < threshold) %>%
    pull(step) %>%
    mean(na.rm = T)

  st.mu.2 <- hmm.data %>%
    filter(step >= threshold) %>%
    pull(step) %>%
    mean(na.rm = T)

  st.sd.1 <- hmm.data %>%
    filter(step < threshold) %>%
    pull(step) %>%
    sd(na.rm = T)

  st.sd.2 <- hmm.data %>%
    filter(step >= threshold) %>%
    pull(step) %>%
    sd(na.rm = T)

  an.mu.1 <- hmm.data %>%
    filter(step < threshold) %>%
    pull(angle) %>%
    mean(na.rm = T)

  an.mu.2 <- hmm.data %>%
    filter(step >= threshold) %>%
    pull(angle) %>%
    mean(na.rm = T)

  an.sd.1 <- hmm.data %>%
    filter(step < threshold) %>%
    pull(angle) %>%
    sd(na.rm = T)

  an.sd.2 <- hmm.data %>%
    filter(step >= threshold) %>%
    pull(angle) %>%
    sd(na.rm = T)

  # 4. Fit HMM
  mod <- moveHMM::fitHMM(
    data = hmm.data,
    nbStates = 2,
    stepPar0 = c(st.mu.1, st.mu.2, st.sd.1, st.sd.2),
    anglePar0 = c(an.mu.1, an.mu.2, an.sd.1, an.sd.2),
    formula = ~1, # No covariate effects
    stepDist = 'gamma',
    angleDist = 'vm'
  )

  # 5. Decode states of original detections
  hmm.data <- hmm.data %>%
    dplyr::mutate(State = moveHMM::viterbi(mod))

  # 6. Return dataframe in original format with added states
  # Bind states in to full resolution data
  dets_raw <- dets_raw %>%
    dplyr::left_join(hmm.data %>% select(c("id", "State")), by = "id") %>%
    tidyr::fill(State, .direction = "downup") %>%
    dplyr::select(-id)

  # Save the behavioural state detections into the original location
  write.csv(
    dets_raw,
    file = detections,
    row.names = F
  )


  return(dets_raw)
}







#' Calculate the frame rate of a detections file
#'
#' @param detections Either a file path to an AnimalTrackR detection file or a dataframe of AnimalTrackR detections in the same format as the csv files.
#'
#' @returns the FPS of recorded detections in the provided detection file
#' @export
#'
#' @examples
#' \dontrun{
#' # From file
#' get_detection_fps("Detections/test_detections.csv")
#'
#' # From dataframe
#' detections <- data.table::fread("Detections/test_detections.csv")
#' get_detections_fps(detections)
#' }
get_detections_fps <- function(detections) {

  if (is.character(detections)) {

    if (file.exists(detections)) {
      detections <- .read_detections(detections)
    } else {
      stop("`detections` must be a path an AnimalTrackR detection file or a dataframe of detections in the same format")
    }

  } else if (!is.data.frame(detections)) {
    stop("`detections` must be a path an AnimalTrackR detection file or a dataframe of detections in the same format")
  }

  fps <- .calc_dets_frame_rate(
    frame = detections$Frame,
    timestamp = detections$Timestamp
  )

  return(round(fps))

}



#' INTERNAL Check the format of detections dataframe
#'
#' @param detections_df A dataframe of model detections, read from a detections csv file
#'
#' @returns T/F depending on whether the detections are formatted correctly
#'
.check_detections <- function(detections_df) {

  # read column names
  cols <- colnames(detections_df)

  # Minimum required column names
  req.cols <- c("Video",
                "Frame",
                "Timestamp",
                "xc", "yc",
                "xl", "xr",
                "yt", "yb")

  # Check all columns present
  if (!all(req.cols %in% cols)) {
    return(F)
  }

  # Check detections present
  if (nrow(detections_df) == 0) {
    return(F)
  }

  return(T)

}


#' INTERNAL Read a detections csv file in as a dataframe
#'
#' @param detections_path Path to csv of model detections
#'
#' @returns A dataframe of model detections
#'
#' @importFrom data.table fread
.read_detections <- function(detections_path) {

  if (file.exists(detections_path)) {
    d <- data.table::fread(detections_path)

    if (.check_detections(d)) {
      return(d)
    } else {
      stop("Specified detections file does not meet the correct format requirements. Please check the file and try again.")
    }
  } else {
    stop("Specified detections file not found. Please check the file path and try again")
  }

}


#' INTERNAL Calculate median frame rate from frame numbers and timestamps in detection dataframe
#'
#' @param frame Either a numeric vector of frame numbers or a dataframe
#'              containing the frame column.
#' @param timestamp If `frame` is a vector, a numeric vector of timestamps
#'                  in milliseconds. If `frame` is a dataframe, this argument
#'                  should be the name of the timestamp column (string).
#' @param frame_col If `frame` is a dataframe, the name of the frame column (string).
#'
#' @return Median frame rate (frames per second)
#'
#' @examples
#' \dontrun{
#'   df <- data.frame(frame = 1:10, timestamp = seq(0, 900, by = 100))
#'   .calc_dets_frame_rate(df, "frame", "timestamp")
#' }
#'
#'
#' @importFrom stats median

.calc_dets_frame_rate <- function(frame, timestamp = NULL, frame_col = NULL) {
  # Case 1: frame is a dataframe
  if (is.data.frame(frame)) {
    if (is.null(frame_col) || is.null(timestamp)) {
      stop("If 'frame' is a dataframe, provide 'frame_col' and 'timestamp' as column names.")
    }
    frame_vec <- frame[[frame_col]]
    ts_vec <- frame[[timestamp]]
  } else {
    # Case 2: frame and timestamp are vectors
    frame_vec <- frame
    ts_vec <- timestamp
  }

  # Remove any NA rows
  keep <- !(is.na(frame_vec) | is.na(ts_vec))
  frame_vec <- frame_vec[keep]
  ts_vec <- ts_vec[keep]

  # Ensure numeric
  frame_vec <- as.numeric(frame_vec)
  ts_vec <- as.numeric(ts_vec)

  # Order by frame number
  ord <- order(frame_vec)
  frame_vec <- frame_vec[ord]
  ts_vec <- ts_vec[ord]

  # Calculate diffs
  frame_diffs <- diff(frame_vec)
  time_diffs_sec <- diff(ts_vec) / 1000

  # Avoid division by zero
  valid <- time_diffs_sec > 0

  return(median(frame_diffs[valid] / time_diffs_sec[valid]))
}


#' INTERNAL Downsample detections to a target frame rate
#'
#' @param df Dataframe containing detection data
#' @param frame_col Name of frame number column (string)
#' @param dets_fps Current frame rate of detections
#' @param state_fps Desired frame rate
#'
#' @return Downsampled dataframe
.downsample_to_fps <- function(df, frame_col, dets_fps, state_fps) {
  if (state_fps > dets_fps) {
    warning("Desired frame rate is higher than detection frame rate; returning original data.")
    return(df)
  }

  step <- dets_fps / state_fps  # how many original frames to skip
  keep_frames <- round(seq(
    from = min(df[[frame_col]], na.rm = TRUE),
    to   = max(df[[frame_col]], na.rm = TRUE),
    by   = step
  ))

  df[df[[frame_col]] %in% keep_frames, ]
}





