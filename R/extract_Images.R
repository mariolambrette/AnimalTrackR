#' Extract images from video data
#'
#' @description
#' This is the first stage of the tracking model development workflow.
#' extract_Images() is used to extract frames from a full video dataset so that
#' they can be annotated and used for training and testing YOLO object detection
#' models.
#'
#' @param videos A nested list of file paths to experimental videos. The videos
#'   recorded in most experiments will form natural groups (i.e. different
#'   experimental/treatment groups). These should be listed in separate elements
#'   in the `videos` list. For example, a simple experiment may have two groups,
#'   a treatment and a control, if each group contained 5 individuals and there was
#'   one monitoring video per individual, the structure of the `videos` object would
#'   be as follows:
#'   * videos <- list(Treatment = list(vid1.mp4, vid2.mp4, ..), Control = list(vid6.mp4, ...)).
#'
#'   In cases where there is chapterised video monitoring a single individual (e.g.
#'   when the monitoring period is very long), the parent directory of the video chapters
#'   can be used as the list elements rather than the path to the video files themselves.
#'
#'   AnimalTrackR is compatible with most commonly used video formats. All image
#'   processing relies on the opencv python package so more information regarding
#'   possible compatibility issues can be found in the [package documentation](https://docs.opencv.org/4.x/)
#'
#' @param group_weights Sample size weights to be applied to each experimental group.
#'
#'   By default `extract_Images()` will weight each group equally, i.e. if there
#'   are two groups 50% of exported images will come from each group. There are
#'   a number of reasons why this behaviour may not be optimal. For example, if
#'   the number/length of video sin each group are uneven or a detection model
#'   performs particularly poorly on one group.
#'
#'   In order to specify group weights manually, users should pass a names list of
#'   the same length (and with the same names) as `videos` where each element is a
#'   decimal denoting the weight to apply to that group and the sum of the elements
#'   is equal to 1.
#'
#' @param nimgs Numeric. The total number of images to export from the video dataset.
#'
#'   The default, 1600, will provide about 1000 training images and 300 images for
#'   test and validation respectively, following a 60/20/20 training/testing/validation
#'   split. This is a rough estimate of the number of images that will produce
#'   good model performance in may cases, however users will need to adapt this
#'   bbased on their individual needs and development model performance. Increasing
#'   the number of training images will almost always lead to improved model performance
#'   (with diminishing returns as the number of images increases), though it is
#'   important to balance with against the time & resource cost of annotating
#'   more images.
#' @param vid_ext Character. The file extension of the video files.
#'
#'   If the video file extension is one of : '.mp4', '.avi', '.mov', '.mkv', '.wmv',
#'  '.flv', '.webm', this parameter cna be ignored. In other cases users should
#'  provide the file extension here.
#'
#'  The above list of formats are fully supported, other formats are also likely
#'  to be supported but some features may fail so users should proceed with caution.
#'
#'  See the [opencv-python](https://docs.opencv.org/4.x/) documentation for more
#'  details on compatibility
#'
#' @param project The TrackR project to uses. By default the current active project
#'   (retrieved with `get_Project()`) is used. This will be the correct behaviour
#'   in the vast majority of cases so most users can ignore this parameter.
#'
#' @return invisibly returns TRUE
#' @export
#'
#' @examples
#' \dontrun{
#' extract_Images(
#'  videos = list(
#'    Group1 = list(
#'      "video1.mp4",
#'      "video2.mp4",
#'      "video3.mp4"
#'    ),
#'    Group2 = list(
#'      "video4.mp4",
#'      "video5.mp4",
#'      "video6.mp4"
#'    )
#'  ),
#'  group_weights = list(
#'    Group1 = 0.3,
#'    Group2 = 0.7
#'  ),
#'  nimgs = 1600
#' )
#' }


extract_Images <-
  function(videos,
           group_weights = NULL,
           nimgs = 1600, # ~ 1000 training images, 300 testing 300 validation
           vid_ext = NULL,
           project = get_Project()){


    # Check python environment meets requirements
    if(!check_TrackR_env()){
      stop("Conda environment does not meet requirements.\n
            Please activate a suitable conda envirnoment using `set_TrackR_env()`, or create one with `create_TrackR_env()`")
    }

    if(!is.null(group_weights)){

      # Check names of video and weights lists match
      if(!identical(sort(names(videos)), sort(names(group_weights)))){
        stop("Group names in video and weights list do not match.\n
              Please check and try again")
      }

      # Check group weights are decimals and sum to 1
      if(sum(as.numeric(group_weights)) != 1){
        stop("Group weights must sum to 1")
      }
    } else{
      ## MANUALLY CALCULATE GROUP WEIGHTS BASED ON RELATIVE NUMBER OF VIDEOS ##
    }

    if(!is.numeric(nimgs)){
      stop("`nimgs` is not numeric")
    }



    if(!py_extractImages$TestingPython()){
      stop("Error in python environment. \n
           Please check your conda configuration, restart your R session and try again.")
    }

    # Extract Images using dedicated python function
    py_extractImages$extract(
      vids = reticulate::r_to_py(videos),
      weights = reticulate::r_to_py(group_weights),
      n = reticulate::r_to_py(as.integer(nimgs)),
      path = file.path(project, "ToAnnotate"),
      vid_ext = reticulate::r_to_py(vid_ext)
    )

    invisible(T)
  }




# videos <- list(
#   Group1 = list(
#     "video1.mp4",
#     "video2.mp4",
#     "video3.mp4"
#   ),
#   Group2 = list(
#     "video4.mp4",
#     "video5.mp4",
#     "video6.mp4"
#   )
# )
#
# group_weights <- list(
#   Group1 = 0.4,
#   Group2 = 0.6
# )
