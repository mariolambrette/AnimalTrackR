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

.is_video <- function(path){

  tryCatch({
    # Try to get file info
    info <- av::av_media_info(path)

    # Check if the video has frames
    if (is.na(info$video["frames"])) {
      return(FALSE)
    } else {
      return(TRUE)
    }
  }, error = function(e) {
    # return FALSE is an error occurs (the file is not a video)
    return(FALSE)
  })
}
