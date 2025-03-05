#' Count the number of annotated images in a project.
#'
#' @description
#' `CountImages()` counts the number of annotated images that have been exported
#' to the YOLO directory of a TrackR project. The default behaviour will count
#' images in the current active project, found using `get-Project()`, though the
#' path to any TrackR project can be passed to the `project` argument instead.
#'
#' The function returns a bar plot of the number of images in each set  required
#' for model development (Train, Test and Val).
#'
#'
#' @param project path to a TrackR project directory
#'
#' @return a ggplot object showing the number of images in each set in the specified project
#' @export
#'
#' @examples
#' \dontrun{
#' # Count images in the currently active project
#' CountImages()
#'
#' # Count images in another project
#' CountImages(project = "path/to/project/root")
#' }
#'

CountImages <- function(project = get_Project()){

  if (is.null(project)) {
    stop("Project not set. \n
          Use `set-Project()` to activate a TrackR project")
  }

  n <- data.frame(
    Train = length(list.files(file.path(project, "YOLO", "Train", "images"))),
    Test = length(list.files(file.path(project, "YOLO", "Test", "images"))),
    Val = length(list.files(file.path(project, "YOLO", "Val", "images")))
  ) %>%
    t() %>%
    as.data.frame() %>%
    tibble::rownames_to_column(var = "Type") %>%
    dplyr::rename(num = V1) %>%
    dplyr::mutate(Type = factor(Type, levels = c("Train", "Test", "Val")))

  p <- ggplot2::ggplot(data = n, ggplot2::aes(x = Type, y = num, fill = Type)) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::scale_fill_manual(values = c(Train = "#00CCCC", Test = "#FFD700", Val = "#FF6B6B")) +
    ggplot2::labs(x = "",
                  y = "Number of images",
                  title = "Number of annotated images",
                  fill = "Set") +
    ggplot2::scale_y_continuous(breaks = scales::breaks_pretty(n = 5)) +
    ggplot2::theme_bw()

  p

  return(p)

}
