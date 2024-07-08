#' Save annoations into project directory
#'
#' @description
#' `SaveAnnotations()` takes a csv file of bounding box annotations created with
#' [Make Sense](https://www.makesense.ai/) and converts them into YOLO format and
#' saves the relevant annotation files and images into the project directory.
#' It should be used after completing each annotating session.
#'
#' As well as saving the completed annotations the function will remove annotated
#' images from the 'ToAnnotate' folder, ensuring that users do not annotate the
#' same image twice if they are completing annotations over multiple sessions.
#'
#' @param csv path to the csv file exported from [Make Sense](https://www.makesense.ai/).
#'
#' @return invisibly return TRUE
#' @export
#'
#' @examples
#' \dontrun{
#' # After creating annotations in Make Sense, exporting them to a csv file and
#' # saving that file to "some/path/annotations.csv":
#'
#' SaveAnnotations(csv = "some/path/annotation.csv")
#' #
#' }

SaveAnnotations <- function(csv){

  # Read class index
  class_index <- data.frame(
    label_name = readLines(file.path(get_Project(), "YOLO", "configs", "labels.txt")),
    index = c(0,1,2)
  )

  # Read annotation file and normalise coordinates to YOLO format (0-1 scale)
  annots <- utils::read.csv(csv, header = T) %>%
    dplyr::mutate(
      bbox_x = bbox_x/image_width,
      bbox_y = bbox_y/image_height,
      bbox_height = bbox_height/image_height,
      bbox_width = bbox_width/image_width
    ) %>%
    dplyr::left_join(class_index, by = "label_name")

  # classify image into training, testing and validation sets
  sets <- data.frame(
    image_name = unique(annots$image_name),
    set = sample(
      x = c("Train", "Test", "Val"),
      size = length(unique(annots$image_name)),
      replace = TRUE,
      prob = c(0.6, 0.2, 0.2)
    )
  )

  plyr::llply(
    as.list(sets$image_name),
    function(name) {
      set <- sets$set[sets$image_name == name][1]
      annos <- annots %>%
        dplyr::filter(image_name == name) %>%
        dplyr::mutate(lines = paste(index, bbox_x, bbox_y, bbox_width, bbox_height, sep = " "))

      if(nrow(annos) > 1){
        # Check if more than one target is annotated
        if(sum(annos$label_name == "Target") > 1){
          message(paste0("Skipping image: ", name, ". Contains more than 1 target annotation."))
          return(invisible(T))
        }

        # Check if 'Empty' Empty appears alongside other annotations
        if("Empty" %in% annos$label_name){
          message(paste0("Skipping image: ", name, ". 'Empty' label is not alone."))
          return(invisible(T))
        }
      }

      # Call the SaveImage function
      SaveImage(name, set, annos %>% dplyr::select(lines))
    },
    .progress = "text"
  )

  return(invisible(T))

}



#' Convert single image annotations to YOLO format
#'
#' @description
#' Internal function used with `plyr::llply` loop to save individual images in YOLO
#' format with annotations into correct train/text/val folder
#'
#'
#' @param name file name of the image in the 'ToAnnotate' folder
#' @param set The train/test/val set to put the image in to
#' @param annos annotations related to the image
#'
#' @return invisibly returns TRUE
#' @noRd
#'
#' @examples
#' \dontrun{
#' plyr::llply(
#'  as.list(sets$image_name),
#'  SaveImage,
#'  .progress = "text"
#' )
#' }
#'

SaveImage <-
  function(name,
           set,
           annos){

    # Create neccesary file paths
    img_path <- file.path(trackr_env$proj, "ToAnnotate", name)
    img_dest <- file.path(trackr_env$proj, "YOLO", set, "images", name)
    lab_dest <- file.path(trackr_env$proj, "YOLO", set, "labels", paste0(tools::file_path_sans_ext(name), ".txt"))

    # Copy image to correct directory (may need to be adjusted to use a python function
    # to allow the image to be resized)
    file.copy(from = img_path, to = img_dest)

    # Save the annotation file
    writeLines(annos$lines, con = lab_dest)

    # Delete the image in the 'ToAnnotate' folder
    file.remove(img_path)

    return(invisible(T))
}

