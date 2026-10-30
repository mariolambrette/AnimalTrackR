#' Save annotations into project directory
#'
#' @description
#' `save_annotations()` takes a csv file of bounding box annotations created with
#' [Make Sense](https://www.makesense.ai/) and converts them into YOLO format and
#' saves the relevant annotation files and images into the project directory.
#' It should be used after completing each annotating session.
#'
#' As well as saving the completed annotations the function will remove annotated
#' images from the 'ToAnnotate' folder, ensuring that users do not annotate the
#' same image twice if they are completing annotations over multiple sessions.
#'
#' @details
#' **Splitting into training, validation and test sets**
#'
#' By default (`split_by = "video"`) images are allocated to the training,
#' validation and test sets by *source video*, so that all frames from a given
#' video end up in the same set. Frames from the same video are often very
#' similar, so allowing them into both the training and test sets (a random
#' frame-level split) tends to give over-optimistic performance estimates.
#'
#' The source video is read from the file name written by [extract_images()]
#' (`{group}_{video}_{frame}.jpg`), so images extracted with AnimalTrackR need
#' no extra input. For images named in some other way, use `group_pattern`
#' (see below). Images whose source cannot be identified are allocated
#' individually, with a message saying how many.
#'
#' **Choosing a `group_pattern`**
#'
#' `group_pattern` tells AnimalTrackR which part of each file name identifies
#' the video, individual or tank an image came from. It is a
#' [regular expression][base::regex]: images whose file names give the same
#' match are kept together. The match is made against the file name without
#' its extension (e.g. `ZF15_pre_000123` for `ZF15_pre_000123.jpg`).
#'
#' A few building blocks cover most cases:
#'
#' * `^` means "start of the file name"
#' * `[0-9]+` means "one or more digits"
#' * `[A-Za-z]+` means "one or more letters"
#' * any other letters, numbers, `_` and `-` match themselves
#'
#' Examples:
#'
#' | File names | To group by | `group_pattern` | Gives |
#' |---|---|---|---|
#' | `ZF15_pre_000123.jpg`, `ZF15_1h_004410.jpg` | fish | `"^ZF[0-9]+"` | `ZF15` |
#' | `ZF15_pre_000123.jpg`, `ZF15_1h_004410.jpg` | video (fish and timepoint) | `"^ZF[0-9]+_[A-Za-z0-9]+"` | `ZF15_pre`, `ZF15_1h` |
#' | `tank03_2024-05-01_0153.jpg` | tank | `"^tank[0-9]+"` | `tank03` |
#' | `01_B1-02-2023-11-02-151006.mp4_0010000.jpg` | tank | `"^[0-9]+_B1-[0-9]+"` | `01_B1-02` |
#'
#' Where the same animal appears in several videos, grouping by individual is
#' stricter than grouping by video, and gives a fairer test of how the model
#' will perform on new animals.
#'
#' To check a pattern before using it, run it on a few of your file names:
#'
#' ```
#' files <- list.files("path/to/images")
#' unique(regmatches(files, regexpr("^ZF[0-9]+", files)))
#' ```
#'
#' This should list one entry per video or individual. The message printed by
#' `save_annotations()` / `import_annotations()` also reports how many groups
#' were found and how many images could not be matched.
#'
#' Videos that already have images in the project (from earlier annotation
#' sessions) keep their existing allocation, so a video never spans sets.
#' New videos are shuffled and allocated so that the proportion of *images* in
#' each set is as close as possible to `split`. With few videos the realised
#' proportions can differ noticeably from `split`; the realised numbers are
#' reported in a message.
#'
#' `split_by = "image"` reproduces the original behaviour of allocating each
#' image independently, which is not recommended for model evaluation.
#'
#' @param csv path to the csv file exported from [Make Sense](https://www.makesense.ai/).
#' @param split_by Either `"video"` (default) or `"image"`. See Details.
#' @param split Named numeric vector giving the target proportion of images in
#'   the `Train`, `Val` and `Test` sets. Must sum to 1.
#' @param group_pattern Optional. A regular expression that picks out the part
#'   of each file name identifying its video, individual or tank, e.g.
#'   `"^ZF[0-9]+"` to group `ZF15_pre_000123.jpg` by fish (`ZF15`). Not needed
#'   for images extracted with [extract_images()]. Only used when
#'   `split_by = "video"`. See "Choosing a `group_pattern`" in Details for
#'   more examples and how to check a pattern.
#' @param seed Optional integer random seed, for a reproducible split.
#'
#' @return invisibly return TRUE
#' @export
#'
#' @examples
#' \dontrun{
#' # After creating annotations in Make Sense, exporting them to a csv file and
#' # saving that file to "some/path/annotations.csv":
#' save_annotations(csv = "some/path/annotation.csv", seed = 1)
#' }
#'
#' @import dplyr
#' @importFrom data.table fread

save_annotations <- function(csv,
                             split_by = c("video", "image"),
                             split = c(Train = 0.6, Val = 0.2, Test = 0.2),
                             group_pattern = NULL,
                             seed = NULL){

  split_by <- match.arg(split_by)
  split <- .check_split(split)

  # Read class index
  class_index <- data.frame(
    label_name = readLines(file.path(get_Project(), "YOLO", "configs", "labels.txt")),
    index = c(0,1,2)
  )

  # Read annotation file and normalise coordinates to YOLO format (0-1 scale)
  annots <- data.table::fread(csv, header = T) %>%
    dplyr::mutate(
      bbox_x = (bbox_x + bbox_width/2) / image_width,
      bbox_y = (bbox_y + bbox_height/2) / image_height,
      bbox_height = bbox_height / image_height,
      bbox_width = bbox_width / image_width
    ) %>%
    dplyr::left_join(class_index, by = "label_name")

  # classify images into training, testing and validation sets
  sets <- .assign_sets(
    image_names = unique(annots$image_name),
    split_by = split_by,
    split = split,
    group_pattern = group_pattern,
    seed = seed,
    project = get_Project()
  )

  plyr::llply(
    as.list(sets$image_name),
    function(name) {
      set <- sets$set[sets$image_name == name][1]
      annos <- annots %>%
        dplyr::filter(image_name == name) %>%
        dplyr::mutate(lines = paste(index, bbox_x, bbox_y, bbox_width, bbox_height, sep = " "))

      if (nrow(annos) > 1) {
        # Check if more than one target is annotated
        if (sum(annos$label_name == "Target") > 1) {
          message(paste0("Skipping image: ", name, ". Contains more than 1 target annotation."))
          return(invisible(T))
        }

        # Check if 'Empty' Empty appears alongside other annotations
        if ("Empty" %in% annos$label_name) {
          message(paste0("Skipping image: ", name, ". 'Empty' label is not alone."))
          return(invisible(T))
        }
      }

      # Call the .save_image function
      .save_image(name, set, annos %>% dplyr::select(lines))
    },
    .progress = "text"
  )

  return(invisible(T))

}



#' Import existing YOLO-format annotations into a project
#'
#' @description
#' `import_annotations()` adds images that have already been annotated in YOLO
#' format (for example with another annotation tool, or in an earlier project)
#' to the active TrackR project, allocating them to the training, validation and
#' test sets in the same way as [save_annotations()].
#'
#' Images and labels are copied, the originals are left untouched. Each image
#' must have a label file with the same base name (e.g. `img1.jpg` and
#' `img1.txt`); images without a label file are skipped with a message. Class
#' indices in the label files must match the project's classes
#' (`0` = Target, `1` = ZZZ), or be remapped with `class_map`.
#'
#' Images from other sources are rarely named in the `extract_images()` format,
#' so you will usually need `group_pattern` to keep images from the same video
#' or individual together. See "Choosing a `group_pattern`" in
#' [save_annotations()] for examples.
#'
#' @param images Path to a directory of images (.jpg, .jpeg or .png).
#' @param labels Path to a directory of YOLO-format label (.txt) files.
#' @param class_map Optional named vector to remap class indices, where names
#'   are the indices in the supplied labels and values are the project indices,
#'   e.g. `c("0" = 0, "3" = 1)`. Classes not in `class_map` are dropped.
#' @inheritParams save_annotations
#'
#' @return invisibly returns a data frame of image names, groups and sets.
#' @export
#'
#' @examples
#' \dontrun{
#' # Images named e.g. "01_B1-02-2023-11-02-151006-154006.mp4_0010000.jpg",
#' # grouped by tank ID ("01_B1-02")
#' import_annotations(
#'   images = "old_project/images",
#'   labels = "old_project/labels",
#'   group_pattern = "^[0-9]+_B1-[0-9]+",
#'   seed = 1
#' )
#' }
import_annotations <- function(images,
                               labels,
                               split_by = c("video", "image"),
                               split = c(Train = 0.6, Val = 0.2, Test = 0.2),
                               group_pattern = NULL,
                               class_map = NULL,
                               seed = NULL,
                               project = get_Project()) {

  split_by <- match.arg(split_by)
  split <- .check_split(split)

  if (is.null(project)) {
    stop("No active TrackR project. Activate a project using `set_Project()`.")
  }
  if (!dir.exists(images)) stop("`images` directory not found: ", images)
  if (!dir.exists(labels)) stop("`labels` directory not found: ", labels)

  img_files <- list.files(images, pattern = "\\.(jpe?g|png)$", ignore.case = TRUE)
  if (length(img_files) == 0) stop("No images found in ", images)

  lab_files <- file.path(labels, paste0(tools::file_path_sans_ext(img_files), ".txt"))
  has_label <- file.exists(lab_files)
  if (any(!has_label)) {
    message("Skipping ", sum(!has_label), " image(s) with no matching label file.")
  }
  img_files <- img_files[has_label]
  lab_files <- lab_files[has_label]

  sets <- .assign_sets(
    image_names = img_files,
    split_by = split_by,
    split = split,
    group_pattern = group_pattern,
    seed = seed,
    project = project
  )

  for (i in seq_along(img_files)) {
    set <- sets$set[sets$image_name == img_files[i]][1]

    lines <- readLines(lab_files[i], warn = FALSE)
    lines <- lines[nzchar(trimws(lines))]

    if (!is.null(class_map) && length(lines) > 0) {
      cls <- sub("\\s.*$", "", lines)
      keep <- cls %in% names(class_map)
      lines <- paste(class_map[cls[keep]], sub("^\\S+", "", lines[keep]), sep = "")
    }

    file.copy(
      from = file.path(images, img_files[i]),
      to = file.path(project, "YOLO", set, "images", img_files[i])
    )
    writeLines(
      lines,
      con = file.path(project, "YOLO", set, "labels",
                      paste0(tools::file_path_sans_ext(img_files[i]), ".txt"))
    )
  }

  return(invisible(sets))
}



#' INTERNAL Check a train/val/test split specification
#' @noRd
.check_split <- function(split) {
  if (!is.numeric(split) || length(split) != 3 ||
      !setequal(names(split), c("Train", "Val", "Test"))) {
    stop("`split` must be a named numeric vector with elements 'Train', 'Val' and 'Test'.")
  }
  if (any(split < 0) || abs(sum(split) - 1) > 1e-8) {
    stop("`split` proportions must be non-negative and sum to 1.")
  }
  split[c("Train", "Val", "Test")]
}



#' INTERNAL Identify the source video (or other group) of each image
#'
#' @description
#' Uses `group_pattern` if supplied. Otherwise parses names written by
#' `extract_images()`: `{group}_{video}_{frame}` or, for chaptered videos,
#' `{group}_{video}c{chapter}_{frame}`. Returns NA where no group is found.
#'
#' @noRd
.image_groups <- function(image_names, group_pattern = NULL) {

  base <- tools::file_path_sans_ext(basename(image_names))

  if (!is.null(group_pattern)) {
    m <- regexpr(group_pattern, base, perl = TRUE)
    out <- rep(NA_character_, length(base))
    out[m > 0] <- regmatches(base, m)
    return(out)
  }

  pat <- "^(.+)_([0-9]+)(c[0-9]+)?_[0-9]+$"
  out <- rep(NA_character_, length(base))
  ok <- grepl(pat, base)
  out[ok] <- sub(pat, "\\1_\\2", base[ok])
  out
}



#' INTERNAL Allocate images to Train/Val/Test sets
#'
#' @description
#' With `split_by = "video"`, all images from the same group go to the same set.
#' Groups already present in the project keep their existing set. New groups are
#' shuffled and each is added to the set furthest below its target number of
#' images. With `split_by = "image"` each image is allocated independently.
#'
#' @return data frame with columns image_name, group and set
#' @noRd
.assign_sets <- function(image_names, split_by, split, group_pattern = NULL,
                         seed = NULL, project = get_Project()) {

  set_names <- c("Train", "Val", "Test")

  # Local, reproducible random number generation that leaves the user's RNG
  # state unchanged
  if (!is.null(seed)) {
    if (exists(".Random.seed", envir = .GlobalEnv)) {
      old_seed <- get(".Random.seed", envir = .GlobalEnv)
      on.exit(assign(".Random.seed", old_seed, envir = .GlobalEnv), add = TRUE)
    } else {
      on.exit(rm(".Random.seed", envir = .GlobalEnv), add = TRUE)
    }
    set.seed(seed)
  }

  if (split_by == "image") {
    set <- sample(set_names, length(image_names), replace = TRUE, prob = split)
    return(data.frame(image_name = image_names, group = image_names, set = set))
  }

  groups <- .image_groups(image_names, group_pattern)

  if (any(is.na(groups))) {
    message(
      "Could not identify the source video of ", sum(is.na(groups)),
      " image(s); these will be allocated individually. ",
      "Use `group_pattern` to define groups for images not named by `extract_images()`."
    )
    groups[is.na(groups)] <- paste0(".img_", image_names[is.na(groups)])
  }

  # Existing allocations from previous annotation sessions
  existing <- do.call(rbind, lapply(set_names, function(s) {
    f <- list.files(file.path(project, "YOLO", s, "images"))
    if (length(f) == 0) return(NULL)
    data.frame(group = .image_groups(f, group_pattern), set = s)
  }))
  counts <- stats::setNames(rep(0, 3), set_names)
  group_set <- character(0)

  if (!is.null(existing)) existing <- existing[!is.na(existing$group), , drop = FALSE]

  if (!is.null(existing) && nrow(existing) > 0) {
    split_groups <- unique(existing$group[duplicated(existing$group) &
      !duplicated(existing[, c("group", "set")])])
    if (length(split_groups) > 0) {
      warning(length(split_groups), " group(s) already span more than one set in ",
              "this project (e.g. from an earlier image-level split).")
    }
    first <- existing[!duplicated(existing$group), ]
    group_set <- stats::setNames(first$set, first$group)
    tab <- table(factor(existing$set, levels = set_names))
    counts[names(tab)] <- as.numeric(tab)
  }

  # Allocate new groups, largest deficit first
  group_sizes <- table(groups)
  new_groups <- setdiff(names(group_sizes), names(group_set))
  new_groups <- new_groups[sample.int(length(new_groups))]
  target <- split * (sum(counts) + length(image_names))

  for (g in new_groups) {
    s <- set_names[which.max((target - counts) / pmax(target, 1e-9))]
    group_set[g] <- s
    counts[s] <- counts[s] + group_sizes[[g]]
  }

  out <- data.frame(
    image_name = image_names,
    group = groups,
    set = unname(group_set[groups])
  )

  n_groups <- tapply(out$group, factor(out$set, levels = set_names),
                     function(x) length(unique(x)))
  n_imgs <- table(factor(out$set, levels = set_names))
  message(
    "Allocated ", length(image_names), " images from ",
    length(unique(groups)), " groups: ",
    paste0(set_names, " ", n_imgs, " images (", ifelse(is.na(n_groups), 0, n_groups),
           " groups)", collapse = "; ")
  )
  if (any(counts == 0)) {
    message(
      "The ", paste(set_names[counts == 0], collapse = " and "),
      " set(s) currently contain no images. Annotate images from more videos ",
      "before training."
    )
  }

  out
}



#' INTERNAL Convert single image annotations to YOLO format
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
#'  .save_image,
#'  .progress = "text"
#' )
#' }
#'

.save_image <-
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
