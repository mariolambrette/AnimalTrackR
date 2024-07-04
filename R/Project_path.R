## package level project path variable helper functions



#' Set TrackR project
#'
#' @description
#' AnimalTrackR uses a [project folder structure](https://github.com/mariolambrette/AnimalTrackR?tab=readme-ov-file#file-structure).
#' \code{\link{set_Project}()} is a helper function to allow the user to define the root folder
#' of the project they are working on.
#'
#' @param path directory path to the desired project root folder
#'
#' @return Invisibly returns TRUE if the project path is set correctly.
#' @export
#'
#' @examples
#' \dontrun{
#' set_Project(".../AnimalTrackR-projects/project1")
#' }

set_Project <- function(path){ # Takes the path to the project root folder
  trackr_env$proj <- tools::file_path_as_absolute(path)
  invisible(T)
}



# Return the current project (rarely needed by users, more useful internally)
#' Get the current project root directory
#'
#' @description
#' Allows the user to check the current AnimalTackR project root directory
#'
#'
#' @return File path to the active project root directory
#' @export
#'
#' @examples
#' \dontrun{
#' get_Project()
#' }

get_Project <- function(){
  return(trackr_env$proj)
}



#' Initiate a new AnimalTrackR project
#'
#' @description
#' AnimalTrackR relies on a specific project folder structure. Many of its functions
#' will act on this folder structure implicitly to handle tasks such as dividing
#' annotated images into training, testing and validation groups. `init_Project()`
#' is a helper function to allow users to initialise a new project and begin
#' model development.
#'
#' The function use the path specified as the root to create a full project
#' folder structure. More information on this structure can be found [here](https://github.com/mariolambrette/AnimalTrackR?tab=readme-ov-file#file-structure).
#'
#' A newly initiated project will automatically be set as the current active project.
#' To change back to a previous project use \code{\link{set_Project}()}
#'
#'
#' @param path File path (can be an absolute or relative path) to the root of the
#'   project to initialise. The final part of the file path will be the name of
#'   the project root and therefore the project name so it should be chosen with
#'   some care and be unique among the user's AnimalTrackR projects
#'
#' @return Returns a message in the console confirming successful initialisation
#' @export
#'
#' @examples
#' \dontrun{
#' # Creates a folder in the current working directory called 'AnimalTrackR-projects'
#' # then creates the Project folder within that unde the 'Project1' root folder
#'
#' init_project("AnimalTrackR-projects/Project1")
#' }
init_Project <- function(path = "Project"){ # Path to project root directory

  # Construct an absolute file path from the users input
  path <- normalizePath(path, mustWork = F)

  # Check for writing permissions in project path
  if(file.access(path, 2) != 0){
    stop("No write permissions for selected path.\n
          Please check permissions and try again.")
  }

  # Check if any folders exist in the parent directory with the same name and
  # and increment the name if so
  dirs <- list.dirs(
    dirname(path),
    recursive = F,
    full.names = F
  )
  dirs <- grep(basename(path), dirs, value = T)

  if(length(dirs) > 0){
    path <- paste0(
      path,
      as.character((length(dirs)+1))
    )
  }

  # Create the project directory and the relevant subdirectories
  dir.create(path)

  if(!dir.exists(path)){
    stop("Error creating project directory.\n
          Please check file paths and try again")
  }

  dir.create(file.path(path, "ToAnnotate"))
  dir.create(file.path(path, "YOLO"))
  dir.create(file.path(path, "YOLO", "Train"))
  dir.create(file.path(path, "YOLO", "Test"))
  dir.create(file.path(path, "YOLO", "Val"))
  dir.create(file.path(path, "YOLO", "configs"))
  dir.create(file.path(path, "YOLO", "models"))
  dir.create(file.path(path, "YOLO", "Train", "images"))
  dir.create(file.path(path, "YOLO", "Test", "images"))
  dir.create(file.path(path, "YOLO", "Val", "images"))
  dir.create(file.path(path, "YOLO", "Train", "labels"))
  dir.create(file.path(path, "YOLO", "Test", "labels"))
  dir.create(file.path(path, "YOLO", "Val", "labels"))

  file.copy(
    from = system.file("config", "classes.txt", package = "AnimalTrackR"),
    to = file.path(path, "YOLO", "configs", "labels.txt")
  )

  cat("\033[31mProject directory initialized.\033[0m\n")

  set_Project(path)

  return(invisible(T))
}
