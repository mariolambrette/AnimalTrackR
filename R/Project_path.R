## package level project path variable helper functions



# Set project (used to change between projects)
set_Project <- function(path){ # Takes the path to the project root folder
  trackr_env$proj <- tools::file_path_as_absolute(path)
}



# Return the current project (rarely needed by users, more useful internally)
get_Project <- function(){
  return(trackr_env$proj)
}



# Initiate a new project
init_Project <- function(path = "Project"){ # Path to project root directory

  # Construct an absolute file path from the users input
  path <- tools::file_path_as_absolute(path)

  # Check if any folders exist in the parent directory with the same name and
  # and increment the name if so
  dirs <- list.dirs(
    dirname(path),
    pattern = basename(path),
    recursive = F,
    full.names = F
  )
  if(length(dirs) > 0){
    path <- paste0(
      path,
      as.character((length(dirs)+1))
    )
  }

  # Create the project directory and the relevant subdirectories
  dir.create(path)
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

  print("Project directory intialised.")
}
