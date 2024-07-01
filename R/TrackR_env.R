#' Set the virtual environment for the current R session
#'
#' @description
#' For users who have created custom environments or use reticulate in other
#' contexts frequently this function provides a simple way to set the path to
#' the correct virtual environment for `AnimalTrackR` to run.
#'
#' Note that the named environment must be a python 3.9 environment with the correct
#' dependencies installed or the function gives an error.
#'
#'
#' @param envname Character string. Name of the virtual environemnt to connect to.
#'
#' @return Invisibly returns TRUE
#' @export
#'
#' @examples
#' \dontrun{
#' TrackR_virtualenv('animaltrackr')
#' }

TrackR_virtualenv <- function(envname){

  # Check the specified environment exists
  if(reticulate::virtualenv_exists(envname)){
    # Use the specified environment
    reticulate::use_virtualenv(envname)

    # Check the enviornment
    check_TrackR_env()

  } else{
    # Throw an error if the virtual environment does not exist
    stop("No virtual environments with this name.\n
         Check name or use `install_TrackR()` to create a virtual environment")
  }

  invisible(T)
}


#' Check the current TrackR environment
#'
#' @description
#' Checks the active conda environment meets the requirements to run
#' AnimalTrackR. Internal, non-exported function.
#'
#' @noRd
#'
#' @return TRUE if the environment meets the requirements
#'

check_TrackR_env <- function(){
  # Check the python version and installed packages in the specified environment
  py_version <- reticulate::py_discover_config()$python_version

  if(is.null(py_version)){
    return(F)
  }

  missingpackages <- setdiff(trackr_env$python_packages, reticulate::py_list_packages()[,1]) %>%
    length()

  # Throw an error if environment requirements are not met
  if(py_version != "3.9" | missingpackages != 0){
    stop("Virtual environment does not meet requirements.\n
           Please use `install_TrackR()` to create a suitable virtual environment")
  }

  return(TRUE)
}
