#' Create the necessary environment for using `AnimalTrackR`
#'
#' @description
#' `install_TrackR()` creates a python 3.9 virtual environment and installs a number
#' of required packages within it. For the majority of users it only needs to be run once
#' on the first usage of `AnimalTrackR`
#'
#' @details
#' The majority of users should run `install_TrackR()` once when they first use the
#' `AnimalTrackR` package with the default arguments. This will create a virtual
#' environment named 'animaltrackr'. Each subsequent call to `library(AnimalTrackR)`
#' will automatically connect the user's R session to this virtual environment.
#'
#' Some users may wish to modify the default behaviour and create an environment
#' with a different name. The `envname` parameter allows this, though users must note
#' that subsequent calls to `library(AnimalTrackR)` will not automatically connect to
#' the custom environment. Users will need to use the [TrackR_virtualenv()] function
#' to connect to their custom environment.
#'
#'
#' @param envname Character string. The name of the virtual environment to create
#' @param new_env Boolean. Whether or not the envname has been changed from the default 'animaltrackr'
#'
#' @returns Returns a message in the console confirming successful installation
#' @export
#'
#' @examples
#' \dontrun{
#' # Default behaviour (reccomended)
#' library(AnimalTrackR)
#' install_TrackR() # Not run again on this machine
#'
#' # Custom environments (Not reccomended)
#' # On first usage of AnimalTrackR:
#' library(AnimalTrackR)
#' install_TrackR(envname = 'somename')
#'
#' # On subsequent usages of AnimalTrackR:
#' library(AnimalTrackR)
#' TrackR_virtualenv(envname = 'somename')
#'
#' }
#'
install_TrackR <-
  function(envname = 'animaltrackr',
           new_env = identical(envname, "animaltrackr")){

    # If the package virtual environment already exists delete it so that a fresh one
    # can be created
    if(new_env && reticulate::virtualenv_exists(envname)){
      reticulate::virtualenv_remove(envname)
    }

    # Get the os on which the package is running and add it to the package environment
    trackr_env$os <- Sys.info()["sysname"]

    # Get the python versions installed on the machine to check if 3.9 is present
    if(trackr_env$os == "Windows"){
      vs <- reticulate::py_versions_windows()$version
      if(!("3.9" %in% vs)){
        stop("No python 3.9 installation found.\n
             Please install python 3.9 using `reticulate::install_python(version = '3.9:latest')`,
             or your preffered method of installation, then try again")
      }
    } else if(trackr_env$os == "Darwin"){
      vs <- system("whereis python")
      if(isFALSE(grepl(vs))){
        stop("No python 3.9 installation found.\n
             Please install python 3.9 using `reticulate::install_python(version = '3.9:latest')`,
             or your preffered method of installation, then try again")
      }
    } else{
      stop("Operating system not yet supported.\n
           AnimalTrackR currently only supports Windows and Mac systems")
    }

    # Create the virtual python environment
    reticulate::virtualenv_create(
      envname,
      version = "3.9",
      packages = trackr_env$python_packages
    )

    # Make sure reticulate is using the created virtual environment
    reticulate::use_virtualenv(envname)

    cat("\033[31mAnimalTrackR successfully installed.\033[0m\n")
  }
