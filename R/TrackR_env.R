#' Create the necessary conda environment for using `AnimalTrackR`
#'
#' @description
#' `create_TrackR_env()` creates a python 3.11 conda environment and installs a number
#' of required packages within it. For the majority of users it only needs to be run once
#' on the first usage of `AnimalTrackR`
#'
#' @details
#' The majority of users should run `create_TrackR_env()` once when they first use the
#' `AnimalTrackR` package with the default arguments. This will create a conda
#' environment named "animaltrackr". Each subsequent call to `library(AnimalTrackR)`
#' will automatically connect the user's R session to this conda environment.
#'
#' The creation of the conda environment relies on users having [miniconda](https://docs.anaconda.com/miniconda/)
#' installed on their machine. If no miniconda installation can be found the
#' function will prompt users to install miniconda. Internally, accepting this
#' prompt will call [reticulate::install_miniconda()] and install miniconda in the
#' default locations on the users machine. If users prefer to install their own
#' version of miniconda they can do so at the link above.
#'
#' Some users may wish to modify the default behaviour and create an environment
#' with a different name. The `envname` parameter allows this, though users must note
#' that subsequent calls to `library(AnimalTrackR)` will not automatically connect to
#' the custom environment. After calling `library(AnimalTrackR)`, users will need to
#' use the [set_TrackR_env()] function to connect to their custom environment.
#'
#' @param envname Character string. The name of the conda environment to create.
#'  Most users should not need to change from the default, "animaltrackr"
#' @param cuda.version The verion of CUDA installed on the user's machine. Either
#'  as a decimal or a string. You can find your cuda version by running `nvidia-smi`
#'  at the command line.
#'
#' @seealso [reticulate::install_miniconda()], [set_TrackR_env()]
#'
#' @returns Invisibly returns TRUE upon successful environment creation and activation.
#' @export
#'
#' @examples
#' \dontrun{
#' # Default behaviour (reccomended)
#' library(AnimalTrackR)
#' create_TrackR_env() # Not run again on this machine
#'
#' # Custom environments (Not reccomended)
#' # On first usage of AnimalTrackR:
#' library(AnimalTrackR)
#' create_TrackR_env(envname = 'somename')
#'
#' # On subsequent usages of AnimalTrackR:
#' library(AnimalTrackR)
#' TrackR_condaenv(envname = 'somename')
#'
#' }
#'
#'
#' @import reticulate

create_TrackR_env <- function(envname = "animaltrackr", cuda.version = FALSE){

  # If the conda environment exists ask the user if they want to delete and
  # recreate it
  if (reticulate::condaenv_exists(envname)) {

    message("Environment already exists.")
    ui_env <- menu(
      choices = c("Yes", "No"),
      title = "Do you want to overwrite it?"
    )

    if (ui_env == 1) {
      message("Removing existing environment...")
      reticulate::conda_remove(envname)
    } else {

      # Check the environment meets the requirements
      if (check_TrackR_env()) {

        # Activate the enviroment if possible, or display an error
        tryCatch({
          reticulate::use_condaenv(envname, required = TRUE)
          message(paste0("Successfuly activated conda enviroment ", envname))
          return(invisible(TRUE))
        }, error = function(e){
          message(paste0("Failed to acitvate conda enviroment: ", envname))
          message("Error message:", e$message)
          message("Try restarting your R session and trying again.")
        })
      } else {

        # If environment does not meet requirements display an error
        stop("Chosen environment down not meet TrackR requirements.\n
              Please use a different environment, or overwrite the chosen one, and try again.")
      }
    }
  }

  # Check if miniconda is installed on the system and if not, prompt the user
  # to install it
  if (length(list.files(reticulate::miniconda_path())) == 0) {

    message("No miniconda installation found,  Miniconda is required to set up AnimalTrackR")
    ui_mc <- menu(
      choices = c("Yes", "No"),
      title = "Would you like to install Miniconda?"
    )

    if (ui_mc == 1) {
      message("Installing Miniconda...")
      reticulate::install_miniconda()
    } else{
      stop("Miniconda installation is required to proceed. Aborting setup.")
    }
  }

  message("Creating conda environment '", envname, "'...")

  # Create empty environment
  reticulate::conda_create(envname, python_version = "3.11")

  # Install requirements
  message("Installing dependencies...")

  if (!cuda.version) {
    reticulate::py_install(
      c("opencv-python", "ultralytics"),
      envname = envname,
      pip = TRUE
    )
  } else {
    reticulate::conda_install(
      envname = envname,
      packages = c("pytorch=2.1", "torchvision", paste0("pytorch-cuda=", cuda.version)),
      channel = c("pytorch", "nvidia")
    )
    reticulate::conda_install(
      envname = envname,
      packages = "ultralytics",
      pip = TRUE
    )
    reticulate::conda_install(
      envname = envname,
      packages = "numpy<2",
      channels = c("conda-forge")
    )
  }

  # Check and activate environment
  tryCatch({
    reticulate::use_condaenv(envname, required = TRUE)
    message(paste0("Successfuly activated conda enviroment ", envname))
  }, error = function(e){
    message(paste0("Failed to acitvate conda enviroment: ", envname))
    message("Error message:", e$message)
    message("Try restarting your R session and trying again.")
  })

  invisible(T)
}


#' Configure conda environment to enable GPU acceleration
#'
#' @description
#' This function installs the necessary packages in the animaltrackr environment
#' to allow GPU acceleration. This can significantly improve model processing
#' speeds.
#'
#' The function is a simple wrapper function to install pytorch in the animaltrackr
#' environment and check if GPU acceleration is now available. If this does not work
#' (you can verify gpu availability using `check_gpu()`) you will need to verify
#' your set-up separately.
#'
#' @param envname Name of the conda environment in which to configure CUDA. This
#'  should normally stay as the default, "animaltrackr"
#' @param cuda.version Version of cuda installed on the users machine (numeric or character string, e.g. 12.1)
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(AnimalTrackR)
#' create_TrackR_env()
#' configure_cuda(cuda.version = 12.1)
#' }
#'
#' @seealso [check_gpu()]

configure_cuda <- function(envname = "animaltrackr", cuda.version){

  reticulate::conda_remove(envname, packages = c("pytorch", "torchvision", "torchaudio", "pytorch-cuda"))
  reticulate::conda_install(
    envname = envname,
    packages = c("pytorch=2.1", "torchvision", "torchaudio", paste0("pytorch-cuda=", cuda.version)),
    channel = c("pytorch", "nvidia")
  )

  if (check_gpu()) {
    message("GPU acceleration available")
  } else {
    message("GPU acceleration not successfully enabled. Please check your set-up and try again")
  }
}

#' Check if GPU is available
#'
#' @description
#' Checks wheter a named conda environment ("animaltrackr" by default) is
#' correctly configured for GPU acceleration. If you have a CUDA GPU enabled
#' system you can set the envrionment up to use it properly with
#' [`create_TrackR_env()`] for a new environment or [`configure_cuda()`] for
#' an existing environment.
#'
#' @param envname Name of the conda environment in which to check the availability
#'  of a GPU
#'
#' @return Boolean indicating whether GPU is available.
#' @export
#'
#' @examples
#' \dontrun{
#' library(AnimalTrackR)
#' check_gpu()
#' }
#'
#' @import reticulate
#'
#' @seealso [create_TrackR_env()] [configure_cuda()]

check_gpu <- function(envname = "animaltrackr"){

  if (!reticulate::condaenv_exists(envname)) {
    stop("No conda environment found with the given name. Check `envname` and try again")
  }

  envpath <- reticulate::conda_list()$python[reticulate::conda_list()$name == envname]
  system(paste(envpath, "-c \"import torch; print(torch.cuda.is_available())\""), intern = TRUE) == "True"
}



#' Set the conda environment for the current session
#'
#' @description
#' For users who have created custom environments or use reticulate in other
#' contexts frequently this function provides a simple way to set the path to
#' the correct conda environment for `AnimalTrackR` to run.
#'
#' Note that the named environment must be a python 3.11 environment with the correct
#' dependencies installed or the function gives an error. We reccomend you use
#' [`create_TrackR_env()`] to create the environment to ensure this is the case
#'
#' @param envname Character string. Name of conda environemnt to connect to.
#'
#' @return Invisibly returns TRUE
#' @export
#'
#' @examples
#' \dontrun{
#' set_TrackR_env('animaltrackr')
#' }
#'
#' @import reticulate
#'
#' @seealso [create_TrackR_env()]

set_TrackR_env <- function(envname = "animaltrackr"){

  # Check the specified environment exists
  if (reticulate::condaenv_exists(envname)) {

    # Check the environment and if it meets the requirements activate it
    if (check_TrackR_env(envname)) {
      # Use the specified environment
      reticulate::use_condaenv(envname)
    } else {
      stop("The specified conda environment does not meet the neccesary requirements.\n
            Please use a different conda environment, or create a suitable environment \n
            using `create_TrackR_env()`")
    }

  } else {
    # Throw an error if the virtual environment does not exist
    stop("No conda environments with this name.\n
         Check name or use `create_TrackR_env()` to create a virtual environment")
  }

  invisible(T)
}


#' Check a named conda environment for AnimalTrackR compatibility
#'
#' @description
#' Checks a named conda environment ("animaltrackr" by default) to see if meets
#' the python dependency requirements of AnimalTrackR. These include python 3.11,
#' ultralytics and opencv installations. If your environment does not meet the
#' necessary requirements, consider using [`create_TrackR_env()`] to create one
#' that does.
#'
#' To further check an enviornment for GPU compatibility, you can use
#' [`check_gpu()`].
#'
#' @param envname of the conda envionment to check
#'
#' @return TRUE if the environment meets the requirements, or FALSE if not.
#' @export
#'
#' @examples
#' \dontrun{
#' create_TrackR_env()
#' check_TrackR_env("animaltrackr")
#' }
#'
#' @importFrom reticulate conda_list
#'
#' @seealso [create_TrackR_env()] [check_gpu()]
#'

check_TrackR_env <- function(envname = "animaltrackr"){
  # Find the path to the environment python executable
  envpath <- reticulate::conda_list()$python[reticulate::conda_list()$name == envname]

  # Check if ultralytics and opencv (core depndencies) are installed
  ul <- system(paste(envpath, "-c \"import ultralytics\""), ignore.stdout = TRUE, ignore.stderr = TRUE) == 0
  cv <- system(paste(envpath, "-c \"import cv2\""), ignore.stdout = TRUE, ignore.stderr = TRUE) == 0
  pv <- system(paste(envpath, "-c \"import sys; print(sys.version)\""), intern = TRUE, ignore.stdout = TRUE, ignore.stderr = TRUE) == "3.11"

  return(all(cv, ul, pv))
}
