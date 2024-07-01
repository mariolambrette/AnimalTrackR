#' Create the necessary conda environment for using `AnimalTrackR`
#'
#' @description
#' `create_TrackR_env()` creates a python 3.9 conda environment and installs a number
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
#' use the [set_TrackR_condaenv()] function to connect to their custom environment.
#'
#'
#' @param envname Character string. The name of the virtual environment to create
#'
#' @seealso [reticulate::install_miniconda()], [set_TrackR_condaenv()]
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

create_TrackR_env <- function(envname = 'animaltrackr'){

  # If the conda environment exists ask the user if they want to delete and
  # recreate it
  if(reticulate::condaenv_exists(envname)){

    message("Environment already exists.")
    ui_env <- menu(
      choices = c("Yes", "No"),
      title = "Do you want to overwrite it?"
    )

    if(ui == 1){
      reticulate::conda_remove(envname)
    } else{

      # Check the enviroment meets the requirements
      if(check_TrackR_env()){

        # Activate the enviroment if possible, or display an error
        tryCatch({
          reticulate::use_condaenv(envname, required = TRUE)
          message(paste0("Successfuly activated conda enviroment ", envname))
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
  if(length(list.files(reticulate::miniconda_path())) == 0){

    message("No miniconda installation found,  Miniconda is required to set up AnimalTrackR")
    ui <- menu(
      choices = c("Yes", "No"),
      title = "Would you like to install Miniconda?"
    )

    if(ui == 1){
      message("Installing Miniconda...")
      reticulate::install_miniconda()
    } else{
      stop("Miniconda installation is required to procedd. Aborting setup.")
    }
  }

  # Create conda environment
  reticulate::conda_create( ## CHANGE TO USE ENVIRONMENT DEFINITION FILE
    envname = envname,
    packages = trackr_env$python_packages,
    python_version = "3.9"
  )

  # Check and activate envrionment
  if(check_TrackR_env()){
    tryCatch({
      reticulate::use_condaenv(envname, required = TRUE)
      message(paste0("Successfuly activated conda enviroment ", envname))
    }, error = function(e){
      message(paste0("Failed to acitvate conda enviroment: ", envname))
      message("Error message:", e$message)
      message("Try restarting your R session and trying again.")
    })
  } else {
    stop(paste0("Conda environment: ", envname, " Does not meet requirements.\n
                 Please try again."))
  }

  invisible(T)
}
