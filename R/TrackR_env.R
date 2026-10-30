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
#' @param cuda.version Optional. If your computer has an NVIDIA GPU, your CUDA
#'  version as shown by running `nvidia-smi` in a command prompt (e.g. `13.1`).
#'  The environment is then set up to use the GPU (see [configure_cuda()]).
#'  Leave as `FALSE` (the default) for computers without a GPU.
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

  if (!isFALSE(cuda.version)) .parse_cuda_version(cuda.version)

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

  # The same packages are installed whether or not a GPU will be used, so
  # results are comparable between computers
  reticulate::py_install(
    .trackr_py_packages(),
    envname = envname,
    pip = TRUE
  )

  # Swap in a GPU build of the same version of PyTorch if requested
  if (!isFALSE(cuda.version)) {
    configure_cuda(envname = envname, cuda.version = cuda.version)
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


#' Set up an AnimalTrackR environment to use your GPU
#'
#' @description
#' Training and running detection models is much faster (often 10-50 times)
#' on a computer with a suitable NVIDIA graphics card (GPU). `configure_cuda()`
#' replaces the standard version of PyTorch (the software that runs the models)
#' in your AnimalTrackR environment with a version that can use your GPU. The
#' same version of PyTorch is kept, so results are comparable with runs on
#' computers without a GPU.
#'
#' You only need to do this once per computer. [create_TrackR_env()] calls it
#' for you if you give it a `cuda.version`.
#'
#' @details
#' **Finding your CUDA version**
#'
#' Open a command prompt (Windows) or terminal (Mac/Linux) and run
#' `nvidia-smi`. The CUDA version is shown in the top right of the output
#' (e.g. "CUDA Version: 13.1"). If the command is not found, your computer
#' either has no NVIDIA GPU or its drivers are not installed.
#'
#' **What the function does**
#'
#' The CUDA version shown by `nvidia-smi` is the newest your graphics driver
#' supports. PyTorch is built for particular CUDA versions, so
#' `configure_cuda()` looks for the newest PyTorch build that matches the
#' version of PyTorch already in your environment and is no newer than your CUDA
#' version, then installs it. The download is large (often 2-3 GB) and can take
#' several minutes. If no suitable build can be installed, the original version
#' of PyTorch is put back so your environment keeps working.
#'
#' Finally, the function checks that the GPU can be used ([check_gpu()]).
#' Restart R afterwards so the change takes effect.
#'
#' @param envname Name of the AnimalTrackR conda environment. Most users should
#'  keep the default, "animaltrackr".
#' @param cuda.version Your CUDA version, as shown by `nvidia-smi`, e.g. `13.1`
#'  or `"12.8"`.
#'
#' @return Invisibly, `TRUE` if the GPU can now be used, otherwise `FALSE`.
#' @export
#'
#' @examples
#' \dontrun{
#' library(AnimalTrackR)
#' configure_cuda(cuda.version = 13.1)
#' # Restart R, then check:
#' check_gpu()
#' }
#'
#' @seealso [check_gpu()], [create_TrackR_env()]

configure_cuda <- function(envname = "animaltrackr", cuda.version){

  cuda <- .parse_cuda_version(cuda.version)

  if (!reticulate::condaenv_exists(envname)) {
    stop("No conda environment called '", envname, "' found. ",
         "Create one with `create_TrackR_env()` first.")
  }
  python <- .env_python(envname)

  # Versions of PyTorch currently installed, without any '+cpu' style suffix
  script <- tempfile(fileext = ".py")
  on.exit(unlink(script), add = TRUE)
  writeLines(c("import torch, torchvision", "print(torch.__version__)",
               "print(torchvision.__version__)"), script)
  current <- .run_py(python, script)
  if (current$status != 0 || length(current$output) < 2) {
    stop("PyTorch was not found in the '", envname, "' environment. ",
         "Recreate it with `create_TrackR_env()`.")
  }
  torch_v <- sub("\\+.*$", "", trimws(current$output[length(current$output) - 1]))
  vision_v <- sub("\\+.*$", "", trimws(current$output[length(current$output)]))
  pins <- c(paste0("torch==", torch_v), paste0("torchvision==", vision_v))

  message("Looking for a GPU build of PyTorch ", torch_v,
          " for CUDA ", cuda$label, " or older...")

  tags <- .cuda_tags(cuda)
  chosen <- NULL
  for (tag in tags) {
    url <- paste0("https://download.pytorch.org/whl/", tag)
    available <- .run_py(python, c("-m", "pip", "index", "versions", "torch",
                                   "--index-url", url))
    if (any(grepl("unknown command|no such command", available$output, ignore.case = TRUE))) {
      stop("This version of pip cannot list available PyTorch builds. Install the ",
           "GPU build manually using the command from ",
           "https://pytorch.org/get-started/locally/ (PyTorch ", torch_v, ").")
    }
    if (available$status == 0 &&
        any(grepl(paste0("\\b", gsub(".", "\\.", torch_v, fixed = TRUE), "\\b"),
                  available$output))) {
      chosen <- tag
      break
    }
  }

  if (is.null(chosen)) {
    message("No GPU build of PyTorch ", torch_v, " was found for CUDA ",
            cuda$label, " or older. Your environment has not been changed.\n",
            "See https://pytorch.org/get-started/locally/ for supported CUDA versions.")
    return(invisible(FALSE))
  }

  message("Installing PyTorch ", torch_v, " (", chosen, "). This is a large ",
          "download and may take several minutes...")
  url <- paste0("https://download.pytorch.org/whl/", chosen)
  .run_py(python, c("-m", "pip", "uninstall", "-y", "torch", "torchvision"))
  installed <- .run_py(python, c("-m", "pip", "install", pins, "--index-url", url),
                       echo = TRUE)

  if (installed$status != 0) {
    message("Installing the GPU build failed. Restoring the original version...")
    .run_py(python, c("-m", "pip", "install", pins), echo = TRUE)
    message("Your environment has been restored. See the messages above for ",
            "the cause of the problem.")
    return(invisible(FALSE))
  }

  gpu <- isTRUE(tryCatch(check_gpu(envname), error = function(e) FALSE))
  if (gpu) {
    message("GPU acceleration is available. Restart R for the change to take effect.")
  } else {
    message("PyTorch ", torch_v, " (", chosen, ") was installed, but the GPU ",
            "could not be used. Check that your NVIDIA drivers are up to date.")
  }
  invisible(gpu)
}



#' INTERNAL Parse a CUDA version given as e.g. 13.1, "13.1" or "12.8"
#' @return list(major, minor, label)
#' @noRd
.parse_cuda_version <- function(cuda.version) {
  v <- trimws(as.character(cuda.version))
  if (length(v) != 1 || !grepl("^[0-9]+(\\.[0-9]+)?$", v)) {
    stop("`cuda.version` should be your CUDA version as shown by `nvidia-smi`, ",
         "e.g. 13.1 or \"12.8\".")
  }
  parts <- as.integer(strsplit(v, ".", fixed = TRUE)[[1]])
  if (length(parts) == 1) parts <- c(parts, 0L)
  if (parts[1] < 11) {
    stop("CUDA ", v, " is too old for current versions of PyTorch. ",
         "Please update your NVIDIA drivers.")
  }
  list(major = parts[1], minor = parts[2], label = paste(parts, collapse = "."))
}



#' INTERNAL PyTorch wheel index tags to try, newest first, no newer than the
#' driver's CUDA version (e.g. 13.1 -> cu131, cu130, cu129, ..., cu118)
#' @noRd
.cuda_tags <- function(cuda) {
  tags <- character(0)
  for (major in seq(cuda$major, 11)) {
    top <- if (major == cuda$major) cuda$minor else 9
    bottom <- if (major == 11) 8 else 0
    if (top >= bottom) tags <- c(tags, paste0("cu", major, seq(top, bottom)))
  }
  tags
}



#' INTERNAL Path to the Python executable of a conda environment
#' @noRd
.env_python <- function(envname) {
  envs <- reticulate::conda_list()
  envs$python[envs$name == envname][1]
}



#' INTERNAL Run Python (e.g. pip) in a subprocess
#' @return list(status, output)
#' @noRd
.run_py <- function(python, args, echo = FALSE) {
  windows <- .Platform$OS.type == "windows"
  quote <- function(x) if (windows) shQuote(x, type = "cmd") else shQuote(x)
  # Arguments are passed to the shell as typed, so quote any containing spaces
  # (e.g. paths); flags and version numbers are safe as they are
  args <- ifelse(grepl("\\s", args), quote(args), args)
  # system2() quotes the program path itself, so it must not be quoted here
  out <- suppressWarnings(system2(python, args, stdout = TRUE, stderr = TRUE))
  status <- attr(out, "status")
  if (echo && length(out) > 0) message(paste(out, collapse = "\n"))
  list(status = if (is.null(status)) 0L else status, output = out)
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

    # Use the specified environment
    reticulate::use_condaenv(envname)

    # Check the environment and if it meets the requirements activate it
    if (!check_TrackR_env(envname)) {
      stop("The specified conda environment does not meet the neccesary requirements.\nPlease use a different conda environment, or create a suitable environment \nusing `create_TrackR_env()`")
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
#' the python dependency requirements of AnimalTrackR: Python 3.11 with the
#' ultralytics, opencv, pandas and matplotlib packages. If anything is missing,
#' a message says what and gives the command to install it. Alternatively,
#' [`create_TrackR_env()`] creates a new environment with everything needed.
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

  # Check the environment exists
  if (!reticulate::condaenv_exists(envname)) {
    stop(paste0(
      "No conda environment with name ", envname,
      " found.\nCheck `envname` and try again"
    ))
  }

  # Check that the Python modules AnimalTrackR uses are available
  modules <- .trackr_py_modules()
  available <- vapply(names(modules), reticulate::py_module_available, logical(1))
  if (!all(available)) {
    missing <- unname(modules[!available])
    message(
      "The active Python environment is missing: ", paste(missing, collapse = ", "),
      ".\nInstall with: reticulate::py_install(c(",
      paste0('"', missing, '"', collapse = ", "),
      "), envname = \"", envname, "\", pip = TRUE)\n",
      "then restart R."
    )
  }

  # Check Python version
  py_version <- as.numeric(substr(reticulate::py_config()$version, 1, 4))
  pv <- py_version == 3.11

  return(all(available, pv))
}



#' INTERNAL Python packages installed by create_TrackR_env()
#'
#' @description
#' Every Python package imported by AnimalTrackR's Python code must be listed
#' here (pip names), and in `.trackr_py_modules()` (import names), so that new
#' environments have everything needed and `check_TrackR_env()` catches
#' anything missing. pandas and matplotlib are listed explicitly because recent
#' versions of ultralytics no longer install pandas.
#'
#' @noRd
.trackr_py_packages <- function() {
  c("opencv-python", "ultralytics", "pandas", "matplotlib")
}

#' INTERNAL Python modules AnimalTrackR imports (import name = pip name)
#' @noRd
.trackr_py_modules <- function() {
  c(cv2 = "opencv-python", ultralytics = "ultralytics",
    pandas = "pandas", matplotlib = "matplotlib")
}
