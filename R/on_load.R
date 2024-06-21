# .onLoad function

.onLoad <- function(libname, pkgname){

  # Check if the animaltrackr virtual environment exists, if so use it and if not
  # print a warning that install_TrackR should be run
  if(reticulate::virtualenv_exists("animaltrackr")){
    reticulate::use_virtualenv("animaltrackr")
  } else{
    warning("Default python environment (animaltrackr) not found.\n
            Please use `install_TrackR()` to create it, or use `TrackR_virtualenv()`
            to use an environemnt with a different name")
  }

  ## Load python modules here using import_from_path and delay_load = TRUE
  ## use assignInMyNamespace to make functions available in the package
  ## https://forum.posit.co/t/build-package-namespace-dynamically-during-onload/4101/3

  py_extractImages <- reticulate::import_from_path(
    "extract_Images_py",
    system.file("python", package = packageName()),
    delay_load = TRUE
  )
  utils::assignInMyNamespace("py_extractImages", py_extractImages)

}


# package environment variables
trackr_env <- new.env(parent = emptyenv())
trackr_env$project <- NULL
trackr_env$python_packages <- c("ultralytics",
                                "torchaudio",
                                "torchvision",
                                "opencv-python")
trackr_env$proj <- NULL
trackr_env$os <- NULL
