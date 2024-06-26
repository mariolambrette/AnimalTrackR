# onAttach function

.onAttach <- function(libname, pkgname){
  # Check if the animaltrackr virtual environment exists, if so use it and if not
  # print a warning that install_TrackR should be run
  if(reticulate::virtualenv_exists("animaltrackr")){
    reticulate::use_virtualenv("animaltrackr")
  } else{
    packageStartupMessage(
      "Default python environment (animaltrackr) not found.\n
       Please use `install_TrackR()` to create it, or use `TrackR_virtualenv()`
       to use an environemnt with a different name"
    )
  }
}
