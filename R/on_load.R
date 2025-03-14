# .onLoad function

.onLoad <- function(libname, pkgname){

  ## Load python modules here using import_from_path and delay_load = TRUE
  ## Function within each module are available via module_name$function_name

  py_extractImages <<- reticulate::import_from_path(
    "extract_images_py",
    system.file("python", package = pkgname),
    delay_load = TRUE
  )

  py_BehaviourVis <<- reticulate::import_from_path(
    "behaviour_vis_py",
    system.file("python", package = pkgname),
    delay_load = TRUE
  )

  py_run_model <<- reticulate::import_from_path(
    "run_model_py",
    system.file("python", package = pkgname),
    delay_load = TRUE
  )

  py_train_model <<- reticulate::import_from_path(
    "train_model_py",
    system.file("python", package = pkgname),
    delay_load = TRUE
  )
}



# package environment variables
trackr_env <- new.env(parent = emptyenv())
trackr_env$project <- NULL
trackr_env$proj <- NULL
trackr_env$os <- NULL

# Python bindings
py_extractImages <- NULL
py_BehaviourVis  <- NULL
py_run_model     <- NULL
py_train_model   <- NULL
