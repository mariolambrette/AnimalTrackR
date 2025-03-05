# onAttach function

# .onAttach <- function(libname, pkgname){
#
#   # Check if the default conda environment has been created, if so use it and if not
#   # print a warning that create_TrackR_env should be run
#   if (reticulate::condaenv_exists('animaltrackr')) {
#
#     # Attempt to activate the environment
#     tryCatch({
#       suppressMessages(suppressWarnings({
#         reticulate::use_condaenv('animaltrackr', required = TRUE)
#       }))
#       packageStartupMessage(paste0("Successfuly activated conda environment: ", 'animaltrackr'))
#
#     }, error = function(e){
#       packageStartupMessage(paste0("Failed to acitvate conda environment: ", 'animaltrackr'))
#       packageStartupMessage("Error message: ", e$message)
#       packageStartupMessage("Try restarting your R session and trying again.")
#     })
#
#     if (!check_TrackR_env()) {
#       packageStartupMessage(
#         "Active conda environment does not meet requirements.\n
#          Please activate a different environment using `set_TrackR_env()`,\n
#          or create a suitable environment using `create_TrackR_env()`. \n
#          You may need to restart your R session."
#       )
#     }
#
#   } else{
#     packageStartupMessage(
#       "Default conda environment not found.\n
#        If this is your first time using the `AnimalTrackR on this machine,\n
#        please create a suitable environment using `create_TrackR_env()`. \n
#        \n
#        If you have already created an environment with a custom name you can \n
#        activate it using `set_TrackR_env()`."
#     )
#   }
# }

.onAttach <- function(libname, pkgname) {

  # Check if the default conda environment exists, if so use it and otherwise
  # prompt the user to create it
  if (reticulate::condaenv_exists("animaltrackr")) {

    # Activate default environment
    reticulate::use_condaenv("animaltrackr", required = TRUE)

    packageStartupMessage(paste0("Successfuly activated conda environment: ", "animaltrackr"))

  } else {
    packageStartupMessage(
      "Default conda environment not found.\nIf this is your first time using `AnimalTrackR on this machine,\nplease create a suitable environment using `create_TrackR_env()`.\nIf you have already created an environment with a custom name you can \nactivate it using `set_TrackR_env()`."
    )
  }
}
