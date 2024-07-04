# onAttach function

.onAttach <- function(libname, pkgname){

  # Check if the default conda environment has been created, if so use it and if not
  # print a warning that create_TrackR_env should be run
  if(reticulate::condaenv_exists('animaltrackr')){
    # Attempt to activate the enviornment
    tryCatch({
      reticulate::use_condaenv('animaltrackr', required = TRUE)
      packageStartupMessage(paste0("Successfuly activated conda enviroment: ", 'animaltrackr'))
    }, error = function(e){
      packageStartupMessage(paste0("Failed to acitvate conda enviroment: ", 'animaltrackr'))
      packageStartupMessage("Error message:", e$message)
      packageStartupMessage("Try restarting your R session and trying again.")
    })

    if(!check_TrackR_env()){
      packageStartupMessage(
        "Default conda environment does not meet requirements.\n
         Please activate a different environment using `set_TrackR_condaenv()`,\n
         or create a suitable environment using `create_TrackR_condaenv()`. \n
         You may need to restart your R session."
      )
    }

  } else{
    packageStartupMessage(
      "Default conda environment not found.\n
       If this is your first time using the `AnimalTrackR on this machine,\n
       please create a suitable environment using `create_TrackR_condaenv()`. \n
       \n
       If you have already created an environment with a custom name you can \n
       activate it using `set_TrackR_condaenv()`."
    )
  }
}
