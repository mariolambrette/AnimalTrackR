## Function to extract training/testing&validation images from video data

extract_Images <-
  function(videos,
           group_weights = NULL,
           nimgs = 1600, # ~ 1000 training images, 300 testing 300 validation
           project = get_Project()){


    # Check python environment meets requirements
    check_TrackR_env()

    # Check names of video and weights lists match
    if(!is.NULL(group_weights) &&
       sort(names(videos)) != sort(names(group_weights))){
      stop("Group names in video and weights list do not match.\n
           Please check and try again")
    }

    # Create python arguments for extraction function
    reticulate::r_to_py(videos)
    reticulate::r_to_py(group_weights)
    reticulate::r_to_py(nimgs)
    file.path(project, "ToAnnotate")





  }




videos <- list(
  Group1 = list(
    "video1.mp4",
    "video2.mp4",
    "video3.mp4"
  ),
  Group2 = list(
    "video4.mp4",
    "video5.mp4",
    "video6.mp4"
  )
)

group_weights <- list(
  Group1 = 0.4,
  Group2 = 0.6
)
