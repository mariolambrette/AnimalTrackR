## Function to extract training/testing&validation images from video data

extract_Images <-
  function(videos,
           group_weights = NULL,
           project = get_Project()){


    # Check python environment meet requirements
    check_TrackR_env()

    # Create python arguments for extraction function
    reticulate::r_to_py(videos)
    reticulate::r_to_py(group_weights)
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
