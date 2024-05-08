## Function to extract training/testing&validation images from video data

# Minimum of one image per video
# accept either path to mp4 files or path to parent folder for chapterised videos
# parent folders will be treated as 'videos' for sampling purposes
# supported video format: '.mp4', '.avi', '.mov', '.mkv', '.wmv', '.flv', '.webm'
#  (not case sensitive). ALternative file types can be added with the vid_ext argument,
#  though the file types must be compatible with opencv

extract_Images <-
  function(videos,
           group_weights = NULL,
           nimgs = 1600, # ~ 1000 training images, 300 testing 300 validation
           vid_ext = NULL,
           project = get_Project()){


    # Check python environment meets requirements
    check_TrackR_env()

    if(!is.NULL(group_weights)){

      # Check names of video and weights lists match
      if(sort(names(videos)) != sort(names(group_weights))){
        stop("Group names in video and weights list do not match.\n
           Please check and try again")
      }

      # Check group weights are decimals and sum to 1
      if(sum(as.numeric(group_weights)) != 1){
        stop("Group weights must sum to 1")
      }
    }

    if(!is.numeric(nimgs)){
      stop("`nimgs` is not numeric")
    }

    # Create python arguments for extraction function
    reticulate::r_to_py(videos)
    reticulate::r_to_py(group_weights)
    reticulate::r_to_py(nimgs)
    reticulate::r_to_py(vid_ext)
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
