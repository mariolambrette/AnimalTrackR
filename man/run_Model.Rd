% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/run_model.R
\name{run_Model}
\alias{run_Model}
\title{Run AnimalTrackR detection model}
\usage{
run_Model(
  video,
  detections_path = NULL,
  model = NULL,
  save_vid = F,
  save_path = NULL,
  gopro = FALSE
)
}
\arguments{
\item{video}{Path to the video on which to run inference. This can either be a path to a single video file or a path to a directory containing chapterised video.}

\item{detections_path}{File path at which to save the detections. This is an optional parameter. By default the detections will be saved at "{project_path}/Detections/VideoName.csv".}

\item{model}{The detection model to use for detections. If you have only trained one model in this project you can leave the default behaviour, which uses the first model that appears in the "YOLO/models" directory in the project folder. Otherwise, the name of the desired model folder should be provided. See details for more information.}

\item{save_vid}{Boolean. Defaults to \code{FALSE}, if set to \code{TRUE} a copy of the video is saved with predicted bounding boxes plotted onto it. This is useful for validation purposes though for long videos can significantly increase processing times. We recommend using \code{\link[=demo_run]{demo_run()}} for this type of validation.}

\item{save_path}{The path at which to save the video with overlaid detections. Only used if \code{save_vid} is \code{TRUE}.}

\item{gopro}{T/F. The default is FALSE but this must be set to TRUE if footage was recorded on a gopro. See details for more information.}
}
\value{
Returns the filepath to the detections file
}
\description{
Run AnimalTrackR detection model
}
\details{
\strong{\code{model} details:} In principle, any YOLOv8-v11 model weights can be used here.
For most users, providing the model name specified in \code{\link[=train_Model]{train_Model()}} will
be the right thing to do here. However, users wishing to experiment with
other model weights can do so by providing the full path to those weights
here.
\subsection{Important for GoPro users}{

\strong{\code{gopro} details:} Modern GoPro footage is unfortunately incompatible with
this software (see \href{https://stackoverflow.com/questions/78039408/cv2-ffmpeg-grabframe-packet-read-max-attempts-exceeded-error-after-exactly-rea}{here} for details).
If you try to run a detection model on GoPro footage you may get this error:
"\link{ WARN:0@2245.518} global cap_ffmpeg_impl.hpp:1595 grabFrame packet read max attempts exceeded".

If this happens, set the GoPro parameter to TRUE. This reformats the footage
to remove the audio stream. so that it can be processed by the software. The
reformatted file is stored in a temporary location and the original video
file you provide is not modified. This reformatting slows the overall
processing speed. unfortunately this is currently unavoidable, but we are
monitoring updates in this software's underlying computer vision framework
(\code{opencv}) for a more friendly solution.

Note that this reformatting requires an additional software library, 'FFMPEG'.
This is a free video processing library that can be installed \href{https://ffmpeg.org/download.html}{here}
}
}
\examples{
\dontrun{
run_model(video = "path/to/video.mp4")
}

}
