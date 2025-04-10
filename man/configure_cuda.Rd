% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/TrackR_env.R
\name{configure_cuda}
\alias{configure_cuda}
\title{Configure conda environment to enable GPU acceleration}
\usage{
configure_cuda(envname = "animaltrackr", cuda.version)
}
\arguments{
\item{envname}{Name of the conda environment in which to configure CUDA. This
should normally stay as the default, "animaltrackr"}

\item{cuda.version}{Version of cuda installed on the users machine (numeric or character string, e.g. 12.1)}
}
\description{
This function installs the necessary packages in the animaltrackr environment
to allow GPU acceleration. This can significantly improve model processing
speeds.

The function is a simple wrapper function to install pytorch in the animaltrackr
environment and check if GPU acceleration is now available. If this does not work
(you can verify gpu availability using \code{check_gpu()}) you will need to verify
your set-up separately.
}
\examples{
\dontrun{
library(AnimalTrackR)
create_TrackR_env()
configure_cuda(cuda.version = 12.1)
}

}
\seealso{
\code{\link[=check_gpu]{check_gpu()}}
}
