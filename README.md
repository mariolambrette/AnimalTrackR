
<!-- README.md is generated from README.Rmd. Please edit that file -->

# AnimalTrackR

<!-- badges: start -->
<!-- badges: end -->

AnimalTrackR provides a simple interface to YOLOv8 to allow users to
train animal detection models to aid large scale beavioural analyses.

## Installation

FFMPEG NEEDED!!!!!

You can install the development version of AnimalTrackR like so:

``` r
# FILL THIS IN! HOW CAN PEOPLE INSTALL YOUR DEV PACKAGE?
```

## Intructions

### Overview

`AnimalTrackR` provides a simple end-to-end pipeline for using [YOLOv8
detection models](https://docs.ultralytics.com/tasks/detect/) to track
an individual animal in captivity for behavioural analyses. Broadly, it
will allow you to sample frames from the total of your experiment’s
footage which you can then annotate and use to train a YOLOv8 detection
model. You can then run this model on short example videos for display
and validation or in batch on large quantities of footage to allow
subsequent analyses.

#### File Structure

`AnimalTrackR` uses a specific file structure to support working on
multiple projects and allow seamless integration with standard YOLOv8
development pipelines. The structure is as follows:

. └── AnimalTrackR-projects ├── Project1 │ ├── ToAnnotate │ └── YOLO │
├── Train │ │ ├── labels │ │ │ └── .txt annotation files corresponding
to the images in the images/ directory │ │ └── images │ │ └── .jpg
images ready for model training │ ├── Test │ │ └── … │ ├── Val │ │ └── …
│ ├── configs │ │ ├── model1.yaml │ │ │ └── model training configuration
files, there will be one for each model in the models/ directory │ │ └──
model2.yaml │ └── models │ ├── model1/ │ │ └── Files containing model
training info and model weights. │ └── model2/ └── Project2 └── …

Most users will never need to interact directly with the vast majority
of this file structure, it is created and managed by helper functions
provided in `AnimalTrackR`

WRITE A MORE DETAILED FILE STRUCTURE MANUAL ELSEWHERE

### First Usage:

AnimalTrackR relies heavily on python to execute image processing tasks.
Specifically, it uses a python 3.9 virtual environment. This means that
when you first install the package you will need to run the following
command to create this environment:

``` r

library(AnimalTrackR)

install_TrackR()
```

This relies on you having python 3.9 installed on your machine. The
above function will give an error if you don’t. You can install python
3.9 using any of the common installation techniques, though the simplest
method for this application is to run the following code in your R
session to install the latest release of python 3.9 into your root
directory:

``` r

library(reticulate)

install_python(version = "3.9:latest")
```

These steps are only required the first time you use AnimalTrackR. The
correct environment is now set up and will be made available to your R
session with any subsequent usage of `library(AnimalTrackR)`.

### Starting a project

Now that you have attached `AnimalTrackR` to your R session and created
a suitable python environment to handle image processing tasks you will
need to start a project. An `AnimalTrackR` project contained within a
directory that has a specific structure to support the image annotation
process and allow for seamless integration with other YOLO development
pipelines. See the FILE STRUCTURE DOCUMENTATION FOR MORE INFORMATION.
use the `init_Project()` function to initiate a new project by providing
the path to the project root directory. Providing just a project name
(as in the example below) will create a project folder with that name in
the current working directory.

``` r

init_Project(path = 'MyProject')
```

Now that your project is set up you are ready to start developing a
detection model.

### Extracting training images

The first step is to extract still images from your experimental
footage. This step will require careful consideration of your
experimental design and the footage you have. TO have the best chance of
devloping accurate detection models the training images used should
accurately reflect variability in your exerimental data. For some
experiments with very little variability between video clips this may be
very straightforward but for others where there may be many different
tank/cage set ups and variable backgrounds this may be more complex.

The basic premise for this step is to create a stratified sample with
the different experimental groups. Due to the potential for massive
variation in user’s file structures and experimental designs the process
of grouping video files sensibly is left to users. The image extraction
function `IMAGEEXTRACTIONFUNCTION`(described in detail further down)
takes a named list as in input. This list’s structure is as follows:

``` r

video_files <- list(
  Group1 = list(
    "video1.mp4",
    "video2.mp4",
    "video3.mp4",
    ...
  ),
  Group2 = list(
    "video4.mp4",
    "video5.mp4",
    "video6.mp4",
    ...
  ),
  ...
)
```

The `IMAGEEXTRACTIONFUNCTION` also takes another lis as an argument
which defines the weights to apply to each group when performing a
stratified sample. This argument is optional, by default the function
will use the relative numbers of videos in each group to inform the
weights to apply, however there may be instances where users want to
change this. This argument to do this should be of the same structure as
the `video_files` list:

``` r

group_weights <- list(
  Group1 = 0.4,
  Group2 = 0.6
)
```

The sum of the weights supplied must be equal to 1 and the names of the
groups must be the same in these two lists or `IMAGEEXTRACTIONFUNCTION`
will throw an error.
