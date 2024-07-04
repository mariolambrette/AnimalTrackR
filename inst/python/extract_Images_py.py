import os
import random
import cv2 as cv

def TestingPython():
  return(True)


def extract(vids, weights, n, path, vid_ext):
    
    # supported extensions
    exts = ['.mp4', '.avi', '.mov', '.mkv', '.wmv', '.flv', '.webm']

    # Add custom extension - may fail later on if incompatible with opencv
    if vid_ext is not None:
        exts.append(vid_ext)

    # Get group names
    if weights is not None:
        names = list(weights.keys())
    else:
        names = list('Group1')
        weights = {'Group1': 1}

    # Counter for number of frames exported
    exported = 0

    # Calculate the number of images per video to export in each group
    for gr in names:
        nimgs = int(n * weights[gr])
        nvids = len(vids[gr])
        vid_imgs = int(nimgs / nvids)

        # Check that number of images per video is greater than or equal to 1
        if vid_imgs < 1:
            vid_imgs = 1

        # Extract images from each video in group
        for vid_id, vid in enumerate(vids[gr]):

            print(f"Extracting frames from {vid}")

            # Check if video path is a file or folder
            if os.path.isdir(vid):

                # List to store video file paths
                files = []

                # List the files with compatible extensions in the chapter directory
                for f in os.listdir(vid):
                    if os.path.isfile(os.path.join(vid, f)):
                        f_ext = os.path.splitext(f)[1]
                        if f_ext.lower() in exts:
                            files.append(f)

                # Get the number of frames in each chapter
                frames = []
                for idx, f in enumerate(files):
                    cap = cv.VideoCapture(f)

                    if not cap.isOpened():
                        raise ValueError(f"Cannot read video: {f}")

                    # get number of frames
                    nframes = int(cap.get(cv.CAP_PROP_FRAME_COUNT))
                    cap.release()

                    frames.extend([(idx, frame) for frame in range(1, nframes + 1)])

                # Random sample of frames to extract
                select = random.sample(frames, vid_imgs)

                # Iterate over unique video id's in selected frames to extract selected frames
                for id in set(idx for idx, _ in select):
                    # Capture video
                    cap = cv.VideoCapture(files[id])

                    # Get the frame numbers to extract
                    frame_nums = [f_num for idx, f_num in select if idx == id]

                    for num in frame_nums:
                        # Navigate to frame
                        cap.set(cv.CAP_PROP_POS_FRAMES, num - 1)

                        # read the frame
                        ret, frame = cap.read()

                        # Check if the frame was read successfully
                        if not ret:
                            print(f"Error: Could not read frame {num}.")
                            continue

                        savepath = os.path.join(path, f"{gr}_{id}_{num}.jpg")
                        cv.imwrite(savepath, frame)
                        exported += 1

                    cap.release()

            else:
                # Check the video has a compatible extension
                if os.path.isfile(vid):
                    f_ext = os.path.splitext(vid)[1]
                    if f_ext.lower() not in exts:
                        raise ValueError("Video format incompatible. Check file path and adjust `vid_ext` if necessary")
                else:
                    raise ValueError("Video path format incompatible. Please check file paths and try again")

                # Capture video
                cap = cv.VideoCapture(vid)

                # Get number of frames
                frames = int(cap.get(cv.CAP_PROP_FRAME_COUNT))

                # Random sample
                select = random.sample(range(0, frames), vid_imgs)

                for num in select:
                    # Navigate to frame
                    cap.set(cv.CAP_PROP_POS_FRAMES, num - 1)

                    # read the frame
                    ret, frame = cap.read()

                    # Check if the frame was read successfully
                    if not ret:
                        print(f"Error: Could not read frame {num}.")
                        continue

                    savepath = os.path.join(path, f"{gr}_{vid_id}_{num}.jpg")
                    cv.imwrite(savepath, frame)
                    exported += 1

                cap.release()

    print(f"{exported} frames saved to {path}")
