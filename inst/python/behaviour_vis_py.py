import os
import cv2 as cv
import pandas as pd
import matplotlib.pyplot as plt

def behaviour_vis(video_path, detections, output_path, class_column):
    """
    Draws bounding boxes on the video frames and saves the modified video.

    Parameters:
    video_path (str): Path to the input video file.
    detections (pd.DataFrame): A pandas dataframe containing bounding box data.
    output_path (str): Path to save the output video with bounding boxes.
    class_column (str): Name of the behavioural class column in the detections dataframe
    """
    
    # Open the original video file
    cap = cv.VideoCapture(video_path)
  
    # Check the video could be opened
    if not cap.isOpened():
        raise ValueError(f"Cannot open video: {video_path}")
    
    # Get video properties
    width = int(cap.get(cv.CAP_PROP_FRAME_WIDTH))
    height = int(cap.get(cv.CAP_PROP_FRAME_HEIGHT))
    fps = cap.get(cv.CAP_PROP_FPS)
    fourcc = cv.VideoWriter_fourcc(*'mp4v')

    # Prepare the video writer
    out = cv.VideoWriter(output_path, fourcc, fps, (width, height))
  
    # Get unique categories and generate a color map
    unique_categories = sorted(detections[class_column].unique())
    color_palette = plt.get_cmap("tab10")  # You can change this to any matplotlib colormap
    color_map = {category: color_palette(i) for i, category in enumerate(unique_categories)}

    # Convert matplotlib RGB values to OpenCV format
    color_map = {k: tuple(int(x * 255) for x in v[:3]) for k, v in color_map.items()}

    # Process each frame
    frame_index = 0
    while cap.isOpened():
        ret, frame = cap.read()
        if not ret:
            break

        # Filter detections for the current frame
        frame_detections = detections[detections['Frame'] == frame_index]

        for index, row in frame_detections.iterrows():
            xl, yt, xr, yb = row['xl'], row['yt'], row['xr'], row['yb']
            category = row[class_column]
            color = color_map.get(category, (0, 255, 255))  # Default color if category not found

            # Draw the bounding box on the frame
            cv.rectangle(frame, (xl, yt), (xr, yb), color, 2)

        # Write the frame to the output video
        out.write(frame)
        frame_index += 1

    # Release resources
    cap.release()
    out.release()
    cv.destroyAllWindows()
    print(f"Output video saved at: {output_path}")

