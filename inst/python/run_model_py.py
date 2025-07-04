import cv2 as cv
import csv
from torch import cuda
from ultralytics import YOLO
import os

# Function for checking cuda availability
def check_cuda():
  return cuda.is_available()

# Function for running model on single video
def run_model(vid, weights, detections, detect_fps=None):
    
    """
    Runs YOLO detection model on video data and records detection in AnimalTrackR format csv file.
  
    Parameters:
    vid (str): Path to the input video file.
    weights (str): Path to YOLO model weights.
    detections (str): Path to output AnimalTrackR detection file
    """
    
    # Load model
    model = YOLO(weights)
    
    # Create video capture
    cap = cv.VideoCapture(vid)
    
    # Check the capture was successful
    if not cap.isOpened():
      raise FileNotFoundError(f"Cannot open video: {vid}")
    
    # Get video fps
    video_fps = cap.get(cv.CAP_PROP_FPS)
    
    # Calculate frame skip interval
    if detect_fps is None:
      frame_skip = 1  # Process every frame
    else:
      if detect_fps > video_fps:
        print(f"Warning: Detection FPS ({detect_fps}) is higher than video FPS ({video_fps}). Processing every frame.")
        frame_skip = 1
      else:
        frame_skip = int(video_fps / detect_fps)
    
    # Empty list of rows to store detections
    rows = []
    
    # frame counter
    frame_count = 0
    
    # Iterate over each frame, running inference
    while True:
      
      # Iterate frame number
      frame_count += 1
      
      # Read the frame
      ret, frame = cap.read()
      
      if not ret:
        break
      
      if frame_count % frame_skip == 0:
        # Process frame by running inference and updating csv file
        res = model.predict(frame, save=False, conf=0.75, classes=[0], save_txt=False, stream=True, max_det=1)
        
        # Extract box coordinates
        for r in res:
          box = r.boxes
        coords = box.xywh
    
        # Skip saving if there is no detection
        if coords.shape[0] == 0:
          continue
        
        current_frame = cap.get(cv.CAP_PROP_POS_FRAMES)
        timestamp = cap.get(cv.CAP_PROP_POS_MSEC)
        
        # Get coordinate values
        xc = coords[0, 0].item()
        yc = coords[0, 1].item()
        hw = (coords[0, 2].item()) / 2
        hh = (coords[0, 3].item()) / 2
        xl = xc - hw
        xr = xc + hw
        yt = yc - hh
        yb = yc + hh
        
        # Append coordinates to the table
        row = [vid, current_frame, timestamp, xc, yc, xl, xr, yt, yb]
        rows.append(row)
      

      
    # Release video capture
    cap.release()
    
    # Open file and create writer
    csv_file = open(detections, 'a', newline='')
    csv_writer = csv.writer(csv_file)
  
    # write rows
    csv_writer.writerows(rows)
  
    # close the file
    csv_file.close()
    
# Function for running model in demo mode
def demo_run(weights, video, project, name):

    # Load model
    model = YOLO(weights)
    
    # Capture video
    cap = cv.VideoCapture(video)
    
    # Check the capture was successful
    if not cap.isOpened():
      raise FileNotFoundError(f"Cannot open video: {vid}")
    
    # Get video properties
    fps = int(cap.get(cv.CAP_PROP_FPS))
    width = int(cap.get(cv.CAP_PROP_FRAME_WIDTH))
    height = int(cap.get(cv.CAP_PROP_FRAME_HEIGHT))
    
    # Define video output path
    output_vid = name + ".avi"
    output_path = os.path.join(project, output_vid)
    
    # Define codec and create VideoWriter
    fourcc = cv.VideoWriter_fourcc(*'XVID')
    out = cv.VideoWriter(output_path, fourcc, fps, (width, height))
    
    # Check if VideoWriter opened successfully
    if not out.isOpened():
        print("Error: Could not open VideoWriter")
        cap.release()
        exit()
    
    frame_count = 0
    
    while True:
        # Read the frame
        ret, frame = cap.read()
        
        if not ret:
            break
        
        # Process frame by running inference
        res = model.predict(frame, save=False, conf=0.75, classes=[0], save_txt=False, stream=True, max_det=1)
        
        # Extract box coordinates
        coords = None
        for r in res:
            box = r.boxes
            if box is not None:
                coords = box.xywh
        
        # Skip saving if there is no detection
        if coords is None or coords.shape[0] == 0:
            # Write original frame without annotations
            out.write(frame)
            frame_count += 1
            continue
        
        # Get frame info (optional - for logging)
        current_frame = cap.get(cv.CAP_PROP_POS_FRAMES)
        timestamp = cap.get(cv.CAP_PROP_POS_MSEC)
        
        # Get coordinate values
        xc = coords[0, 0].item()
        yc = coords[0, 1].item()
        hw = (coords[0, 2].item()) / 2
        hh = (coords[0, 3].item()) / 2
        xl = int(xc - hw)
        xr = int(xc + hw)
        yt = int(yc - hh)
        yb = int(yc + hh)
        
        # Draw bounding box on frame
        cv.rectangle(frame, (xl, yt), (xr, yb), (247, 232, 22), 2)
        
        # Write annotated frame to output video
        out.write(frame)
        
        frame_count += 1
        if frame_count % 30 == 0:  # Print progress every 30 frames
            print(f"Processed {frame_count} frames")

    # Release everything
    cap.release()
    out.release()
    cv.destroyAllWindows()
