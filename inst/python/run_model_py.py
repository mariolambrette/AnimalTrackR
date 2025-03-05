import cv2 as cv
import csv
from torch import cuda
from ultralytics import YOLO

def check_cuda():
  return cuda.is_available()

# Function for running model on single video
def run_model(vid, weights, detections):
    
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
    
    # Empty list of rows to store detections
    rows = []
    
    # Iterate over each frame, running inference
    while True:
      
      # Read the frame
      ret, frame = cap.read()
      
      if not ret:
        break
      
      # Process frame by running inference and updating csv file
      res = model.predict(frame, save=False, conf=0.75, classes=[0], save_txt=False, stream=True, max_det=1)
      
      # Extract box coordinates
      for r in res:
        box = r.boxes
      coords = box.xywh
  
      # Skip saving if there is no detection
      if coords.shape[0] == 0:
        continue
      
      frame = cap.get(cv.CAP_PROP_POS_FRAMES)
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
      
      row = [vid, frame, timestamp, xc, yc, xl, xr, yt, yb]
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
