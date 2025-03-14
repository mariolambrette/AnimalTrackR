import ultralytics
import os 

def train_model(config_file, project_dir, model_name, gpu, param_file):
  
  """
  Python function to train YOLO11s detection model based on a TrackR project 
  configuration file.
  
  :param config_file: Path to a YOLO config file
  :param project_dir: Path to a TrackR project directory
  :param model_name: The name under which to save the model
  :param gpu: Boolean indicating whether training should be conducted using a GPU (boolean created in R using check_gpu())
  :param param_file: Path to a training hyperparameter configuration file
  :return: XXX
  """
  
  wd = os.getcwd()
  os.chdir(project_dir)
  
  models_dir = os.path.join(project_dir, "YOLO", "models")
  model = ultralytics.YOLO(os.path.join(models_dir, "yolo11s.pt"))
  
  
  
  if gpu:
    model.train(
      data=config_file,
      project=models_dir,
      name=model_name,
      verbose=True,
      plots=True,
      device=0,
      cfg=param_file
    )
  else:
    model.train(
      data=config_file,
      project=models_dir,
      name=model_name,
      verbose=True,
      plots=True,
      device="cpu",
      cfg=param_file
    )
  
  os.chdir(wd)
