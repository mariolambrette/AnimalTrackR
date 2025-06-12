import ultralytics
import os
import yaml

def train_model(config_file, project_dir, model_name, gpu, param_file, t_only):
  
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
  
  if t_only:
        # Read the existing YAML file
        with open(param_file, 'r') as file:
            config = yaml.safe_load(file)
        
        # Modify the classes parameter
        config['classes'] = [0]
        
        # Save the modified YAML file
        with open(param_file, 'w') as file:
            yaml.safe_dump(config, file, default_flow_style=False)
  
  
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
  
  if t_only:
      # Read the existing YAML file
      with open(param_file, 'r') as file:
          config = yaml.safe_load(file)
      
      # Modify the classes parameter
      config['classes'] = [0,1]
      
      # Save the modified YAML file
      with open(param_file, 'w') as file:
          yaml.safe_dump(config, file, default_flow_style=False)
  
  os.chdir(wd)
