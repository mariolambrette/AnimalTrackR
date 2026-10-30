import ultralytics
import os


def train_model(config_file, project_dir, model_name, gpu, param_file, t_only,
                device=None, seed=None):

  """
  Python function to train YOLO11s detection model based on a TrackR project
  configuration file.

  :param config_file: Path to a YOLO config file
  :param project_dir: Path to a TrackR project directory
  :param model_name: The name under which to save the model
  :param gpu: Boolean indicating whether a GPU is available (from check_gpu() in R)
  :param param_file: Path to a training hyperparameter configuration file
  :param t_only: Train on the Target class only (ignore ZZZ annotations)
  :param device: Device to train on: a GPU index (e.g. 0 or 1) or "cpu".
      None uses GPU 0 if a GPU is available, otherwise the CPU.
  :param seed: Random seed for training. None uses the seed in param_file.
  :return: path to the directory the training run was saved in (Ultralytics
      adds a number to model_name if a model of that name already exists)
  """

  wd = os.getcwd()
  os.chdir(project_dir)

  try:
    models_dir = os.path.join(project_dir, "YOLO", "models")
    model = ultralytics.YOLO(os.path.join(models_dir, "yolo11s.pt"))

    if device is None:
      device = 0 if gpu else "cpu"
    elif isinstance(device, float):
      device = int(device)

    # Settings passed here override those in param_file, which is left
    # unchanged (so that several models can be trained at once)
    args = dict(
      data=config_file,
      project=models_dir,
      name=model_name,
      verbose=True,
      plots=True,
      device=device,
      cfg=param_file
    )
    if t_only:
      args["classes"] = [0]
    if seed is not None:
      args["seed"] = int(seed)

    model.train(**args)
  finally:
    os.chdir(wd)

  return str(model.trainer.save_dir)
