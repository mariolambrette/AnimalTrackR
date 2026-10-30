"""
Evaluation of a trained AnimalTrackR detection model on the held-out test set.

Two complementary evaluations are produced:

1. Standard object-detection metrics from Ultralytics (precision, recall,
   mAP@0.5, mAP@0.5:0.95 per class, plus precision-recall / F1 curves and a
   confusion matrix).

2. 'Deployment' metrics that mirror how run_Model() uses the model: on each
   image only the single highest-confidence Target detection is kept, and it is
   counted if its confidence is at or above the deployment threshold. These are
   the figures that matter for the position data used in behavioural analysis.
   They are reported at the deployment threshold and across a range of
   thresholds, overall and per video (or other group), with confidence
   intervals, together with the error in the predicted box centre.
"""

import csv
import math
import os
import platform
import random
import time

import sys

import cv2 as cv
import numpy as np
import ultralytics

# Ultralytics log messages contain emoji, which raise encoding errors in
# consoles that are not UTF-8 (e.g. some Windows R consoles)
for _stream in (sys.stdout, sys.stderr):
    try:
        _stream.reconfigure(errors="replace")
    except Exception:
        pass


IOU_MATCH = 0.5
SWEEP = [round(x, 2) for x in np.arange(0.05, 0.951, 0.05)]


# --------------------------------------------------------------------------
# Helpers
# --------------------------------------------------------------------------

def _wilson(k, n, z=1.96):
    """Wilson score interval for a proportion k/n."""
    if n == 0:
        return (float("nan"), float("nan"))
    p = k / n
    denom = 1 + z ** 2 / n
    centre = (p + z ** 2 / (2 * n)) / denom
    half = z * math.sqrt(p * (1 - p) / n + z ** 2 / (4 * n ** 2)) / denom
    return (max(0.0, centre - half), min(1.0, centre + half))


def _prf(tp, fp, fn):
    p = tp / (tp + fp) if (tp + fp) > 0 else float("nan")
    r = tp / (tp + fn) if (tp + fn) > 0 else float("nan")
    f1 = 2 * tp / (2 * tp + fp + fn) if (2 * tp + fp + fn) > 0 else float("nan")
    return p, r, f1


def _iou(a, b):
    """IoU of two boxes in xyxy format."""
    ix1, iy1 = max(a[0], b[0]), max(a[1], b[1])
    ix2, iy2 = min(a[2], b[2]), min(a[3], b[3])
    inter = max(0.0, ix2 - ix1) * max(0.0, iy2 - iy1)
    ua = (a[2] - a[0]) * (a[3] - a[1]) + (b[2] - b[0]) * (b[3] - b[1]) - inter
    return inter / ua if ua > 0 else 0.0


def _read_target_boxes(label_path, w, h):
    """Return Target (class 0) boxes from a YOLO label file in pixel xyxy."""
    boxes = []
    if not os.path.exists(label_path):
        return boxes
    with open(label_path) as f:
        for line in f:
            parts = line.split()
            if len(parts) < 5 or parts[0] != "0":
                continue
            xc, yc, bw, bh = (float(v) for v in parts[1:5])
            boxes.append([(xc - bw / 2) * w, (yc - bh / 2) * h,
                          (xc + bw / 2) * w, (yc + bh / 2) * h])
    return boxes


def _outcome(row, thr):
    """TP/FP/FN counts for one image at confidence threshold thr."""
    detected = row["pred_conf"] is not None and row["pred_conf"] >= thr
    has_gt = row["n_gt_target"] > 0
    if has_gt:
        if detected and row["iou"] >= IOU_MATCH:
            return 1, 0, 0
        if detected:
            return 0, 1, 1
        return 0, 0, 1
    return (0, 1, 0) if detected else (0, 0, 0)


def _counts(rows, thr):
    tp = fp = fn = 0
    for r in rows:
        a, b, c = _outcome(r, thr)
        tp += a
        fp += b
        fn += c
    return tp, fp, fn


def _write_csv(path, rows, fields):
    with open(path, "w", newline="") as f:
        w = csv.DictWriter(f, fieldnames=fields)
        w.writeheader()
        for r in rows:
            w.writerow({k: ("" if r.get(k) is None else r.get(k)) for k in fields})


def _fmt(x, nd=4):
    if x is None or (isinstance(x, float) and math.isnan(x)):
        return None
    return round(float(x), nd)


# --------------------------------------------------------------------------
# System information
# --------------------------------------------------------------------------

def system_info():
    """Hardware and software details for reporting."""
    info = {
        "os": platform.platform(),
        "processor": platform.processor() or platform.machine(),
        "cpu_count": os.cpu_count(),
        "python": platform.python_version(),
        "ultralytics": ultralytics.__version__,
        "opencv": cv.__version__,
        "numpy": np.__version__,
    }
    try:
        import psutil
        info["ram_gb"] = round(psutil.virtual_memory().total / 1024 ** 3, 1)
    except Exception:
        pass
    try:
        import torch
        info["torch"] = torch.__version__
        info["cuda_available"] = bool(torch.cuda.is_available())
        info["cuda_version"] = torch.version.cuda
        if torch.cuda.is_available():
            info["gpus"] = [torch.cuda.get_device_name(i)
                            for i in range(torch.cuda.device_count())]
    except Exception:
        pass
    return info


def training_summary(model_dir):
    """Summarise the training run from Ultralytics' args.yaml and results.csv."""
    out = {}
    import yaml
    args_path = os.path.join(model_dir, "args.yaml")
    if os.path.exists(args_path):
        with open(args_path) as f:
            args = yaml.safe_load(f)
        keep = ["model", "epochs", "patience", "batch", "imgsz", "optimizer",
                "lr0", "lrf", "momentum", "weight_decay", "seed",
                "deterministic", "device", "hsv_h", "hsv_s", "hsv_v",
                "degrees", "translate", "scale", "fliplr", "flipud", "mosaic",
                "mixup", "classes"]
        out["settings"] = {k: args.get(k) for k in keep if k in args}

    res_path = os.path.join(model_dir, "results.csv")
    if os.path.exists(res_path):
        with open(res_path) as f:
            rows = [{k.strip(): v for k, v in r.items()} for r in csv.DictReader(f)]
        if rows:
            out["epochs_run"] = len(rows)
            try:
                fit = [0.1 * float(r["metrics/mAP50(B)"]) +
                       0.9 * float(r["metrics/mAP50-95(B)"]) for r in rows]
                out["best_epoch"] = int(rows[int(np.argmax(fit))]["epoch"])
            except (KeyError, ValueError):
                pass
            if "time" in rows[-1]:
                try:
                    out["training_time_hours"] = round(float(rows[-1]["time"]) / 3600, 3)
                except ValueError:
                    pass
            epochs = out.get("settings", {}).get("epochs")
            if epochs is not None:
                out["stopped_early"] = len(rows) < int(epochs)
    return out


# --------------------------------------------------------------------------
# Main evaluation
# --------------------------------------------------------------------------

def evaluate(weights, data_config, project_dir, eval_dir, test_images, groups,
             conf=0.75, n_boot=1000, seed=0, gpu=False, device=None):
    """
    Evaluate a trained model on the test set.

    :param weights: path to model weights (best.pt)
    :param data_config: path to the YOLO dataset yaml
    :param project_dir: TrackR project directory
    :param eval_dir: directory to write evaluation outputs to
    :param test_images: list of test image file names
    :param groups: list of group (video) labels, same order as test_images
    :param conf: deployment confidence threshold (as used by run_Model)
    :param n_boot: number of bootstrap resamples (by group) for CIs
    :param seed: random seed for the bootstrap
    :param gpu: whether to use the GPU (GPU 0); ignored if device is given
    :param device: a GPU index (e.g. 0 or 1) or "cpu" 
    :return: dict summarising the evaluation
    """
    os.makedirs(eval_dir, exist_ok=True)
    if device is None:
        device = 0 if gpu else "cpu"
    elif isinstance(device, float):
        device = int(device)
    model = ultralytics.YOLO(weights)
    names = model.names

    # Only score the classes the model was trained to detect ('Empty' is a
    # placeholder label for images with nothing in them, not a class)
    train_args = training_summary(os.path.dirname(os.path.dirname(weights)))
    classes = train_args.get("settings", {}).get("classes") or [0, 1]

    # ---- 1. Standard Ultralytics metrics --------------------------------
    wd = os.getcwd()
    os.chdir(project_dir)
    try:
        metrics = model.val(data=data_config, split="test", project=eval_dir,
                            name="plots", exist_ok=True, plots=True,
                            classes=classes, device=device, verbose=False)
    finally:
        os.chdir(wd)

    box = metrics.box
    mean_f1 = (2 * box.mp * box.mr / (box.mp + box.mr)) if (box.mp + box.mr) > 0 else float("nan")
    std_rows = [{
        "class": "all",
        "precision": _fmt(box.mp), "recall": _fmt(box.mr), "F1": _fmt(mean_f1),
        "mAP50": _fmt(box.map50), "mAP50_95": _fmt(box.map)
    }]
    for j, c in enumerate(box.ap_class_index):
        p, r, ap50, ap = box.class_result(j)
        f1 = box.f1[j] if hasattr(box, "f1") and len(box.f1) > j else None
        std_rows.append({
            "class": names[int(c)],
            "precision": _fmt(p), "recall": _fmt(r), "F1": _fmt(f1),
            "mAP50": _fmt(ap50), "mAP50_95": _fmt(ap)
        })
    _write_csv(os.path.join(eval_dir, "test_metrics.csv"), std_rows,
               ["class", "precision", "recall", "F1", "mAP50", "mAP50_95"])

    # ---- 2. Deployment-style evaluation, image by image ----------------
    img_dir = os.path.join(project_dir, "YOLO", "Test", "images")
    lab_dir = os.path.join(project_dir, "YOLO", "Test", "labels")
    rows = []
    t0 = time.perf_counter()
    for name, grp in zip(test_images, groups):
        img = cv.imread(os.path.join(img_dir, name))
        if img is None:
            continue
        h, w = img.shape[:2]
        gt = _read_target_boxes(
            os.path.join(lab_dir, os.path.splitext(name)[0] + ".txt"), w, h)
        res = model.predict(img, conf=0.01, classes=[0], max_det=1,
                            device=device, verbose=False)
        b = res[0].boxes
        row = {"image": name, "group": grp, "n_gt_target": len(gt),
               "pred_conf": None, "iou": 0.0, "centre_error_px": None,
               "centre_error_rel": None}
        if len(gt) > 0:
            g = gt[0]
            row.update({"gt_x1": g[0], "gt_y1": g[1], "gt_x2": g[2], "gt_y2": g[3]})
        if b is not None and len(b) > 0:
            p = b.xyxy[0].tolist()
            row.update({"pred_conf": float(b.conf[0]), "pred_x1": p[0],
                        "pred_y1": p[1], "pred_x2": p[2], "pred_y2": p[3]})
            if len(gt) > 0:
                row["iou"] = max(_iou(p, g) for g in gt)
                g = gt[0]
                dx = (p[0] + p[2]) / 2 - (g[0] + g[2]) / 2
                dy = (p[1] + p[3]) / 2 - (g[1] + g[3]) / 2
                err = math.hypot(dx, dy)
                row["centre_error_px"] = err
                row["centre_error_rel"] = err / max(g[2] - g[0], g[3] - g[1])
        rows.append(row)
    secs_per_img = (time.perf_counter() - t0) / max(len(rows), 1)

    for r in rows:
        tp, fp, fn = _outcome(r, conf)
        r["outcome"] = "TP" if tp else ("FP+FN" if (fp and fn) else
                                         ("FP" if fp else ("FN" if fn else "TN")))
    pred_fields = ["image", "group", "outcome", "n_gt_target", "pred_conf",
                   "iou", "centre_error_px", "centre_error_rel",
                   "gt_x1", "gt_y1", "gt_x2", "gt_y2",
                   "pred_x1", "pred_y1", "pred_x2", "pred_y2"]
    _write_csv(os.path.join(eval_dir, "test_predictions.csv"),
               [{k: (_fmt(v) if isinstance(v, float) else v) for k, v in r.items()}
                for r in rows], pred_fields)

    # Threshold sweep
    sweep_rows = []
    for t in SWEEP:
        tp, fp, fn = _counts(rows, t)
        p, rc, f1 = _prf(tp, fp, fn)
        sweep_rows.append({"conf": t, "TP": tp, "FP": fp, "FN": fn,
                           "precision": _fmt(p), "recall": _fmt(rc), "F1": _fmt(f1)})
    _write_csv(os.path.join(eval_dir, "threshold_sweep.csv"), sweep_rows,
               ["conf", "TP", "FP", "FN", "precision", "recall", "F1"])
    best = max((r for r in sweep_rows if r["F1"] is not None),
               key=lambda r: r["F1"], default=None)

    # Per group (video)
    by_group = {}
    for r in rows:
        by_group.setdefault(r["group"], []).append(r)
    grp_rows = []
    for g, rs in sorted(by_group.items()):
        tp, fp, fn = _counts(rs, conf)
        p, rc, f1 = _prf(tp, fp, fn)
        errs = [r["centre_error_px"] for r in rs
                if _outcome(r, conf)[0] == 1 and r["centre_error_px"] is not None]
        grp_rows.append({"group": g, "n_images": len(rs), "TP": tp, "FP": fp,
                         "FN": fn, "precision": _fmt(p), "recall": _fmt(rc),
                         "F1": _fmt(f1),
                         "median_centre_error_px": _fmt(np.median(errs)) if errs else None})
    _write_csv(os.path.join(eval_dir, "per_group_metrics.csv"), grp_rows,
               ["group", "n_images", "TP", "FP", "FN", "precision", "recall",
                "F1", "median_centre_error_px"])

    # Overall deployment metrics with CIs
    tp, fp, fn = _counts(rows, conf)
    p, rc, f1 = _prf(tp, fp, fn)
    rng = random.Random(seed)
    keys = list(by_group.keys())
    boot = []
    if len(keys) > 1 and n_boot > 0:
        for _ in range(n_boot):
            sample = [r for k in (rng.choice(keys) for _ in keys) for r in by_group[k]]
            boot.append(_prf(*_counts(sample, conf)))
    def _q(i, q):
        vals = [b[i] for b in boot if not math.isnan(b[i])]
        return _fmt(np.percentile(vals, q)) if vals else None

    tp_errs = [r for r in rows if _outcome(r, conf)[0] == 1]
    e_px = [r["centre_error_px"] for r in tp_errs]
    e_rel = [r["centre_error_rel"] for r in tp_errs]
    p_w = _wilson(tp, tp + fp)
    r_w = _wilson(tp, tp + fn)
    dep = [
        {"metric": "n_test_images", "value": len(rows)},
        {"metric": "n_test_groups", "value": len(keys)},
        {"metric": "conf_threshold", "value": conf},
        {"metric": "TP", "value": tp}, {"metric": "FP", "value": fp},
        {"metric": "FN", "value": fn},
        {"metric": "precision", "value": _fmt(p), "wilson_lower": _fmt(p_w[0]),
         "wilson_upper": _fmt(p_w[1]), "boot_lower": _q(0, 2.5), "boot_upper": _q(0, 97.5)},
        {"metric": "recall", "value": _fmt(rc), "wilson_lower": _fmt(r_w[0]),
         "wilson_upper": _fmt(r_w[1]), "boot_lower": _q(1, 2.5), "boot_upper": _q(1, 97.5)},
        {"metric": "F1", "value": _fmt(f1), "boot_lower": _q(2, 2.5), "boot_upper": _q(2, 97.5)},
        {"metric": "centre_error_px_median", "value": _fmt(np.median(e_px)) if e_px else None},
        {"metric": "centre_error_px_mean", "value": _fmt(np.mean(e_px)) if e_px else None},
        {"metric": "centre_error_px_p95", "value": _fmt(np.percentile(e_px, 95)) if e_px else None},
        {"metric": "centre_error_rel_median", "value": _fmt(np.median(e_rel)) if e_rel else None},
        {"metric": "centre_error_rel_p95", "value": _fmt(np.percentile(e_rel, 95)) if e_rel else None},
        {"metric": "best_F1_conf", "value": best["conf"] if best else None},
        {"metric": "best_F1", "value": best["F1"] if best else None},
    ]
    _write_csv(os.path.join(eval_dir, "deployment_metrics.csv"), dep,
               ["metric", "value", "wilson_lower", "wilson_upper",
                "boot_lower", "boot_upper"])

    speed = {k: _fmt(v, 2) for k, v in getattr(metrics, "speed", {}).items()}
    speed["deployment_ms_per_image"] = _fmt(secs_per_img * 1000, 2)
    speed["device"] = "cpu" if device == "cpu" else f"gpu {device}"

    return {
        "standard": std_rows,
        "deployment": {d["metric"]: d.get("value") for d in dep},
        "speed_ms": speed,
    }
