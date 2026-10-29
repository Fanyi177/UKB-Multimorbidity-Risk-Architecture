#!/usr/bin/env python3
"""Run all sensitivity Stage 1 CoxEM folds.

Uses run_model_sensitivity.py so the primary run_model.py is never modified.
Completed fold outputs are skipped by default, allowing interrupted runs to resume.
"""

import argparse
import concurrent.futures
import os
import subprocess
import sys

import config_sensitivity as cfg


def result_path(c_type, target, view):
    if c_type == "comorbidity" and target == "all":
        return os.path.join(
            cfg.res_dir,
            f"{c_type}_{target}_{view}",
            f"sample_ratio={cfg.sample_ratio} & event_censor_ratio=fold & "
            f"fl_corr_type={cfg.fl_corr_type} & min_fl_c_corr={cfg.min_fl_c_corr} & "
            f"n_fea_sel={cfg.n_fea_sel}",
        )
    return os.path.join(cfg.res_dir, f"{c_type}_{target}_{view}")


def is_complete(c_type, target, view, task_id):
    base = result_path(c_type, target, view)
    required = [
        os.path.join(base, f"CoxEM_Cox_perf_{cfg.fold_type}_{task_id}.csv"),
        os.path.join(base, f"CoxEM_Cox_raw_fi_{cfg.fold_type}_{task_id}.csv"),
        os.path.join(base, f"CoxEM_Cox_raw_signed_fi_{cfg.fold_type}_{task_id}.csv"),
        os.path.join(base, f"CoxEM_Cox_raw_beta_{cfg.fold_type}_{task_id}.csv"),
        os.path.join(base, f"CoxEM_Cox_raw_se_{cfg.fold_type}_{task_id}.csv"),
    ]
    return all(os.path.exists(x) and os.path.getsize(x) > 0 for x in required)


def run_one(c_type, target, view, task_id, force=False):
    if not force and is_complete(c_type, target, view, task_id):
        print(f"SKIP completed: {c_type}/{target}/{view}/fold{task_id}", flush=True)
        return

    cmd = [
        sys.executable,
        "run_with_sensitivity_config.py",
        "run_model_sensitivity.py",
        "--c_type", c_type,
        "--target", target,
        "--view", view,
        "--task_id", str(task_id),
    ]
    print("RUN", " ".join(cmd), flush=True)
    subprocess.run(cmd, check=True)


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "--max-workers",
        type=int,
        default=int(os.environ.get("SENS_STAGE1_MAX_WORKERS", "1")),
    )
    parser.add_argument("--force", action="store_true")
    args = parser.parse_args()

    tasks = []
    for c_type in cfg.comorbidity_types:
        for target in cfg.comorbidity_targets[c_type]:
            for view in cfg.feat_name_dict:
                for task_id in range(cfg.n_process):
                    tasks.append((c_type, target, view, task_id))

    print(
        f"Sensitivity={cfg.sensitivity_name}; Stage1 tasks={len(tasks)}; "
        f"max_workers={args.max_workers}; CoxEM_n_jobs={cfg.CoxEM_n_jobs}",
        flush=True,
    )

    with concurrent.futures.ThreadPoolExecutor(max_workers=args.max_workers) as ex:
        futures = [
            ex.submit(run_one, *task, force=args.force)
            for task in tasks
        ]
        for future in concurrent.futures.as_completed(futures):
            future.result()


if __name__ == "__main__":
    main()
