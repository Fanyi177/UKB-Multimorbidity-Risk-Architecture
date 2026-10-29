#!/usr/bin/env python3
"""Collect prediction, domain-combination and feature-stability outputs for R."""

import math
import os
from pathlib import Path

import numpy as np
import pandas as pd

import config_sensitivity as cfg


SENS_STAGE2_FOLDER = cfg.stage3_stage2_result_folder
PRIMARY_STAGE2_FOLDER = cfg.primary_stage2_result_folder
STAGE3_FOLDER = cfg.stage3_result_folder
STAGE3B_FOLDER = "Stage 3B"
PRIMARY_RES = Path(cfg.primary_result_dir)
SENS_RES = Path(cfg.res_dir)
OUT_DIR = Path(cfg.sensitivity_root) / "For_R"
OUT_DIR.mkdir(parents=True, exist_ok=True)


def tasks():
    return [
        (c_type, target)
        for c_type in cfg.comorbidity_types
        for target in cfg.comorbidity_targets[c_type]
    ]


def outcome_label(c_type, target):
    if c_type == "comorbidity":
        return {"all": "Overall", "male": "Men", "female": "Women"}[target]
    return "P" + c_type.rsplit("_", 1)[-1]


def read_one(path):
    if not path.exists():
        raise FileNotFoundError(str(path))
    return pd.read_csv(path)


def stage3_perf(root, c_type, target):
    p = root / STAGE3_FOLDER / f"{c_type}_{target}" / "performance.csv"
    x = read_one(p)
    if len(x) != 1:
        raise ValueError(f"Expected one performance row: {p}")
    return x.iloc[0]


def stage2_folder_for_root(root):
    root = Path(root)
    if root == SENS_RES:
        return SENS_STAGE2_FOLDER
    configured = root / PRIMARY_STAGE2_FOLDER
    if configured.exists():
        return PRIMARY_STAGE2_FOLDER
    fallback = root / "stage_parallel"
    if fallback.exists():
        return "stage_parallel"
    return PRIMARY_STAGE2_FOLDER


def event_counts(root, c_type, target):
    # Any Stage 2 view has the same labels/eids. Use enviro as the reference.
    base = root / stage2_folder_for_root(root) / f"{c_type}_{target}_enviro"
    train = read_one(base / "train_data.csv")
    test = read_one(base / "test_data.csv")
    return {
        "n_development": len(train),
        "n_development_events": int(pd.to_numeric(train["status"], errors="coerce").fillna(0).sum()),
        "n_heldout": len(test),
        "n_heldout_events": int(pd.to_numeric(test["status"], errors="coerce").fillna(0).sum()),
    }


def fixed_horizon(root, c_type, target):
    p = root / STAGE3_FOLDER / f"{c_type}_{target}" / "fixed_horizon_performance.csv"
    if not p.exists():
        return {}
    x = pd.read_csv(p)
    out = {}
    for _, row in x.iterrows():
        years = row.get("horizon_years", np.nan)
        if pd.isna(years):
            continue
        key = f"auc_{float(years):g}y"
        out[key] = row.get("time_dependent_auc", np.nan)
        out[f"calibration_diff_{float(years):g}y"] = row.get(
            "calibration_difference_pred_minus_observed", np.nan
        )
    return out


def stage3b_wide(root, c_type, target):
    p = root / STAGE3B_FOLDER / f"{c_type}_{target}" / "stage3b_combination_performance.csv"
    if not p.exists():
        return {}
    x = pd.read_csv(p)
    out = {}
    for _, row in x.iterrows():
        combo = str(row["combination"]).replace("+", "_")
        for metric in (
            "mean_time_dependent_auc", "c_index", "auc_3y", "auc_5y",
            "auc_10y", "auc_15y",
        ):
            if metric in row:
                out[f"{metric}_{combo}"] = row[metric]
        delta_col = "delta_mean_time_dependent_auc_vs_E"
        if delta_col in row:
            out[f"delta_mean_auc_vs_E_{combo}"] = row[delta_col]
    return out


def selected_feature_count(root, c_type, target, view):
    p = root / stage2_folder_for_root(root) / f"{c_type}_{target}_{view}" / "feature_list.csv"
    return len(read_one(p))


def build_master_summary():
    rows = []
    for c_type, target in tasks():
        sens = stage3_perf(SENS_RES, c_type, target)
        try:
            primary = stage3_perf(PRIMARY_RES, c_type, target)
        except FileNotFoundError:
            primary = pd.Series(dtype=float)
        counts = event_counts(SENS_RES, c_type, target)

        sens_auc = sens.get("mean_time_dependent_auc", np.nan)
        pri_auc = primary.get("mean_time_dependent_auc", np.nan)
        sens_c = sens.get("c_index", np.nan)
        pri_c = primary.get("c_index", np.nan)

        row = {
            "sensitivity": cfg.sensitivity_name,
            "outcome": outcome_label(c_type, target),
            "c_type": c_type,
            "target": target,
            **counts,
            "mean_time_dependent_auc": sens_auc,
            "primary_mean_time_dependent_auc": pri_auc,
            "delta_auc_vs_primary": sens_auc - pri_auc if np.isfinite(pri_auc) else np.nan,
            "c_index": sens_c,
            "primary_c_index": pri_c,
            "delta_cindex_vs_primary": sens_c - pri_c if np.isfinite(pri_c) else np.nan,
            "ibs": sens.get("ibs", np.nan),
            "primary_ibs": primary.get("ibs", np.nan),
            "n_integrated_features": sens.get("n_final_features", np.nan),
        }
        row.update(fixed_horizon(SENS_RES, c_type, target))
        row.update(stage3b_wide(SENS_RES, c_type, target))

        for code, view in (("E", "enviro"), ("B", "biomarkers"), ("M", "metabolomics"), ("G", "gene")):
            row[f"n_{code}_features"] = selected_feature_count(SENS_RES, c_type, target, view)

        rows.append(row)

    out = pd.DataFrame(rows)
    out.to_csv(OUT_DIR / "sensitivity_full_summary_for_R.csv", index=False)
    return out


def read_feature_list(root, c_type, target, view):
    p = root / stage2_folder_for_root(root) / f"{c_type}_{target}_{view}" / "feature_list.csv"
    x = read_one(p)
    return x["Feature"].astype(str).tolist()


def read_signed(root, c_type, target, view):
    p = root / stage2_folder_for_root(root) / f"{c_type}_{target}_{view}" / "signed_feature_importance.csv"
    if not p.exists():
        return pd.Series(dtype=float)
    x = pd.read_csv(p)
    if "Feature" not in x.columns or "Signed_importance" not in x.columns:
        return pd.Series(dtype=float)
    return pd.Series(
        pd.to_numeric(x["Signed_importance"], errors="coerce").to_numpy(),
        index=x["Feature"].astype(str),
    )


def corr_on_overlap(a, b, overlap):
    if len(overlap) < 2:
        return np.nan
    x = np.asarray([a.get(k, np.nan) for k in overlap], dtype=float)
    y = np.asarray([b.get(k, np.nan) for k in overlap], dtype=float)
    ok = np.isfinite(x) & np.isfinite(y)
    if ok.sum() < 2 or np.nanstd(x[ok]) == 0 or np.nanstd(y[ok]) == 0:
        return np.nan
    return float(np.corrcoef(x[ok], y[ok])[0, 1])


def build_feature_stability():
    rows = []
    for c_type, target in tasks():
        for view in cfg.feat_name_dict:
            try:
                p_list = read_feature_list(PRIMARY_RES, c_type, target, view)
                p_signed = read_signed(PRIMARY_RES, c_type, target, view)
            except FileNotFoundError:
                p_list = []
                p_signed = pd.Series(dtype=float)
            s_list = read_feature_list(SENS_RES, c_type, target, view)
            p_set, s_set = set(p_list), set(s_list)
            overlap = sorted(p_set & s_set)
            union = p_set | s_set
            s_signed = read_signed(SENS_RES, c_type, target, view)

            rows.append({
                "sensitivity": cfg.sensitivity_name,
                "outcome": outcome_label(c_type, target),
                "c_type": c_type,
                "target": target,
                "view": view,
                "primary_n_features": len(p_list),
                "sensitivity_n_features": len(s_list),
                "n_overlap": len(overlap),
                "jaccard_index": len(overlap) / len(union) if union else np.nan,
                "top10_overlap": len(set(p_list[:10]) & set(s_list[:10])),
                "top20_overlap": len(set(p_list[:20]) & set(s_list[:20])),
                "signed_importance_pearson_on_overlap": corr_on_overlap(
                    p_signed, s_signed, overlap
                ),
            })

    out = pd.DataFrame(rows)
    out.to_csv(OUT_DIR / "sensitivity_feature_stability_for_R.csv", index=False)
    return out


def copy_stage3b_long():
    frames = []
    for c_type, target in tasks():
        p = SENS_RES / STAGE3B_FOLDER / f"{c_type}_{target}" / "stage3b_combination_performance.csv"
        if not p.exists():
            continue
        x = pd.read_csv(p)
        x.insert(0, "outcome", outcome_label(c_type, target))
        x.insert(0, "sensitivity", cfg.sensitivity_name)
        frames.append(x)
    if frames:
        pd.concat(frames, ignore_index=True).to_csv(
            OUT_DIR / "sensitivity_stage3b_combination_performance_for_R.csv",
            index=False,
        )


def main():
    summary = build_master_summary()
    stability = build_feature_stability()
    copy_stage3b_long()
    print(f"Saved {len(summary)} outcome rows to {OUT_DIR}")
    print(f"Saved {len(stability)} feature-stability rows to {OUT_DIR}")


if __name__ == "__main__":
    main()
