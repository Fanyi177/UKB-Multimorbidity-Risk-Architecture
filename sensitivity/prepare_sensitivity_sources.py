#!/usr/bin/env python3
"""Prepare/validate source labels for the four prespecified sensitivity analyses.

The disease definitions and P1-P8 definitions are never reconstructed here.
This script only subsets the canonical final outcome file or resets the time
origin for the 2-year landmark analysis.
"""

import argparse
import json
import os
from pathlib import Path

import numpy as np
import pandas as pd

PROJECT_DIR = Path(__file__).resolve().parent
SOURCE_ROOT = PROJECT_DIR / "Source"
PRIMARY_LABEL = SOURCE_ROOT / "UKB_diseases_data_with_patterns.csv"
PRIMARY_DATA_DIR = PROJECT_DIR / "Data_2"
LANDMARK_DAYS = 730.0

ALLOWED = (
    "domain_availability",
    "disease_free",
    "landmark_2y",
    "simple_imputation",
)

PATTERN_FOLLOW_COLS = [
    f"comorbidity_pattern_{i}_follow_time" for i in range(1, 9)
]
PATTERN_EVENT_COLS = [
    f"comorbidity_pattern_{i}" for i in range(1, 9)
]


def require(path: Path) -> Path:
    if not path.exists():
        raise FileNotFoundError(str(path))
    return path


def read_csv(path: Path) -> pd.DataFrame:
    return pd.read_csv(path, low_memory=False)


def require_columns(df: pd.DataFrame, columns, label):
    missing = [c for c in columns if c not in df.columns]
    if missing:
        raise ValueError(f"{label}: missing required columns: {missing}")


def assert_unique_eid(df: pd.DataFrame, label: str):
    if "eid" not in df.columns:
        raise ValueError(f"{label}: no eid column")
    if df["eid"].duplicated().any():
        raise ValueError(f"{label}: duplicated eid")


def primary_split_sets():
    train_file = require(PRIMARY_DATA_DIR / "comorbidity_all_independent_train_label.csv")
    test_file = require(PRIMARY_DATA_DIR / "comorbidity_all_independent_test_label.csv")
    train = pd.read_csv(train_file, index_col=0)
    test = pd.read_csv(test_file, index_col=0)
    train_ids = set(train.index.astype(str))
    test_ids = set(test.index.astype(str))
    if train_ids & test_ids:
        raise ValueError("Primary development and held-out split overlap")
    return train_ids, test_ids


def save_manifest(name: str, source_label: Path, output_label: Path, df: pd.DataFrame, extra: dict):
    sens_root = PROJECT_DIR / "Sensitivity" / name
    sens_root.mkdir(parents=True, exist_ok=True)

    train_ids, test_ids = primary_split_sets()
    ids = set(df["eid"].astype(str))

    manifest = {
        "sensitivity": name,
        "source_label": str(source_label),
        "output_label": str(output_label),
        "n_source_rows": int(extra.pop("n_source_rows", len(df))),
        "n_prepared_rows": int(len(df)),
        "n_in_primary_development_before_time_cap": int(len(ids & train_ids)),
        "n_in_primary_heldout_before_time_cap": int(len(ids & test_ids)),
        **extra,
    }
    pd.DataFrame([manifest]).to_csv(sens_root / "source_manifest.csv", index=False)
    with open(sens_root / "source_manifest.json", "w", encoding="utf-8") as f:
        json.dump(manifest, f, ensure_ascii=False, indent=2)
    print(json.dumps(manifest, ensure_ascii=False, indent=2))


def prepare_domain_availability():
    src = SOURCE_ROOT / "sensitivity_analysis"
    files = {
        "enviro": src / "final_environment_2.csv",
        "biomarkers": src / "final_biomarkers.csv",
        "metabolomics": src / "final_metabolomics.csv",
        "gene": src / "final_genetics.csv",
        "sex": src / "final_eid_sex.csv",
        "labels": src / "UKB_diseases_data_with_patterns.csv",
    }
    for path in files.values():
        require(path)

    labels = read_csv(files["labels"])
    assert_unique_eid(labels, "domain-availability labels")
    require_columns(
        labels,
        ["baseline_disease_count", "comorbidity", "follow_time"]
        + PATTERN_EVENT_COLS + PATTERN_FOLLOW_COLS,
        "domain-availability labels",
    )

    # Strong membership QA across all four views and sex file.  Row order does
    # not need to match because data_preprocess reindexes every view by eid.
    ref_ids = set(labels["eid"].astype(str))
    for key in ("enviro", "biomarkers", "metabolomics", "gene", "sex"):
        frame = read_csv(files[key])
        assert_unique_eid(frame, key)
        ids = set(frame["eid"].astype(str))
        if ids != ref_ids:
            raise ValueError(
                f"domain_availability: eid membership mismatch between labels and {key}; "
                f"labels_only={len(ref_ids - ids)}, {key}_only={len(ids - ref_ids)}"
            )

    save_manifest(
        "domain_availability",
        files["labels"],
        files["labels"],
        labels,
        {"n_source_rows": len(labels), "landmark_days": np.nan},
    )


def prepare_disease_free():
    source = require(PRIMARY_LABEL)
    df = read_csv(source)
    assert_unique_eid(df, "primary labels")
    require_columns(df, ["baseline_disease_count", "comorbidity", "follow_time"], "primary labels")

    out = df.loc[df["baseline_disease_count"] == 0].copy()
    if not (out["baseline_disease_count"] == 0).all():
        raise AssertionError("Disease-free restriction failed")

    out_dir = SOURCE_ROOT / "sensitivity_generated" / "disease_free"
    out_dir.mkdir(parents=True, exist_ok=True)
    out_file = out_dir / "UKB_diseases_data_with_patterns.csv"
    out.to_csv(out_file, index=False)

    save_manifest(
        "disease_free",
        source,
        out_file,
        out,
        {
            "n_source_rows": len(df),
            "n_excluded_baseline_disease_count_1": int((df["baseline_disease_count"] == 1).sum()),
            "landmark_days": np.nan,
        },
    )


def prepare_landmark():
    source = require(PRIMARY_LABEL)
    df = read_csv(source)
    assert_unique_eid(df, "primary labels")
    require_columns(
        df,
        ["comorbidity", "follow_time", "censor_date"]
        + PATTERN_EVENT_COLS + PATTERN_FOLLOW_COLS,
        "primary labels",
    )

    original_n = len(df)
    original_follow = pd.to_numeric(df["follow_time"], errors="coerce")

    # Landmark analysis starts from the same primary enviro-2 analysis population.
    # This prevents subtraction of 730 days from admitting participants who were
    # outside the original <=6100-day primary preprocessing window.
    primary_train_ids, primary_test_ids = primary_split_sets()
    primary_ids = primary_train_ids | primary_test_ids
    in_primary_split = df["eid"].astype(str).isin(primary_ids)
    eligible = (original_follow > LANDMARK_DAYS) & in_primary_split
    out = df.loc[eligible].copy()

    # Keep the original times for transparent QA, then reset the time origin.
    out["original_follow_time"] = pd.to_numeric(out["follow_time"], errors="coerce")
    out["follow_time"] = out["original_follow_time"] - LANDMARK_DAYS

    for col in PATTERN_FOLLOW_COLS:
        out[f"original_{col}"] = pd.to_numeric(out[col], errors="coerce")
        out[col] = out[f"original_{col}"] - LANDMARK_DAYS

    if (out["follow_time"] <= 0).any() or out["follow_time"].isna().any():
        raise ValueError("Landmark overall follow-up is non-positive/missing after resetting time origin")
    for col in PATTERN_FOLLOW_COLS:
        if (out[col] <= 0).any() or out[col].isna().any():
            bad = int(((out[col] <= 0) | out[col].isna()).sum())
            raise ValueError(f"Landmark {col}: {bad} non-positive/missing values")

    if "recruitment_date" in out.columns:
        recruitment = pd.to_datetime(out["recruitment_date"], errors="coerce")
        out["landmark_date"] = (
            recruitment + pd.to_timedelta(int(LANDMARK_DAYS), unit="D")
        ).dt.strftime("%Y-%m-%d")

    # Extra exclusion QA.  For event cases, follow_time<=730 means an early event.
    # For non-events, distinguish death from other end-of-follow-up when dates allow.
    status = pd.to_numeric(df["comorbidity"], errors="coerce")
    early_event = (status == 1) & (original_follow <= LANDMARK_DAYS)
    early_non_event_end = (status == 0) & (original_follow <= LANDMARK_DAYS)

    recruitment_all = pd.to_datetime(df.get("recruitment_date"), errors="coerce")
    death_all = pd.to_datetime(df.get("death_date"), errors="coerce")
    landmark_all = recruitment_all + pd.to_timedelta(int(LANDMARK_DAYS), unit="D")
    early_death = early_non_event_end & death_all.notna() & (death_all <= landmark_all)
    early_other_followup_end = early_non_event_end & ~early_death

    out_dir = SOURCE_ROOT / "sensitivity_generated" / "landmark_2y"
    out_dir.mkdir(parents=True, exist_ok=True)
    out_file = out_dir / "UKB_diseases_data_with_patterns.csv"
    out.to_csv(out_file, index=False)

    save_manifest(
        "landmark_2y",
        source,
        out_file,
        out,
        {
            "n_source_rows": original_n,
            "landmark_days": int(LANDMARK_DAYS),
            "n_excluded_total_from_source": int((~eligible).sum()),
            "n_not_in_primary_enviro2_split": int((~in_primary_split).sum()),
            "n_excluded_early_multimorbidity": int((early_event & in_primary_split).sum()),
            "n_excluded_early_death_without_prior_multimorbidity": int((early_death & in_primary_split).sum()),
            "n_excluded_early_other_followup_end": int((early_other_followup_end & in_primary_split).sum()),
            "n_excluded_early_non_event_followup_end": int((early_non_event_end & in_primary_split).sum()),
            "note": "Early-death and other-follow-up-end counts are hierarchical among participants without early multimorbidity.",
        },
    )


def prepare_simple_imputation():
    source = require(PRIMARY_LABEL)
    df = read_csv(source)
    assert_unique_eid(df, "primary labels")
    require_columns(df, ["comorbidity", "follow_time"], "primary labels")
    save_manifest(
        "simple_imputation",
        source,
        source,
        df,
        {
            "n_source_rows": len(df),
            "landmark_days": np.nan,
            "imputation": "SimpleImputer(strategy=median)",
        },
    )


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--sensitivity", required=True, choices=ALLOWED)
    args = parser.parse_args()

    require(PRIMARY_DATA_DIR / "comorbidity_all_independent_train_label.csv")
    require(PRIMARY_DATA_DIR / "comorbidity_all_independent_test_label.csv")

    if args.sensitivity == "domain_availability":
        prepare_domain_availability()
    elif args.sensitivity == "disease_free":
        prepare_disease_free()
    elif args.sensitivity == "landmark_2y":
        prepare_landmark()
    else:
        prepare_simple_imputation()


if __name__ == "__main__":
    main()
