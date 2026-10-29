#!/usr/bin/env python3
"""Execute an existing project script under the independent sensitivity config.

No primary-analysis script is changed or imported with its primary config.
Stage 3/3B AUC protection is activated only in this sensitivity process.
"""

import os
import runpy
import sys

if len(sys.argv) < 2:
    raise SystemExit(
        "Usage: python run_with_sensitivity_config.py <script.py> [script args...]"
    )

script = sys.argv[1]
script_args = sys.argv[2:]
if not os.path.exists(script):
    raise FileNotFoundError(script)

import config_sensitivity as sensitivity_config
sys.modules["config"] = sensitivity_config

import CoxEM_sensitivity as sensitivity_coxem
sys.modules["CoxEM"] = sensitivity_coxem


def _install_final_stage_auc_safeguard():
    """Preserve original year-specific AUCs; compute mean using estimable years.

    scikit-survival returns an undefined AUC when there are no cases (or no
    controls) at an assessment time.  Such years stay NaN in AUC curves.
    We NEVER replace an undefined AUC with an invented value or silently use
    a different population. Only Stage 3/3B are patched; Stage 2 uses its
    explicit fold-level event-supported calculation.
    """
    import warnings
    import numpy as np
    import sksurv.metrics as survival_metrics

    original_auc = survival_metrics.cumulative_dynamic_auc

    def sensitivity_auc(y_train, y_test, risk, times, *args, **kwargs):
        grid = np.atleast_1d(np.asarray(times, dtype=float))
        try:
            with warnings.catch_warnings():
                warnings.filterwarnings(
                    "ignore", message="invalid value encountered in divide",
                    category=RuntimeWarning,
                )
                values, average = original_auc(
                    y_train, y_test, risk, grid, *args, **kwargs
                )
            if np.all(np.isfinite(values)) and np.isfinite(average):
                return values, average
        except (ValueError, ZeroDivisionError):
            # If an originally requested horizon is outside training support,
            # keep the normal error rather than silently modifying it.
            raise

        event_key, time_key = y_test.dtype.names
        test_events = np.asarray(y_test[event_key], dtype=bool)
        test_times = np.asarray(y_test[time_key], dtype=float)
        min_cases = int(getattr(sensitivity_config, "stage2_auc_min_cases", 2))
        min_controls = int(getattr(sensitivity_config, "stage2_auc_min_controls", 2))
        estimable_times = np.array([
            t for t in grid
            if np.count_nonzero(test_events & (test_times <= t)) >= min_cases
            and np.count_nonzero(test_times > t) >= min_controls
        ], dtype=float)
        auc_curve = np.full(len(grid), np.nan, dtype=float)
        if len(estimable_times) == 0:
            return auc_curve, float("nan")

        # The same criterion is applied to each horizon independent of risk.
        retained_times = []
        retained_auc = []
        for t in estimable_times:
            with warnings.catch_warnings():
                warnings.filterwarnings(
                    "ignore", message="invalid value encountered in divide",
                    category=RuntimeWarning,
                )
                auc_t, _ = original_auc(
                    y_train, y_test, risk, np.asarray([t]), *args, **kwargs
                )
            if np.all(np.isfinite(auc_t)):
                retained_times.append(float(t))
                retained_auc.append(float(auc_t[0]))
        if not retained_times:
            return auc_curve, float("nan")

        with warnings.catch_warnings():
            warnings.filterwarnings(
                "ignore", message="invalid value encountered in divide",
                category=RuntimeWarning,
            )
            checked_auc, checked_mean = original_auc(
                y_train, y_test, risk, np.asarray(retained_times),
                *args, **kwargs,
            )
        if not np.all(np.isfinite(checked_auc)) or not np.isfinite(checked_mean):
            return auc_curve, float("nan")
        for t, v in zip(retained_times, checked_auc):
            auc_curve[np.isclose(grid, t, rtol=0, atol=1e-6)] = v
        return auc_curve, float(checked_mean)

    survival_metrics.cumulative_dynamic_auc = sensitivity_auc


if os.path.basename(script) in {"run_stage3.py", "run_stage3b.py"}:
    _install_final_stage_auc_safeguard()

sys.argv = [script] + script_args
runpy.run_path(script, run_name="__main__")
