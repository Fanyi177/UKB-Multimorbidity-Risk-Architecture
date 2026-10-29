#!/usr/bin/env python3
"""Run data_preprocess_sensitivity.py with safer atomic/retried CSV writes.

The original sensitivity preprocessing script is not modified.  This wrapper is
intended to be launched through run_with_sensitivity_config.py so that the active
sensitivity config has already been exposed as `config`.
"""

import os
import runpy
import sys
import time
from pathlib import Path

import pandas as pd

_ORIGINAL_TO_CSV = pd.DataFrame.to_csv


def _safe_to_csv(self, path_or_buf=None, *args, **kwargs):
    # Preserve normal pandas behaviour for file-like buffers or stdout.
    if path_or_buf is None or not isinstance(path_or_buf, (str, os.PathLike)):
        return _ORIGINAL_TO_CSV(self, path_or_buf, *args, **kwargs)

    final_path = os.fspath(path_or_buf)
    parent = os.path.dirname(final_path) or "."
    os.makedirs(parent, exist_ok=True)

    chunksize = kwargs.pop("chunksize", None)
    if chunksize is None:
        chunksize = int(os.environ.get("SENS_CSV_CHUNKSIZE", "500"))

    max_retries = int(os.environ.get("SENS_IO_MAX_RETRIES", "5"))
    tmp_path = final_path + f".tmp.{os.getpid()}"

    for attempt in range(max_retries):
        try:
            if os.path.exists(tmp_path):
                os.remove(tmp_path)

            result = _ORIGINAL_TO_CSV(
                self,
                tmp_path,
                *args,
                chunksize=chunksize,
                **kwargs,
            )
            os.replace(tmp_path, final_path)
            return result

        except OSError as exc:
            try:
                if os.path.exists(tmp_path):
                    os.remove(tmp_path)
            except OSError:
                pass

            # Errno 7 (E2BIG) was observed transiently on Lustre.
            if exc.errno != 7 or attempt == max_retries - 1:
                raise

            wait_seconds = 10 * (attempt + 1)
            print(
                f"CSV write hit errno 7 for {final_path}; "
                f"retry {attempt + 1}/{max_retries} after {wait_seconds}s.",
                file=sys.stderr,
                flush=True,
            )
            time.sleep(wait_seconds)


pd.DataFrame.to_csv = _safe_to_csv

# Keep command-line arguments (e.g. --all-only / --derive-only) unchanged.
runpy.run_path("data_preprocess_sensitivity.py", run_name="__main__")
