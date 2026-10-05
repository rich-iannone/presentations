"""Simple summary statistics."""

from __future__ import annotations

import math
import warnings


def summarize(values: list[float]) -> dict[str, float]:
    """Compute summary statistics for a list of numbers.

    Missing values (`None` and NaN) are dropped with a warning.
    """
    clean = [v for v in values if v is not None and not math.isnan(v)]
    n_dropped = len(values) - len(clean)
    if n_dropped:
        warnings.warn(f"Dropped {n_dropped} missing value(s)", UserWarning, stacklevel=2)

    if not clean:
        raise ValueError("Cannot summarize an empty list of values")

    n = len(clean)
    mean = sum(clean) / n
    var = sum((v - mean) ** 2 for v in clean) / (n - 1) if n > 1 else 0.0

    return {
        "n": n,
        "mean": mean,
        "sd": math.sqrt(var),
        "min": min(clean),
        "max": max(clean),
    }
