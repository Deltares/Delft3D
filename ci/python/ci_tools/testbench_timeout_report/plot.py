"""Plot duration/timeout ratio boxplots."""

from __future__ import annotations

from pathlib import Path

import matplotlib

matplotlib.use("Agg")
import matplotlib.pyplot as plt  # noqa: E402

from ci_tools.testbench_timeout_report.aggregate import CaseStats


def plot_ratio_boxplots(
    stats: list[CaseStats],
    output_path: Path,
    title: str,
    top_n: int = 40,
) -> list[CaseStats]:
    """Write a horizontal boxplot of duration/timeout for the closest cases.

    Parameters
    ----------
    stats : list[CaseStats]
        Case statistics, already sorted by closeness to timeout.
    output_path : Path
        PNG path.
    title : str
        Plot title.
    top_n : int, optional
        Maximum number of cases to draw. Defaults to 40.

    Returns
    -------
    list[CaseStats]
        Cases that were plotted. Empty when nothing qualifies.
    """
    plotted = [item for item in stats if not item.insufficient_data][:top_n]
    output_path.parent.mkdir(parents=True, exist_ok=True)
    if not plotted:
        _write_empty_plot(output_path, title)
        return []

    # Horizontal boxplots put the first series at the bottom; reverse so the
    # closest-to-timeout cases appear at the top.
    draw = list(reversed(plotted))
    ratios = [[duration / item.timeout_s for duration in item.durations_s] for item in draw]
    labels = [f"{item.name}  n={item.n}  {item.timeout_s:.0f}s" for item in draw]
    height = max(4.0, 0.35 * len(plotted) + 1.5)
    figure, axes = plt.subplots(figsize=(12.0, height))
    boxplot = axes.boxplot(ratios, orientation="horizontal", patch_artist=True, tick_labels=labels)
    for patch in boxplot["boxes"]:
        patch.set_facecolor("#8ecae6")
    axes.axvline(1.0, color="#d62828", linestyle="--", linewidth=1.2, label="maxRunTime")
    axes.set_xlabel("duration / maxRunTime")
    axes.set_title(title)
    axes.grid(axis="x", linestyle=":", alpha=0.6)
    axes.legend(loc="lower right")
    figure.tight_layout()
    figure.savefig(output_path, dpi=120)
    plt.close(figure)
    return plotted


def _write_empty_plot(output_path: Path, title: str) -> None:
    figure, axes = plt.subplots(figsize=(8.0, 2.5))
    axes.set_axis_off()
    axes.set_title(title)
    axes.text(0.5, 0.5, "No cases with at least 5 successful runs.", ha="center", va="center")
    figure.tight_layout()
    figure.savefig(output_path, dpi=120)
    plt.close(figure)
