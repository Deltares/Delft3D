"""Aggregate runs with pandas and write CSV, PNG, HTML, and email artifacts."""

from __future__ import annotations

import html
from pathlib import Path

import matplotlib

matplotlib.use("Agg")
import matplotlib.pyplot as plt  # noqa: E402
import pandas as pd

from ci_tools.testbench_timeout_report.teamcity import BUILD_TYPES

AT_RISK_RATIO = 0.8
MIN_SAMPLES = 5
DURATION_CAVEAT = (
    "TeamCity duration is testStarted to testFinished, which includes prepare, "
    "program run, and comparison. XML maxRunTime only kills the program. Duration "
    "can therefore exceed maxRunTime without a program timeout. The ratio is still "
    "an upper bound for tuning: a p95 far below 1.0 means the timeout is loose; "
    "a p95 near 1.0 means it is tight."
)
TABLE_COLS = ["platform", "name", "n", "timeout_s", "p95_s", "p95_ratio", "max_ratio", "timeout_failures"]


def summarize(catalog: pd.DataFrame, runs: pd.DataFrame, failures: pd.DataFrame) -> pd.DataFrame:
    """Join catalog timeouts with run durations. Sorted by ``p95_ratio`` descending."""
    if runs.empty:
        stats = catalog.copy()
        stats["n"] = 0
        for col in ("min_s", "q1_s", "median_s", "q3_s", "max_s", "p95_s", "p95_ratio", "max_ratio"):
            stats[col] = 0.0
    else:
        grouped = runs.groupby(["platform", "name"])["duration_s"].agg(
            n="count",
            min_s="min",
            q1_s=lambda s: s.quantile(0.25),
            median_s="median",
            q3_s=lambda s: s.quantile(0.75),
            max_s="max",
            p95_s=lambda s: s.quantile(0.95),
        )
        stats = catalog.merge(grouped.reset_index(), on=["platform", "name"], how="left")
        stats["n"] = stats["n"].fillna(0).astype(int)
        for col in ("min_s", "q1_s", "median_s", "q3_s", "max_s", "p95_s"):
            stats[col] = stats[col].fillna(0.0)
        stats["p95_ratio"] = stats["p95_s"].div(stats["timeout_s"]).where(stats["timeout_s"] > 0, 0.0)
        stats["max_ratio"] = stats["max_s"].div(stats["timeout_s"]).where(stats["timeout_s"] > 0, 0.0)

    if failures.empty:
        stats["timeout_failures"] = 0
    else:
        stats = stats.merge(failures, on=["platform", "name"], how="left")
        stats["timeout_failures"] = stats["timeout_failures"].fillna(0).astype(int)
    stats["insufficient_data"] = stats["n"] < MIN_SAMPLES
    stats["at_risk"] = ((~stats["insufficient_data"]) & (stats["p95_ratio"] >= AT_RISK_RATIO)) | (
        stats["timeout_failures"] > 0
    )
    return stats.sort_values(["p95_ratio", "max_ratio", "name"], ascending=[False, False, False]).reset_index(drop=True)


def write_artifacts(
    stats: pd.DataFrame,
    runs: pd.DataFrame,
    catalog: pd.DataFrame,
    teamcity_names: dict[str, set[str]],
    output_dir: Path,
    top_n: int,
    report_url: str,
    full_report_url: str,
) -> None:
    """Write cases.csv, platform PNGs, report.html, and email.html."""
    output_dir.mkdir(parents=True, exist_ok=True)
    export = stats.copy()
    export.to_csv(output_dir / "cases.csv", index=False)

    plots: dict[str, str] = {}
    for platform in BUILD_TYPES:
        filename = f"{platform}.png"
        _boxplot(stats, runs, platform, output_dir / filename, top_n)
        plots[platform] = filename

    (output_dir / "report.html").write_text(
        _html_report(stats, catalog, teamcity_names, plots, report_url), encoding="utf-8"
    )
    (output_dir / "email.html").write_text(_email_html(stats, report_url, full_report_url), encoding="utf-8")


def _boxplot(stats: pd.DataFrame, runs: pd.DataFrame, platform: str, path: Path, top_n: int) -> None:
    usable = stats[(stats["platform"] == platform) & ~stats["insufficient_data"]]
    top = usable.head(top_n)
    title = f"{platform} duration / maxRunTime (top {top_n})"
    if top.empty or runs.empty:
        figure, axes = plt.subplots(figsize=(8.0, 2.5))
        axes.set_axis_off()
        axes.set_title(title)
        axes.text(0.5, 0.5, "No cases with at least 5 successful runs.", ha="center", va="center")
        figure.tight_layout()
        figure.savefig(path, dpi=120)
        plt.close(figure)
        return

    merged = runs.merge(top[["platform", "name", "timeout_s"]], on=["platform", "name"])
    merged["ratio"] = merged["duration_s"] / merged["timeout_s"]
    order = list(reversed(top["name"].tolist()))
    data = [merged.loc[merged["name"] == case_name, "ratio"].tolist() for case_name in order]
    labels = [
        f"{case_name}  n={int(n)}  {timeout:.0f}s"
        for case_name, n, timeout in zip(
            reversed(top["name"].tolist()),
            reversed(top["n"].tolist()),
            reversed(top["timeout_s"].tolist()),
            strict=True,
        )
    ]
    figure, axes = plt.subplots(figsize=(12.0, max(4.0, 0.35 * len(order) + 1.5)))
    box = axes.boxplot(data, orientation="horizontal", patch_artist=True, tick_labels=labels)
    for patch in box["boxes"]:
        patch.set_facecolor("#8ecae6")
    axes.axvline(1.0, color="#d62828", linestyle="--", linewidth=1.2, label="maxRunTime")
    axes.set_xlabel("duration / maxRunTime")
    axes.set_title(title)
    axes.grid(axis="x", linestyle=":", alpha=0.6)
    axes.legend(loc="lower right")
    figure.tight_layout()
    figure.savefig(path, dpi=120)
    plt.close(figure)


def _html_report(
    stats: pd.DataFrame,
    catalog: pd.DataFrame,
    teamcity_names: dict[str, set[str]],
    plots: dict[str, str],
    report_url: str,
) -> str:
    at_risk = stats[stats["at_risk"]]
    parts = [
        "<!DOCTYPE html><html lang='en'><head><meta charset='utf-8'/>",
        "<title>TestBench duration vs maxRunTime</title>",
        _style(),
        "</head><body><h1>TestBench duration vs maxRunTime</h1>",
    ]
    if report_url:
        parts.append(f"<p>TeamCity build: <a href='{html.escape(report_url)}'>{html.escape(report_url)}</a></p>")
    parts.append(f"<p class='caveat'>{html.escape(DURATION_CAVEAT)}</p>")
    parts.append(
        "<ul>"
        f"<li>Cases: {len(stats)}</li>"
        f"<li>At risk: {int(stats['at_risk'].sum())}</li>"
        f"<li>p95 ≥ 80% of maxRunTime: "
        f"{int((~stats['insufficient_data'] & (stats['p95_ratio'] >= AT_RISK_RATIO)).sum())}</li>"
        f"<li>With program-timeout failures: {int((stats['timeout_failures'] > 0).sum())}</li>"
        f"<li>Insufficient data (n &lt; 5): {int(stats['insufficient_data'].sum())}</li>"
        "</ul><h2>Closest to timeout</h2>"
    )
    for platform, filename in plots.items():
        parts.append(f"<h3>{html.escape(platform)}</h3>")
        parts.append(f"<img src='{html.escape(filename)}' alt='{html.escape(platform)} boxplot'/>")
    parts.append("<h2>At risk</h2>")
    parts.append(_table(at_risk, "No at-risk cases."))
    parts.append("<h2>All cases</h2>")
    parts.append(_table(stats, "No cases."))
    parts.append("<h2>Unresolved names</h2>")
    for platform in BUILD_TYPES:
        catalog_names = set(catalog.loc[catalog["platform"] == platform, "name"])
        known = teamcity_names.get(platform, set())
        parts.append(f"<h3>{html.escape(platform)}</h3>")
        parts.append(_names("In XML catalog, not in TeamCity", sorted(catalog_names - known)))
        parts.append(_names("In TeamCity, not in XML catalog", sorted(known - catalog_names)))
    parts.append("</body></html>")
    return "\n".join(parts)


def _email_html(stats: pd.DataFrame, report_url: str, full_report_url: str, top_n: int = 20) -> str:
    at_risk = stats[stats["at_risk"]]
    href = full_report_url or report_url
    link = (
        f"<p><a href='{html.escape(href)}'>Open the full report</a></p>"
        if href
        else "<p>Full report is attached to the TeamCity build artifacts.</p>"
    )
    p95_count = int((~stats["insufficient_data"] & (stats["p95_ratio"] >= AT_RISK_RATIO)).sum())
    timeout_count = int((stats["timeout_failures"] > 0).sum())
    return "\n".join(
        [
            "<html><body><h2>TestBench duration vs maxRunTime</h2>",
            (
                f"<p>{p95_count} cases with p95 ≥ 80% of maxRunTime. "
                f"{timeout_count} cases with recent program-timeout failures. "
                f"{int(at_risk.shape[0])} cases at risk in total.</p>"
            ),
            link,
            "<p>Top cases closest to timeout:</p>",
            _table(at_risk.head(top_n), "No at-risk cases."),
            f"<p class='caveat'>{html.escape(DURATION_CAVEAT)}</p>",
            "</body></html>",
        ]
    )


def _table(frame: pd.DataFrame, empty_message: str) -> str:
    if frame.empty:
        return f"<p>{html.escape(empty_message)}</p>"
    view = frame[TABLE_COLS].copy()
    view["timeout_s"] = view["timeout_s"].map(lambda v: f"{v:.0f}")
    view["p95_s"] = view["p95_s"].map(lambda v: f"{v:.1f}")
    view["p95_ratio"] = view["p95_ratio"].map(lambda v: f"{v:.2f}")
    view["max_ratio"] = view["max_ratio"].map(lambda v: f"{v:.2f}")
    return str(view.to_html(index=False, escape=True, border=0))


def _names(title: str, names: list[str], limit: int = 50) -> str:
    shown = names[:limit]
    extra = len(names) - len(shown)
    items = "".join(f"<li>{html.escape(name)}</li>" for name in shown) or "<li>None</li>"
    suffix = f"<p>{extra} more omitted.</p>" if extra else ""
    return f"<h4>{html.escape(title)} ({len(names)})</h4><ul>{items}</ul>{suffix}"


def _style() -> str:
    return (
        "<style>body{font-family:sans-serif;margin:1.5rem}"
        "table{border-collapse:collapse;width:100%;font-size:0.9rem}"
        "th,td{border:1px solid #ccc;padding:0.3rem 0.5rem;text-align:left}"
        "th{background:#f3f3f3}tr.at-risk{background:#ffe3e0}"
        "img{max-width:100%;height:auto}.caveat{max-width:70rem}</style>"
    )
