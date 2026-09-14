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
) -> None:
    """Write cases.csv, platform PNGs, report.html, and email.html."""
    output_dir.mkdir(parents=True, exist_ok=True)
    stats.to_csv(output_dir / "cases.csv", index=False)
    plots = {platform: f"{platform}.png" for platform in BUILD_TYPES}
    for platform, filename in plots.items():
        _boxplot(stats, runs, platform, output_dir / filename, top_n)
    (output_dir / "report.html").write_text(
        _html_report(stats, catalog, teamcity_names, plots, report_url), encoding="utf-8"
    )
    (output_dir / "email.html").write_text(_email_html(stats, report_url), encoding="utf-8")


def _boxplot(stats: pd.DataFrame, runs: pd.DataFrame, platform: str, path: Path, top_n: int) -> None:
    usable = stats.loc[(stats["platform"] == platform) & ~stats["insufficient_data"]]
    top = usable.head(top_n)
    title = f"{platform} duration / maxRunTime (top {top_n})"
    height = max(4.0, 0.35 * max(len(top), 1) + 1.5)
    figure, axes = plt.subplots(figsize=(12.0, height))
    if top.empty or runs.empty:
        axes.set_axis_off()
        axes.set_title(title)
        axes.text(0.5, 0.5, "No cases with at least 5 successful runs.", ha="center", va="center")
        figure.tight_layout()
        figure.savefig(path, dpi=120)
        plt.close(figure)
        return

    merged = runs.merge(top[["platform", "name", "timeout_s"]], on=["platform", "name"])
    merged["ratio"] = merged["duration_s"] / merged["timeout_s"]
    label_for = {
        str(name): f"{name}  n={int(n)}  {timeout:.0f}s"
        for name, n, timeout in zip(top["name"], top["n"], top["timeout_s"], strict=True)
    }
    # boxplot draws the first category at the bottom; reverse so closest-to-timeout is at the top
    order = [label_for[str(name)] for name in reversed(top["name"].tolist())]
    merged["label"] = pd.Categorical(merged["name"].map(label_for), categories=order, ordered=True)
    merged.boxplot(column="ratio", by="label", ax=axes, vert=False, grid=True)
    axes.axvline(1.0, color="#d62828", linestyle="--", linewidth=1.2, label="maxRunTime")
    axes.set_xlabel("duration / maxRunTime")
    axes.set_ylabel("")
    axes.set_title(title)
    axes.legend(loc="lower right")
    figure.suptitle("")
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
    p95_count = int((~stats["insufficient_data"] & (stats["p95_ratio"] >= AT_RISK_RATIO)).sum())
    timeout_count = int((stats["timeout_failures"] > 0).sum())
    link = (
        f"<p>Full report: <a href='{html.escape(report_url)}'>{html.escape(report_url)}</a></p>" if report_url else ""
    )
    images = "".join(
        f"<h3>{html.escape(platform)}</h3><img src='{html.escape(filename)}' alt='{html.escape(platform)} boxplot'/>"
        for platform, filename in plots.items()
    )
    unresolved = []
    for platform in BUILD_TYPES:
        catalog_names = set(catalog.loc[catalog["platform"] == platform, "name"])
        known = teamcity_names.get(platform, set())
        unresolved.append(f"<h3>{html.escape(platform)}</h3>")
        unresolved.append(_names("In XML catalog, not in TeamCity", sorted(catalog_names - known)))
        unresolved.append(_names("In TeamCity, not in XML catalog", sorted(known - catalog_names)))
    return f"""<!DOCTYPE html><html lang='en'><head><meta charset='utf-8'/>
<title>TestBench duration vs maxRunTime</title>
{_style()}
</head><body>
<h1>TestBench duration vs maxRunTime</h1>
{link}
<p class='caveat'>{html.escape(DURATION_CAVEAT)}</p>
<ul>
<li>Cases: {len(stats)}</li>
<li>At risk: {int(stats["at_risk"].sum())}</li>
<li>p95 ≥ 80% of maxRunTime: {p95_count}</li>
<li>With program-timeout failures: {timeout_count}</li>
<li>Insufficient data (n &lt; 5): {int(stats["insufficient_data"].sum())}</li>
</ul>
<h2>Closest to timeout</h2>
{images}
<h2>At risk</h2>
{_table(stats.loc[stats["at_risk"]], "No at-risk cases.")}
<h2>All cases</h2>
{_table(stats, "No cases.")}
<h2>Unresolved names</h2>
{"".join(unresolved)}
</body></html>
"""


def _email_html(stats: pd.DataFrame, report_url: str, top_n: int = 20) -> str:
    at_risk = stats.loc[stats["at_risk"]]
    p95_count = int((~stats["insufficient_data"] & (stats["p95_ratio"] >= AT_RISK_RATIO)).sum())
    timeout_count = int((stats["timeout_failures"] > 0).sum())
    link = (
        f"<p><a href='{html.escape(report_url)}'>Open the full report</a></p>"
        if report_url
        else "<p>Full report is attached to the TeamCity build artifacts.</p>"
    )
    summary = (
        f"{p95_count} cases with p95 ≥ 80% of maxRunTime. "
        f"{timeout_count} cases with recent program-timeout failures. "
        f"{int(at_risk.shape[0])} cases at risk in total."
    )
    return f"""<html><body>
<h2>TestBench duration vs maxRunTime</h2>
<p>{summary}</p>
{link}
<p>Top cases closest to timeout:</p>
{_table(at_risk.head(top_n), "No at-risk cases.")}
<p class='caveat'>{html.escape(DURATION_CAVEAT)}</p>
</body></html>
"""


def _table(frame: pd.DataFrame, empty_message: str) -> str:
    if frame.empty:
        return f"<p>{html.escape(empty_message)}</p>"
    view = frame.loc[:, TABLE_COLS].copy()
    view["timeout_s"] = view["timeout_s"].round(0).astype(int)
    view["p95_s"] = view["p95_s"].round(1)
    view["p95_ratio"] = view["p95_ratio"].round(2)
    view["max_ratio"] = view["max_ratio"].round(2)
    return view.to_html(index=False, escape=True, border=0)


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
        "th{background:#f3f3f3}"
        "img{max-width:100%;height:auto}.caveat{max-width:70rem}</style>"
    )
