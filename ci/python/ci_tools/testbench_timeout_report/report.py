"""Write HTML, CSV, and email artifacts for the timeout report."""

from __future__ import annotations

import csv
import html
from dataclasses import dataclass
from pathlib import Path

from ci_tools.testbench_timeout_report.aggregate import CaseStats

DURATION_CAVEAT = (
    "TeamCity duration is testStarted to testFinished, which includes prepare, "
    "program run, and comparison. XML maxRunTime only kills the program. Duration "
    "can therefore exceed maxRunTime without a program timeout. The ratio is still "
    "an upper bound for tuning: a p95 far below 1.0 means the timeout is loose; "
    "a p95 near 1.0 means it is tight."
)


@dataclass(frozen=True)
class UnresolvedNames:
    """Names that did not join cleanly between XML and TeamCity.

    Parameters
    ----------
    catalog_only : tuple[str, ...]
        Catalog cases with no TeamCity test of that name.
    teamcity_only : tuple[str, ...]
        TeamCity tests not present in the current XML catalog.
    """

    catalog_only: tuple[str, ...]
    teamcity_only: tuple[str, ...]


def write_csv(stats: list[CaseStats], path: Path) -> None:
    """Write one row per case to CSV.

    Parameters
    ----------
    stats : list[CaseStats]
        Aggregated statistics.
    path : Path
        Output CSV path.
    """
    path.parent.mkdir(parents=True, exist_ok=True)
    fieldnames = [
        "platform",
        "name",
        "n",
        "timeout_s",
        "min_s",
        "q1_s",
        "median_s",
        "q3_s",
        "max_s",
        "p95_s",
        "p95_ratio",
        "max_ratio",
        "timeout_failures",
        "at_risk",
        "insufficient_data",
        "timeout_conflict",
        "config_files",
    ]
    with path.open("w", encoding="utf-8", newline="") as handle:
        writer = csv.DictWriter(handle, fieldnames=fieldnames)
        writer.writeheader()
        for item in stats:
            writer.writerow(
                {
                    "platform": item.platform,
                    "name": item.name,
                    "n": item.n,
                    "timeout_s": f"{item.timeout_s:.1f}",
                    "min_s": f"{item.min_s:.3f}",
                    "q1_s": f"{item.q1_s:.3f}",
                    "median_s": f"{item.median_s:.3f}",
                    "q3_s": f"{item.q3_s:.3f}",
                    "max_s": f"{item.max_s:.3f}",
                    "p95_s": f"{item.p95_s:.3f}",
                    "p95_ratio": f"{item.p95_ratio:.3f}",
                    "max_ratio": f"{item.max_ratio:.3f}",
                    "timeout_failures": item.timeout_failures,
                    "at_risk": item.at_risk,
                    "insufficient_data": item.insufficient_data,
                    "timeout_conflict": item.timeout_conflict,
                    "config_files": ";".join(item.config_files),
                }
            )


def write_html_report(
    stats: list[CaseStats],
    unresolved: dict[str, UnresolvedNames],
    plot_names: dict[str, str],
    path: Path,
    report_url: str = "",
) -> None:
    """Write the full HTML report.

    Parameters
    ----------
    stats : list[CaseStats]
        Aggregated statistics.
    unresolved : dict[str, UnresolvedNames]
        Unresolved names per platform.
    plot_names : dict[str, str]
        Platform to plot filename (relative to the HTML file).
    path : Path
        Output HTML path.
    report_url : str, optional
        TeamCity build URL.
    """
    path.parent.mkdir(parents=True, exist_ok=True)
    at_risk = [item for item in stats if item.at_risk]
    sections = [
        "<!DOCTYPE html>",
        "<html lang='en'><head><meta charset='utf-8'/>",
        "<title>TestBench duration vs maxRunTime</title>",
        _stylesheet(),
        "</head><body>",
        "<h1>TestBench duration vs maxRunTime</h1>",
    ]
    if report_url:
        sections.append(f"<p>TeamCity build: <a href='{html.escape(report_url)}'>{html.escape(report_url)}</a></p>")
    sections.append(f"<p class='caveat'>{html.escape(DURATION_CAVEAT)}</p>")
    sections.append(_summary_block(stats, at_risk))
    sections.append("<h2>Closest to timeout</h2>")
    for platform, filename in plot_names.items():
        sections.append(f"<h3>{html.escape(platform)}</h3>")
        sections.append(f"<img src='{html.escape(filename)}' alt='{html.escape(platform)} boxplot'/>")
    sections.append("<h2>At risk</h2>")
    sections.append(_stats_table(at_risk if at_risk else [], empty_message="No at-risk cases."))
    sections.append("<h2>All cases</h2>")
    sections.append(_stats_table(stats, empty_message="No cases."))
    sections.append("<h2>Unresolved names</h2>")
    for platform, names in unresolved.items():
        sections.append(f"<h3>{html.escape(platform)}</h3>")
        sections.append(
            _name_list(
                "In XML catalog, not in TeamCity",
                names.catalog_only,
            )
        )
        sections.append(
            _name_list(
                "In TeamCity, not in XML catalog",
                names.teamcity_only,
            )
        )
    sections.append("</body></html>")
    path.write_text("\n".join(sections), encoding="utf-8")


def write_email_html(
    stats: list[CaseStats],
    path: Path,
    report_url: str = "",
    full_report_url: str = "",
    top_n: int = 20,
) -> None:
    """Write a short HTML email body.

    Parameters
    ----------
    stats : list[CaseStats]
        Aggregated statistics.
    path : Path
        Output HTML path.
    report_url : str, optional
        TeamCity build overview URL.
    full_report_url : str, optional
        Direct URL to the HTML report artifact.
    top_n : int, optional
        Number of at-risk rows to include. Defaults to 20.
    """
    at_risk = [item for item in stats if item.at_risk]
    p95_count = sum(1 for item in stats if not item.insufficient_data and item.p95_ratio >= 0.8)
    timeout_count = sum(1 for item in stats if item.timeout_failures > 0)
    rows = at_risk[:top_n]
    href = full_report_url or report_url
    if href:
        link = f"<p><a href='{html.escape(href)}'>Open the full report</a></p>"
    else:
        link = "<p>Full report is attached to the TeamCity build artifacts.</p>"
    body = [
        "<html><body>",
        "<h2>TestBench duration vs maxRunTime</h2>",
        (
            f"<p>{p95_count} cases with p95 ≥ 80% of maxRunTime. "
            f"{timeout_count} cases with recent program-timeout failures. "
            f"{len(at_risk)} cases at risk in total.</p>"
        ),
        link,
        "<p>Top cases closest to timeout:</p>",
        _stats_table(rows, empty_message="No at-risk cases."),
        f"<p class='caveat'>{html.escape(DURATION_CAVEAT)}</p>",
        "</body></html>",
    ]
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text("\n".join(body), encoding="utf-8")


def _summary_block(stats: list[CaseStats], at_risk: list[CaseStats]) -> str:
    p95_count = sum(1 for item in stats if not item.insufficient_data and item.p95_ratio >= 0.8)
    timeout_count = sum(1 for item in stats if item.timeout_failures > 0)
    insufficient = sum(1 for item in stats if item.insufficient_data)
    return (
        "<ul>"
        f"<li>Cases: {len(stats)}</li>"
        f"<li>At risk: {len(at_risk)}</li>"
        f"<li>p95 ≥ 80% of maxRunTime: {p95_count}</li>"
        f"<li>With program-timeout failures: {timeout_count}</li>"
        f"<li>Insufficient data (n &lt; 5): {insufficient}</li>"
        "</ul>"
    )


def _stats_table(stats: list[CaseStats], empty_message: str) -> str:
    if not stats:
        return f"<p>{html.escape(empty_message)}</p>"
    header = (
        "<table><thead><tr>"
        "<th>Platform</th><th>Case</th><th>n</th><th>maxRunTime (s)</th>"
        "<th>p95 (s)</th><th>p95 / timeout</th><th>max / timeout</th>"
        "<th>Timeout failures</th><th>Conflict</th>"
        "</tr></thead><tbody>"
    )
    rows = []
    for item in stats:
        risk_class = "at-risk" if item.at_risk else ""
        rows.append(
            "<tr class='"
            + risk_class
            + "'>"
            + "".join(
                _td(value)
                for value in (
                    item.platform,
                    item.name,
                    str(item.n),
                    f"{item.timeout_s:.0f}",
                    f"{item.p95_s:.1f}",
                    f"{item.p95_ratio:.2f}",
                    f"{item.max_ratio:.2f}",
                    str(item.timeout_failures),
                    "yes" if item.timeout_conflict else "",
                )
            )
            + "</tr>"
        )
    return header + "".join(rows) + "</tbody></table>"


def _name_list(title: str, names: tuple[str, ...], limit: int = 50) -> str:
    shown = names[:limit]
    extra = len(names) - len(shown)
    items = "".join(f"<li>{html.escape(name)}</li>" for name in shown)
    suffix = f"<p>{extra} more omitted.</p>" if extra > 0 else ""
    if not names:
        items = "<li>None</li>"
    return f"<h4>{html.escape(title)} ({len(names)})</h4><ul>{items}</ul>{suffix}"


def _td(value: str) -> str:
    return f"<td>{html.escape(value)}</td>"


def _stylesheet() -> str:
    return """
<style>
body { font-family: sans-serif; margin: 1.5rem; }
table { border-collapse: collapse; width: 100%; font-size: 0.9rem; }
th, td { border: 1px solid #ccc; padding: 0.3rem 0.5rem; text-align: left; }
th { background: #f3f3f3; }
tr.at-risk { background: #ffe3e0; }
img { max-width: 100%; height: auto; }
.caveat { max-width: 70rem; }
</style>
""".strip()
