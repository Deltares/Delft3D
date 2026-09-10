from pathlib import Path

from ci_tools.testbench_timeout_report.aggregate import CaseStats
from ci_tools.testbench_timeout_report.catalog import CaseTimeout
from ci_tools.testbench_timeout_report.cli import generate_report, unresolved_names
from ci_tools.testbench_timeout_report.report import UnresolvedNames, write_email_html


def _stats(name: str, p95_ratio: float, n: int = 10, timeout_failures: int = 0) -> CaseStats:
    timeout_s = 100.0
    durations = tuple(timeout_s * p95_ratio for _ in range(n))
    insufficient = n < 5
    at_risk = (not insufficient and p95_ratio >= 0.8) or timeout_failures > 0
    return CaseStats(
        platform="linux",
        name=name,
        timeout_s=timeout_s,
        n=n,
        durations_s=durations,
        min_s=durations[0] if durations else 0.0,
        q1_s=durations[0] if durations else 0.0,
        median_s=durations[0] if durations else 0.0,
        q3_s=durations[0] if durations else 0.0,
        max_s=durations[0] if durations else 0.0,
        p95_s=timeout_s * p95_ratio,
        p95_ratio=p95_ratio,
        max_ratio=p95_ratio,
        timeout_failures=timeout_failures,
        timeout_conflict=False,
        config_files=("dimr/sample.xml",),
        at_risk=at_risk,
        insufficient_data=insufficient,
    )


def test_generate_report_writes_artifacts(tmp_path: Path) -> None:
    stats = [_stats("tight", 0.95, timeout_failures=1), _stats("loose", 0.1)]
    unresolved = {
        "linux": UnresolvedNames(catalog_only=("missing_in_tc",), teamcity_only=("orphan",)),
        "windows": UnresolvedNames(catalog_only=(), teamcity_only=()),
    }

    generate_report(
        stats=stats,
        unresolved=unresolved,
        output_dir=tmp_path,
        top_n=40,
        report_url="https://example.test/build/1",
        full_report_url="https://example.test/artifacts/timeout-report/report.html",
    )

    assert (tmp_path / "linux.png").is_file()
    assert (tmp_path / "windows.png").is_file()
    csv_text = (tmp_path / "cases.csv").read_text(encoding="utf-8")
    html_text = (tmp_path / "report.html").read_text(encoding="utf-8")
    email_text = (tmp_path / "email.html").read_text(encoding="utf-8")
    assert "tight" in csv_text
    assert "tight" in html_text
    assert "missing_in_tc" in html_text
    assert "testStarted to testFinished" in html_text
    assert "https://example.test/build/1" in html_text
    assert "Open the full report" in email_text
    assert "https://example.test/artifacts/timeout-report/report.html" in email_text
    assert "tight" in email_text


def test_unresolved_names_splits_catalog_and_teamcity() -> None:
    catalog = [
        CaseTimeout("linux", "kept", 300.0, ("a.xml",), False),
        CaseTimeout("linux", "only_xml", 300.0, ("a.xml",), False),
    ]
    result = unresolved_names(catalog, {"linux": {"kept", "only_tc"}, "windows": set()})
    assert result["linux"].catalog_only == ("only_xml",)
    assert result["linux"].teamcity_only == ("only_tc",)


def test_write_email_handles_no_at_risk(tmp_path: Path) -> None:
    path = tmp_path / "email.html"
    write_email_html([_stats("loose", 0.1)], path)
    assert "No at-risk cases." in path.read_text(encoding="utf-8")
    assert "Full report is attached to the TeamCity build artifacts." in path.read_text(encoding="utf-8")
