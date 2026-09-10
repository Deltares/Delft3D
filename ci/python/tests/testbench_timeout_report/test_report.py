from pathlib import Path

import pandas as pd
import pytest

from ci_tools.testbench_timeout_report.report import summarize, write_artifacts


def test_summarize_sorts_by_p95_ratio_and_flags_risk() -> None:
    catalog = pd.DataFrame(
        {
            "platform": ["linux", "linux", "linux"],
            "name": ["loose", "tight", "sparse"],
            "timeout_s": [300.0, 100.0, 300.0],
            "timeout_conflict": [False, False, False],
            "config_files": ["a.xml", "b.xml", "c.xml"],
        }
    )
    runs = pd.DataFrame(
        {
            "platform": ["linux"] * 21,
            "name": ["loose"] * 10 + ["tight"] * 10 + ["sparse"],
            "duration_s": [20.0] * 10 + [90.0] * 10 + [10.0],
        }
    )
    failures = pd.DataFrame(
        {
            "platform": ["linux", "linux", "linux"],
            "name": ["loose", "tight", "sparse"],
            "timeout_failures": [0, 2, 0],
        }
    )
    stats = summarize(catalog, runs, failures)
    assert stats["name"].tolist() == ["tight", "loose", "sparse"]
    tight = stats.iloc[0]
    assert bool(tight["at_risk"]) is True
    assert tight["p95_ratio"] == pytest.approx(0.9)
    assert tight["timeout_failures"] == 2
    sparse = stats.loc[stats["name"] == "sparse"].iloc[0]
    assert bool(sparse["insufficient_data"]) is True
    assert bool(sparse["at_risk"]) is False


def test_summarize_missing_history_is_insufficient() -> None:
    catalog = pd.DataFrame(
        {
            "platform": ["linux"],
            "name": ["absent"],
            "timeout_s": [300.0],
            "timeout_conflict": [False],
            "config_files": ["a.xml"],
        }
    )
    stats = summarize(catalog, pd.DataFrame(columns=["platform", "name", "duration_s"]), pd.DataFrame())
    assert int(stats.iloc[0]["n"]) == 0
    assert bool(stats.iloc[0]["insufficient_data"]) is True
    assert bool(stats.iloc[0]["at_risk"]) is False


def test_write_artifacts(tmp_path: Path) -> None:
    catalog = pd.DataFrame(
        {
            "platform": ["linux", "linux"],
            "name": ["tight", "loose"],
            "timeout_s": [100.0, 100.0],
            "timeout_conflict": [False, False],
            "config_files": ["a.xml", "b.xml"],
        }
    )
    runs = pd.DataFrame(
        {
            "platform": ["linux"] * 20,
            "name": ["tight"] * 10 + ["loose"] * 10,
            "duration_s": [95.0] * 10 + [10.0] * 10,
        }
    )
    failures = pd.DataFrame({"platform": ["linux", "linux"], "name": ["tight", "loose"], "timeout_failures": [1, 0]})
    stats = summarize(catalog, runs, failures)
    write_artifacts(
        stats=stats,
        runs=runs,
        catalog=catalog,
        teamcity_names={"linux": {"tight", "orphan"}, "windows": set()},
        output_dir=tmp_path,
        top_n=40,
        report_url="https://example.test/build/1",
        full_report_url="https://example.test/artifacts/timeout-report/report.html",
    )
    email = (tmp_path / "email.html").read_text(encoding="utf-8")
    html = (tmp_path / "report.html").read_text(encoding="utf-8")
    assert (tmp_path / "linux.png").is_file()
    assert (tmp_path / "windows.png").is_file()
    assert "tight" in (tmp_path / "cases.csv").read_text(encoding="utf-8")
    assert "tight" in html
    assert "orphan" in html
    assert "testStarted to testFinished" in html
    assert "Open the full report" in email
    assert "https://example.test/artifacts/timeout-report/report.html" in email
