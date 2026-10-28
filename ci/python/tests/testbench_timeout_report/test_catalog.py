from pathlib import Path

import pytest

from ci_tools.testbench_timeout_report.catalog import (
    DEFAULT_MAX_RUNTIME_SECONDS,
    load_catalog,
    parse_config_timeouts,
)

FIXTURES = Path(__file__).parent / "fixtures"
CONFIGS_ROOT = FIXTURES / "configs"
CSV_PATH = FIXTURES / "dimr_testbench_table.csv"


def test_parse_config_uses_explicit_timeout_and_default() -> None:
    cases = dict(parse_config_timeouts(CONFIGS_ROOT / "dimr" / "sample_lnx64.xml"))
    assert cases["explicit_timeout"] == 480.0
    assert cases["default_timeout"] == DEFAULT_MAX_RUNTIME_SECONDS
    assert "dflowfm_default" not in cases
    assert "ignored_case" not in cases


def test_parse_config_resolves_xinclude() -> None:
    cases = dict(parse_config_timeouts(CONFIGS_ROOT / "dimr" / "sample_lnx64.xml"))
    assert cases["included_timeout"] == 900.0
    assert cases["included_default"] == DEFAULT_MAX_RUNTIME_SECONDS


def test_load_catalog_skips_inactive_and_missing(caplog: pytest.LogCaptureFixture) -> None:
    catalog = load_catalog(CSV_PATH, CONFIGS_ROOT)
    names = set(zip(catalog["platform"], catalog["name"], strict=False))
    assert ("linux", "inactive_case") not in names
    assert ("linux", "explicit_timeout") in names
    assert ("windows", "windows_only") in names
    assert "does_not_exist_lnx64.xml" in caplog.text


def test_conflicting_timeouts_use_minimum() -> None:
    catalog = load_catalog(CSV_PATH, CONFIGS_ROOT)
    conflict = catalog[(catalog["platform"] == "linux") & (catalog["name"] == "conflict_case")].iloc[0]
    assert conflict["timeout_s"] == 120.0
    assert bool(conflict["timeout_conflict"]) is True
    assert conflict["config_files"] == "dimr/sample_lnx64.xml;dimr/sample_lnx64_conflict.xml"


def test_platforms_are_split() -> None:
    catalog = load_catalog(CSV_PATH, CONFIGS_ROOT)
    linux = catalog.loc[catalog["platform"] == "linux"].set_index("name")["timeout_s"]
    windows = catalog.loc[catalog["platform"] == "windows"].set_index("name")["timeout_s"]
    assert "windows_only" not in linux.index
    assert windows["windows_only"] == 1500.0
    assert windows["explicit_timeout"] == 480.0
