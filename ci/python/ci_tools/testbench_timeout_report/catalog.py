"""Build a catalog of TestBench case names and their XML maxRunTime values."""

from __future__ import annotations

import csv
import logging
from dataclasses import dataclass
from pathlib import Path

from lxml import etree

LOGGER = logging.getLogger(__name__)

DEFAULT_MAX_RUNTIME_SECONDS = 300.0
ALL_TESTBENCH_COLUMN = "all-testbench"
CONFIG_COLUMN = "#config"
LINUX_MARKER = "lnx64"
WINDOWS_MARKER = "win64"

Platform = str


@dataclass(frozen=True)
class CaseTimeout:
    """Timeout configured for one TestBench case on one platform.

    Parameters
    ----------
    platform : str
        ``linux`` or ``windows``.
    name : str
        TestBench case name, which is also the TeamCity test name.
    timeout_s : float
        Effective ``maxRunTime`` in seconds. When XMLs disagree, this is the
        minimum of the observed values.
    config_files : tuple[str, ...]
        Relative XML config paths that define this case.
    timeout_conflict : bool
        True when the same case name has more than one distinct timeout.
    """

    platform: Platform
    name: str
    timeout_s: float
    config_files: tuple[str, ...]
    timeout_conflict: bool


def load_case_timeouts(csv_path: Path, configs_root: Path) -> list[CaseTimeout]:
    """Load timeouts for active TestBench configs listed in the CSV.

    Parameters
    ----------
    csv_path : Path
        Path to ``dimr_testbench_table.csv``.
    configs_root : Path
        Directory that contains TestBench XML configs.

    Returns
    -------
    list[CaseTimeout]
        One entry per unique ``(platform, name)``.
    """
    active_configs = _active_config_files(csv_path)
    collected: dict[tuple[str, str], list[tuple[float, str]]] = {}

    for relative_config, platform in active_configs:
        xml_path = configs_root / relative_config
        if not xml_path.is_file():
            LOGGER.warning("Skipping missing TestBench config: %s", xml_path)
            continue
        try:
            cases = parse_config_timeouts(xml_path)
        except etree.XMLSyntaxError:
            LOGGER.exception("Failed to parse TestBench config: %s", xml_path)
            raise
        for name, timeout_s in cases:
            collected.setdefault((platform, name), []).append((timeout_s, relative_config))

    return [_merge_case_timeouts(key, values) for key, values in sorted(collected.items())]


def parse_config_timeouts(xml_path: Path) -> list[tuple[str, float]]:
    """Parse runnable test cases and their maxRunTime from one XML config.

    ``defaultTestCases`` templates are ignored. Cases with ``ignore="true"``
    are skipped. A missing ``maxRunTime`` becomes 300 seconds, matching
    TestBench's ``XmlConfigParser``.

    Parameters
    ----------
    xml_path : Path
        TestBench XML config. ``xi:include`` is resolved.

    Returns
    -------
    list[tuple[str, float]]
        ``(case_name, timeout_seconds)`` pairs.
    """
    parser = etree.XMLParser(remove_blank_text=True, huge_tree=True)
    tree = etree.parse(str(xml_path), parser)
    tree.xinclude()
    root = tree.getroot()

    cases: list[tuple[str, float]] = []
    for test_cases in root.iter(_clark("testCases")):
        for child in test_cases:
            if _local_name(child) != "testCase":
                continue
            name = child.get("name")
            if not name:
                continue
            if (child.get("ignore") or "").lower() == "true":
                continue
            cases.append((name, _case_timeout(child)))
    return cases


def _active_config_files(csv_path: Path) -> list[tuple[str, Platform]]:
    """Return ``(relative_xml_path, platform)`` for all-testbench configs."""
    with csv_path.open(newline="", encoding="utf-8") as handle:
        reader = csv.DictReader(handle)
        if reader.fieldnames is None:
            raise ValueError(f"CSV has no header: {csv_path}")
        if CONFIG_COLUMN not in reader.fieldnames or ALL_TESTBENCH_COLUMN not in reader.fieldnames:
            raise ValueError(f"CSV {csv_path} is missing '{CONFIG_COLUMN}' or '{ALL_TESTBENCH_COLUMN}' columns.")

        result: list[tuple[str, Platform]] = []
        for row in reader:
            if row.get(ALL_TESTBENCH_COLUMN) != "TRUE":
                continue
            relative_config = (row.get(CONFIG_COLUMN) or "").strip()
            if not relative_config:
                continue
            platform = _platform_from_config(relative_config)
            if platform is None:
                LOGGER.warning("Skipping config with unknown platform: %s", relative_config)
                continue
            result.append((relative_config, platform))
        return result


def _platform_from_config(relative_config: str) -> Platform | None:
    """Map a config path to linux/windows using lnx64/win64 markers."""
    lowered = relative_config.lower()
    has_linux = LINUX_MARKER in lowered
    has_windows = WINDOWS_MARKER in lowered
    if has_linux and not has_windows:
        return "linux"
    if has_windows and not has_linux:
        return "windows"
    return None


def _case_timeout(test_case: etree._Element) -> float:
    """Return maxRunTime for a testCase element, defaulting to 300 seconds."""
    for child in test_case:
        if _local_name(child) == "maxRunTime" and child.text:
            return float(child.text.strip())
    return DEFAULT_MAX_RUNTIME_SECONDS


def _merge_case_timeouts(
    key: tuple[str, str],
    values: list[tuple[float, str]],
) -> CaseTimeout:
    """Collapse duplicate case names, keeping the tightest timeout."""
    platform, name = key
    timeouts = {timeout_s for timeout_s, _ in values}
    config_files = tuple(sorted({config for _, config in values}))
    timeout_conflict = len(timeouts) > 1
    timeout_s = min(timeouts)
    if timeout_conflict:
        LOGGER.warning(
            "Conflicting maxRunTime for %s/%s: %s; using minimum %.1f",
            platform,
            name,
            sorted(timeouts),
            timeout_s,
        )
    return CaseTimeout(
        platform=platform,
        name=name,
        timeout_s=timeout_s,
        config_files=config_files,
        timeout_conflict=timeout_conflict,
    )


def _clark(local_name: str) -> str:
    """Match a namespaced or un-namespaced tag by local name."""
    return f"{{*}}{local_name}"


def _local_name(element: etree._Element) -> str:
    """Return the un-namespaced tag name, or empty for comments and PIs."""
    tag = element.tag
    if not isinstance(tag, str):
        return ""
    return etree.QName(element).localname
