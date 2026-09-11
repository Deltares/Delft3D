"""Load TestBench case names and XML maxRunTime values into a DataFrame."""

from __future__ import annotations

import csv
import logging
from pathlib import Path

import pandas as pd
from lxml import etree

LOGGER = logging.getLogger(__name__)

DEFAULT_MAX_RUNTIME_SECONDS = 300.0


def load_catalog(csv_path: Path, configs_root: Path) -> pd.DataFrame:
    """Return one row per ``(platform, name)`` for active all-testbench configs.

    Missing ``maxRunTime`` is 300 seconds. Duplicate names keep the minimum
    timeout and set ``timeout_conflict``.
    """
    rows: list[dict[str, object]] = []
    with csv_path.open(newline="", encoding="utf-8") as handle:
        table = pd.DataFrame(list(csv.DictReader(handle)))
    active_flag = table["all-testbench"].astype(str).str.upper().eq("TRUE")
    for relative in table.loc[active_flag, "#config"].dropna().astype(str).str.strip():
        platform = _platform(relative)
        if platform is None:
            LOGGER.warning("Skipping config with unknown platform: %s", relative)
            continue
        xml_path = configs_root / relative
        if not xml_path.is_file():
            LOGGER.warning("Skipping missing TestBench config: %s", xml_path)
            continue
        for name, timeout_s in parse_config_timeouts(xml_path):
            rows.append({"platform": platform, "name": name, "timeout_s": timeout_s, "config_file": relative})

    if not rows:
        return pd.DataFrame(columns=["platform", "name", "timeout_s", "timeout_conflict", "config_files"])

    raw = pd.DataFrame(rows)
    grouped = (
        raw.groupby(["platform", "name"], as_index=False)
        .agg(
            timeout_s=("timeout_s", "min"),
            timeout_conflict=("timeout_s", lambda s: s.nunique() > 1),
            config_files=("config_file", lambda s: ";".join(sorted(set(s)))),
        )
        .sort_values(["platform", "name"])
        .reset_index(drop=True)
    )
    conflicts = grouped.loc[grouped["timeout_conflict"], ["platform", "name", "timeout_s"]]
    for platform, name, timeout_s in conflicts.itertuples(index=False, name=None):
        LOGGER.warning("Conflicting maxRunTime for %s/%s; using minimum %.1f", platform, name, timeout_s)
    return grouped


def parse_config_timeouts(xml_path: Path) -> list[tuple[str, float]]:
    """Return ``(name, timeout_s)`` for runnable testCase elements after xinclude."""
    tree = etree.parse(str(xml_path), etree.XMLParser(remove_blank_text=True, huge_tree=True))
    tree.xinclude()
    cases: list[tuple[str, float]] = []
    for block in tree.getroot().iter("{*}testCases"):
        for child in block:
            if not isinstance(child.tag, str) or etree.QName(child).localname != "testCase":
                continue
            name = child.get("name")
            if not name or (child.get("ignore") or "").lower() == "true":
                continue
            timeout = DEFAULT_MAX_RUNTIME_SECONDS
            for nested in child:
                if isinstance(nested.tag, str) and etree.QName(nested).localname == "maxRunTime" and nested.text:
                    timeout = float(nested.text.strip())
                    break
            cases.append((name, timeout))
    return cases


def _platform(relative_config: str) -> str | None:
    path = relative_config.lower()
    linux, windows = "lnx64" in path, "win64" in path
    if linux and not windows:
        return "linux"
    if windows and not linux:
        return "windows"
    return None
