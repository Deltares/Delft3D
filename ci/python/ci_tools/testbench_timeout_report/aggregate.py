"""Aggregate last-N durations against catalog timeouts."""

from __future__ import annotations

import math
from dataclasses import dataclass

from ci_tools.testbench_timeout_report.catalog import CaseTimeout
from ci_tools.testbench_timeout_report.teamcity_history import CaseHistory

AT_RISK_RATIO = 0.8
MIN_SAMPLES = 5


@dataclass(frozen=True)
class CaseStats:
    """Duration statistics for one case on one platform.

    Parameters
    ----------
    platform : str
        ``linux`` or ``windows``.
    name : str
        TestBench case name.
    timeout_s : float
        Catalog ``maxRunTime`` in seconds.
    n : int
        Number of successful samples.
    durations_s : tuple[float, ...]
        Successful durations in seconds, newest first.
    min_s : float
        Minimum successful duration in seconds. Zero when ``n`` is 0.
    q1_s : float
        First quartile duration in seconds.
    median_s : float
        Median duration in seconds.
    q3_s : float
        Third quartile duration in seconds.
    max_s : float
        Maximum successful duration in seconds.
    p95_s : float
        95th percentile duration in seconds.
    p95_ratio : float
        ``p95_s / timeout_s``.
    max_ratio : float
        ``max_s / timeout_s``.
    timeout_failures : int
        Recent program-timeout failures.
    timeout_conflict : bool
        Catalog reported conflicting XML timeouts.
    config_files : tuple[str, ...]
        XML configs that define the case.
    at_risk : bool
        ``p95_ratio >= 0.8`` or any timeout failure.
    insufficient_data : bool
        ``n < 5``.
    """

    platform: str
    name: str
    timeout_s: float
    n: int
    durations_s: tuple[float, ...]
    min_s: float
    q1_s: float
    median_s: float
    q3_s: float
    max_s: float
    p95_s: float
    p95_ratio: float
    max_ratio: float
    timeout_failures: int
    timeout_conflict: bool
    config_files: tuple[str, ...]
    at_risk: bool
    insufficient_data: bool


def aggregate_case_stats(catalog: list[CaseTimeout], histories: list[CaseHistory]) -> list[CaseStats]:
    """Join catalog timeouts with TeamCity histories.

    Parameters
    ----------
    catalog : list[CaseTimeout]
        Timeouts from current XML configs.
    histories : list[CaseHistory]
        TeamCity histories, possibly a subset of the catalog.

    Returns
    -------
    list[CaseStats]
        One row per catalog case, sorted by ``p95_ratio`` descending.
    """
    history_by_key = {(item.platform, item.name): item for item in histories}
    stats = [_stats_for_case(case, history_by_key.get((case.platform, case.name))) for case in catalog]
    return sorted(stats, key=lambda item: (item.p95_ratio, item.max_ratio, item.name), reverse=True)


def percentile(values: list[float], fraction: float) -> float:
    """Linear-interpolation percentile of an unsorted sample.

    Parameters
    ----------
    values : list[float]
        Sample values.
    fraction : float
        Percentile in ``[0, 1]``.

    Returns
    -------
    float
        Interpolated percentile. ``0.0`` when ``values`` is empty.
    """
    if not values:
        return 0.0
    ordered = sorted(values)
    if len(ordered) == 1:
        return ordered[0]
    rank = (len(ordered) - 1) * fraction
    lower = math.floor(rank)
    upper = math.ceil(rank)
    if lower == upper:
        return ordered[lower]
    weight = rank - lower
    return ordered[lower] * (1.0 - weight) + ordered[upper] * weight


def _stats_for_case(case: CaseTimeout, history: CaseHistory | None) -> CaseStats:
    successes = history.successes if history is not None else ()
    timeout_failures = history.timeout_failures if history is not None else 0
    durations_s = tuple(occurrence.duration_ms / 1000.0 for occurrence in successes)
    n = len(durations_s)
    min_s = min(durations_s) if durations_s else 0.0
    max_s = max(durations_s) if durations_s else 0.0
    q1_s = percentile(list(durations_s), 0.25)
    median_s = percentile(list(durations_s), 0.5)
    q3_s = percentile(list(durations_s), 0.75)
    p95_s = percentile(list(durations_s), 0.95)
    p95_ratio = p95_s / case.timeout_s if case.timeout_s else 0.0
    max_ratio = max_s / case.timeout_s if case.timeout_s else 0.0
    insufficient_data = n < MIN_SAMPLES
    at_risk = (not insufficient_data and p95_ratio >= AT_RISK_RATIO) or timeout_failures > 0
    return CaseStats(
        platform=case.platform,
        name=case.name,
        timeout_s=case.timeout_s,
        n=n,
        durations_s=durations_s,
        min_s=min_s,
        q1_s=q1_s,
        median_s=median_s,
        q3_s=q3_s,
        max_s=max_s,
        p95_s=p95_s,
        p95_ratio=p95_ratio,
        max_ratio=max_ratio,
        timeout_failures=timeout_failures,
        timeout_conflict=case.timeout_conflict,
        config_files=case.config_files,
        at_risk=at_risk,
        insufficient_data=insufficient_data,
    )
