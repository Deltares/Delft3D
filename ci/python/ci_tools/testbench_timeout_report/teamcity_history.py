"""Fetch TestBench case duration history from the TeamCity REST API."""

from __future__ import annotations

import logging
import time
from concurrent.futures import ThreadPoolExecutor, as_completed
from dataclasses import dataclass
from typing import Any, ClassVar

from httpx import HTTPStatusError

from ci_tools.teamcity.client import TeamcityClient

LOGGER = logging.getLogger(__name__)

LINUX_BUILD_TYPE = "Delft3D_LinuxTest"
WINDOWS_BUILD_TYPE = "Delft3D_WindowsTest"
PLATFORM_BUILD_TYPES: dict[str, str] = {
    "linux": LINUX_BUILD_TYPE,
    "windows": WINDOWS_BUILD_TYPE,
}
TIMEOUT_DETAIL_MARKER = "exceeded its max run time"
RETRY_STATUS_CODES = {429, 500, 502, 503, 504}
MAX_ATTEMPTS = 4
OCCURRENCE_FIELDS = (
    "testOccurrence(id,name,status,duration,details,ignored,muted,build(id,number,finishDate,branchName))"
)
RECENT_BUILD_COUNT = 20


@dataclass(frozen=True)
class TestOccurrence:
    """One TeamCity test occurrence.

    Parameters
    ----------
    name : str
        TeamCity test name.
    status : str
        Occurrence status (``SUCCESS``, ``FAILURE``, ...).
    duration_ms : int
        Duration in milliseconds.
    details : str
        Failure details, empty for successes.
    ignored : bool
        Whether the occurrence was ignored.
    muted : bool
        Whether the occurrence was muted.
    build_id : str | None
        TeamCity build id.
    finish_date : str | None
        Build finish date string from TeamCity.
    """

    __test__: ClassVar[bool] = False
    name: str
    status: str
    duration_ms: int
    details: str
    ignored: bool
    muted: bool
    build_id: str | None
    finish_date: str | None

    @property
    def is_program_timeout(self) -> bool:
        """True when TestBench killed the program for exceeding maxRunTime."""
        return TIMEOUT_DETAIL_MARKER in self.details


@dataclass(frozen=True)
class CaseHistory:
    """Successful durations and timeout-failure count for one case.

    Parameters
    ----------
    platform : str
        ``linux`` or ``windows``.
    name : str
        TestBench / TeamCity case name.
    successes : tuple[TestOccurrence, ...]
        Successful, non-ignored, non-muted occurrences, newest first, length ≤ N.
    timeout_failures : int
        Recent FAILURE occurrences whose details match a program timeout.
    """

    platform: str
    name: str
    successes: tuple[TestOccurrence, ...]
    timeout_failures: int


def list_test_names(client: TeamcityClient, build_type_id: str, recent_builds: int = RECENT_BUILD_COUNT) -> set[str]:
    """List test names seen in recent finished default-branch builds.

    TeamCity 2026.2 does not support ``/app/rest/tests?locator=buildType:...``.
    Names are harvested from ``testOccurrences`` of recent builds instead.

    Parameters
    ----------
    client : TeamcityClient
        Authenticated TeamCity client.
    build_type_id : str
        TeamCity build type id.
    recent_builds : int, optional
        Number of recent finished builds to scan. Defaults to 20.

    Returns
    -------
    set[str]
        Test names.
    """
    items = _get_with_retry_pages(
        client,
        "/app/rest/testOccurrences",
        "testOccurrence",
        params={
            "locator": (
                f"build:(buildType:(id:{build_type_id}),branch:default:true,"
                f"state:finished,count:{recent_builds}),count:10000"
            ),
            "fields": "testOccurrence(name)",
        },
    )
    return {str(item["name"]) for item in items if item.get("name")}


def fetch_case_histories(
    client: TeamcityClient,
    cases: list[tuple[str, str]],
    last_n: int,
    max_workers: int = 8,
    failure_count: int = 20,
) -> list[CaseHistory]:
    """Fetch last-N successes and recent timeout failures for many cases.

    Parameters
    ----------
    client : TeamcityClient
        Authenticated TeamCity client.
    cases : list[tuple[str, str]]
        ``(platform, name)`` pairs.
    last_n : int
        Number of successful occurrences to keep per case.
    max_workers : int, optional
        Concurrent HTTP requests. Defaults to 8.
    failure_count : int, optional
        Number of recent failures to inspect per case. Defaults to 20.

    Returns
    -------
    list[CaseHistory]
        History for each requested case, including empty histories.
    """
    histories: list[CaseHistory] = []
    completed = 0
    LOGGER.info("Fetching TeamCity history for %s cases", len(cases))
    with ThreadPoolExecutor(max_workers=max_workers) as pool:
        futures = {
            pool.submit(
                fetch_one_case_history,
                client,
                platform,
                name,
                last_n,
                failure_count,
            ): (platform, name)
            for platform, name in cases
        }
        for future in as_completed(futures):
            platform, name = futures[future]
            completed += 1
            try:
                histories.append(future.result())
            except Exception:
                LOGGER.exception("Failed to fetch history for %s/%s", platform, name)
                histories.append(CaseHistory(platform=platform, name=name, successes=(), timeout_failures=0))
            if completed % 100 == 0 or completed == len(cases):
                LOGGER.info("Fetched %s/%s case histories", completed, len(cases))
    return histories


def fetch_one_case_history(
    client: TeamcityClient,
    platform: str,
    name: str,
    last_n: int,
    failure_count: int = 20,
) -> CaseHistory:
    """Fetch history for a single case.

    Parameters
    ----------
    client : TeamcityClient
        Authenticated TeamCity client.
    platform : str
        ``linux`` or ``windows``.
    name : str
        Test name.
    last_n : int
        Number of successful occurrences to keep.
    failure_count : int, optional
        Number of recent failures to inspect.

    Returns
    -------
    CaseHistory
        Successes and timeout-failure count.
    """
    build_type_id = PLATFORM_BUILD_TYPES[platform]
    successes = [
        occurrence
        for occurrence in _fetch_occurrences(client, build_type_id, name, "SUCCESS", last_n)
        if not occurrence.ignored and not occurrence.muted
    ]
    timeout_failures = sum(
        1
        for occurrence in _fetch_occurrences(client, build_type_id, name, "FAILURE", failure_count)
        if occurrence.is_program_timeout
    )
    return CaseHistory(
        platform=platform,
        name=name,
        successes=tuple(successes[:last_n]),
        timeout_failures=timeout_failures,
    )


def parse_occurrence(payload: dict[str, Any]) -> TestOccurrence:
    """Parse a TeamCity ``testOccurrence`` JSON object.

    Parameters
    ----------
    payload : dict[str, Any]
        Raw occurrence object.

    Returns
    -------
    TestOccurrence
        Parsed occurrence.
    """
    build = payload.get("build") or {}
    return TestOccurrence(
        name=str(payload.get("name", "")),
        status=str(payload.get("status", "")),
        duration_ms=int(payload.get("duration") or 0),
        details=str(payload.get("details") or ""),
        ignored=bool(payload.get("ignored", False)),
        muted=bool(payload.get("muted", False)),
        build_id=str(build["id"]) if build.get("id") is not None else None,
        finish_date=str(build["finishDate"]) if build.get("finishDate") is not None else None,
    )


def _fetch_occurrences(
    client: TeamcityClient,
    build_type_id: str,
    test_name: str,
    status: str,
    count: int,
) -> list[TestOccurrence]:
    locator = (
        f"test:(name:{_locator_value(test_name)}),"
        f"build:(buildType:(id:{build_type_id}),branch:default:true),"
        f"status:{status},count:{count}"
    )
    items = _get_with_retry_pages(
        client,
        "/app/rest/testOccurrences",
        "testOccurrence",
        params={"locator": locator, "fields": OCCURRENCE_FIELDS},
        max_items=count,
    )
    return [parse_occurrence(item) for item in items]


def _locator_value(value: str) -> str:
    """Wrap and escape a locator dimension value."""
    escaped = value.replace("!", "!!").replace(")", "!)")
    return f"({escaped})"


def _get_with_retry_pages(
    client: TeamcityClient,
    url: str,
    item_key: str,
    params: dict[str, str] | None = None,
    max_items: int | None = None,
) -> list[Any]:
    """GET a paginated collection, retrying transient HTTP errors.

    Parameters
    ----------
    max_items : int | None, optional
        Stop once this many items have been collected. ``None`` means follow
        ``nextHref`` until it is exhausted.
    """
    items: list[Any] = []
    next_url: str | None = url
    next_params = params
    while next_url:
        data = _get_json_with_retry(client, next_url, next_params)
        items.extend(data.get(item_key, []))
        if max_items is not None and len(items) >= max_items:
            return items[:max_items]
        next_href = data.get("nextHref")
        if not next_href:
            break
        next_url = str(next_href)
        next_params = None
    return items


def _get_json_with_retry(client: TeamcityClient, url: str, params: dict[str, str] | None) -> dict[str, Any]:
    last_error: Exception | None = None
    for attempt in range(MAX_ATTEMPTS):
        response = client.call_teamcity_api(url, params=params)
        if response.status_code in RETRY_STATUS_CODES:
            last_error = HTTPStatusError(
                f"TeamCity returned {response.status_code} for {url}",
                request=response.request,
                response=response,
            )
            LOGGER.warning("Retrying %s after status %s (attempt %s)", url, response.status_code, attempt + 1)
            time.sleep(2**attempt)
            continue
        response.raise_for_status()
        payload: dict[str, Any] = response.json()
        return payload
    assert last_error is not None
    raise last_error
