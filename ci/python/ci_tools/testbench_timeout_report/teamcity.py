"""Fetch TestBench durations from the TeamCity REST API as DataFrames."""

from __future__ import annotations

import logging
import time
from concurrent.futures import ThreadPoolExecutor, as_completed
from typing import Any

import pandas as pd
from httpx import HTTPStatusError

from ci_tools.teamcity.client import TeamcityClient

LOGGER = logging.getLogger(__name__)

BUILD_TYPES = {"linux": "Delft3D_LinuxTest", "windows": "Delft3D_WindowsTest"}
TIMEOUT_MARKER = "exceeded its max run time"
RETRY_STATUSES = {429, 500, 502, 503, 504}
SUCCESS_FIELDS = "testOccurrence(name,status,duration,details,ignored,muted)"


def list_test_names(client: TeamcityClient, build_type_id: str, recent_builds: int = 20) -> set[str]:
    """Return unique test names from recent default-branch builds."""
    items = _pages(
        client,
        "/app/rest/testOccurrences",
        "testOccurrence",
        {
            "locator": (
                f"build:(buildType:(id:{build_type_id}),branch:default:true,"
                f"state:finished,count:{recent_builds}),count:10000"
            ),
            "fields": "testOccurrence(name)",
        },
    )
    return {str(item["name"]) for item in items if item.get("name")}


def fetch_runs(
    client: TeamcityClient,
    catalog: pd.DataFrame,
    last_n: int,
    max_workers: int = 8,
) -> tuple[pd.DataFrame, pd.DataFrame]:
    """Fetch last-N successful durations and timeout-failure counts.

    Returns
    -------
    tuple[pd.DataFrame, pd.DataFrame]
        ``runs`` columns: platform, name, duration_s.
        ``failures`` columns: platform, name, timeout_failures.
    """
    cases = list(catalog[["platform", "name"]].itertuples(index=False, name=None))
    run_rows: list[dict[str, object]] = []
    failure_rows: list[dict[str, object]] = []
    LOGGER.info("Fetching TeamCity history for %s cases", len(cases))
    with ThreadPoolExecutor(max_workers=max_workers) as pool:
        futures = {
            pool.submit(_fetch_one, client, platform, name, last_n): (platform, name) for platform, name in cases
        }
        done = 0
        for future in as_completed(futures):
            platform, name = futures[future]
            done += 1
            try:
                durations, n_timeout = future.result()
            except Exception:
                LOGGER.exception("Failed to fetch history for %s/%s", platform, name)
                durations, n_timeout = [], 0
            run_rows.extend({"platform": platform, "name": name, "duration_s": d} for d in durations)
            failure_rows.append({"platform": platform, "name": name, "timeout_failures": n_timeout})
            if done % 100 == 0 or done == len(cases):
                LOGGER.info("Fetched %s/%s case histories", done, len(cases))

    runs = pd.DataFrame(run_rows, columns=["platform", "name", "duration_s"])
    failures = pd.DataFrame(failure_rows, columns=["platform", "name", "timeout_failures"])
    return runs, failures


def _fetch_one(client: TeamcityClient, platform: str, name: str, last_n: int) -> tuple[list[float], int]:
    build_type = BUILD_TYPES[platform]
    successes = _occurrences(client, build_type, name, "SUCCESS", last_n)
    durations = [
        int(item.get("duration") or 0) / 1000.0
        for item in successes
        if not item.get("ignored") and not item.get("muted")
    ][:last_n]
    failures = _occurrences(client, build_type, name, "FAILURE", 20)
    n_timeout = sum(1 for item in failures if TIMEOUT_MARKER in str(item.get("details") or ""))
    return durations, n_timeout


def _occurrences(
    client: TeamcityClient, build_type_id: str, test_name: str, status: str, count: int
) -> list[dict[str, Any]]:
    escaped = test_name.replace("!", "!!").replace(")", "!)")
    locator = (
        f"test:(name:({escaped})),build:(buildType:(id:{build_type_id}),branch:default:true),"
        f"status:{status},count:{count}"
    )
    return _pages(
        client,
        "/app/rest/testOccurrences",
        "testOccurrence",
        {"locator": locator, "fields": SUCCESS_FIELDS},
        max_items=count,
    )


def _pages(
    client: TeamcityClient,
    url: str,
    item_key: str,
    params: dict[str, str] | None = None,
    max_items: int | None = None,
) -> list[Any]:
    items: list[Any] = []
    next_url: str | None = url
    next_params = params
    while next_url:
        data = _get_json(client, next_url, next_params)
        items.extend(data.get(item_key, []))
        if max_items is not None and len(items) >= max_items:
            return items[:max_items]
        next_href = data.get("nextHref")
        if not next_href:
            break
        next_url = str(next_href)
        next_params = None
    return items


def _get_json(client: TeamcityClient, url: str, params: dict[str, str] | None) -> dict[str, Any]:
    last_error: Exception | None = None
    for attempt in range(4):
        response = client.call_teamcity_api(url, params=params)
        if response.status_code in RETRY_STATUSES:
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
