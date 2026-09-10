from ci_tools.testbench_timeout_report.aggregate import aggregate_case_stats, percentile
from ci_tools.testbench_timeout_report.catalog import CaseTimeout
from ci_tools.testbench_timeout_report.teamcity_history import CaseHistory, TestOccurrence


def _timeout(name: str, timeout_s: float, platform: str = "linux") -> CaseTimeout:
    return CaseTimeout(
        platform=platform,
        name=name,
        timeout_s=timeout_s,
        config_files=(f"dimr/{name}.xml",),
        timeout_conflict=False,
    )


def _success(duration_ms: int) -> TestOccurrence:
    return TestOccurrence(
        name="case",
        status="SUCCESS",
        duration_ms=duration_ms,
        details="",
        ignored=False,
        muted=False,
        build_id="1",
        finish_date="20260101T000000+0000",
    )


def test_percentile_interpolates() -> None:
    assert percentile([10.0, 20.0, 30.0, 40.0], 0.5) == 25.0
    assert percentile([10.0], 0.95) == 10.0
    assert percentile([], 0.95) == 0.0


def test_aggregate_sorts_by_p95_ratio_and_flags_risk() -> None:
    catalog = [_timeout("loose", 300.0), _timeout("tight", 100.0), _timeout("sparse", 300.0)]
    histories = [
        CaseHistory(
            platform="linux",
            name="loose",
            successes=tuple(_success(20_000) for _ in range(10)),
            timeout_failures=0,
        ),
        CaseHistory(
            platform="linux",
            name="tight",
            successes=tuple(_success(90_000) for _ in range(10)),
            timeout_failures=2,
        ),
        CaseHistory(
            platform="linux",
            name="sparse",
            successes=(_success(10_000),),
            timeout_failures=0,
        ),
    ]

    stats = aggregate_case_stats(catalog, histories)

    assert [item.name for item in stats] == ["tight", "loose", "sparse"]
    tight = stats[0]
    assert tight.at_risk is True
    assert tight.p95_ratio == 0.9
    assert tight.timeout_failures == 2
    sparse = next(item for item in stats if item.name == "sparse")
    assert sparse.insufficient_data is True
    assert sparse.at_risk is False
    loose = next(item for item in stats if item.name == "loose")
    assert loose.at_risk is False


def test_missing_history_is_insufficient() -> None:
    stats = aggregate_case_stats([_timeout("absent", 300.0)], [])
    assert stats[0].n == 0
    assert stats[0].insufficient_data is True
    assert stats[0].at_risk is False
