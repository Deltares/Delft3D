import json
from unittest.mock import Mock

from httpx import Request, Response
from pytest_mock import MockerFixture

from ci_tools.teamcity.client import TeamcityClient
from ci_tools.testbench_timeout_report.teamcity_history import (
    TIMEOUT_DETAIL_MARKER,
    fetch_one_case_history,
    list_test_names,
    parse_occurrence,
)


def _client(mocker: MockerFixture, responses: list[Response]) -> tuple[TeamcityClient, Mock]:
    http_client = mocker.Mock()
    http_client.get.side_effect = responses
    return TeamcityClient(client=http_client, server="https://example.test"), http_client


def _json_response(payload: dict, status_code: int = 200) -> Response:
    return Response(
        status_code=status_code,
        content=json.dumps(payload).encode("utf-8"),
        request=Request("GET", "https://example.test/app/rest"),
        headers={"Content-Type": "application/json"},
    )


def test_parse_occurrence_detects_program_timeout() -> None:
    occurrence = parse_occurrence(
        {
            "name": "case_a",
            "status": "FAILURE",
            "duration": 300000,
            "details": f"Program dimr {TIMEOUT_DETAIL_MARKER} of 300s",
            "ignored": False,
            "muted": False,
            "build": {"id": 12, "finishDate": "20260101T000000+0000"},
        }
    )
    assert occurrence.is_program_timeout is True
    assert occurrence.duration_ms == 300000
    assert occurrence.build_id == "12"


def test_list_test_names_paginates(mocker: MockerFixture) -> None:
    client, http_client = _client(
        mocker,
        [
            _json_response(
                {
                    "testOccurrence": [{"name": "case_a"}],
                    "nextHref": "/app/rest/testOccurrences?locator=count:10000&start=1",
                }
            ),
            _json_response({"testOccurrence": [{"name": "case_b"}]}),
        ],
    )

    names = list_test_names(client, "Delft3D_LinuxTest")

    assert names == {"case_a", "case_b"}
    assert http_client.get.call_count == 2


def test_fetch_one_case_history_skips_muted_and_counts_timeouts(mocker: MockerFixture) -> None:
    client, _http_client = _client(
        mocker,
        [
            _json_response(
                {
                    "testOccurrence": [
                        {"name": "case_a", "status": "SUCCESS", "duration": 1000, "ignored": False, "muted": False},
                        {"name": "case_a", "status": "SUCCESS", "duration": 2000, "ignored": False, "muted": True},
                    ]
                }
            ),
            _json_response(
                {
                    "testOccurrence": [
                        {
                            "name": "case_a",
                            "status": "FAILURE",
                            "duration": 300000,
                            "details": TIMEOUT_DETAIL_MARKER,
                        },
                        {"name": "case_a", "status": "FAILURE", "duration": 10, "details": "comparison failed"},
                    ]
                }
            ),
        ],
    )

    history = fetch_one_case_history(client, "linux", "case_a", last_n=100)

    assert len(history.successes) == 1
    assert history.successes[0].duration_ms == 1000
    assert history.timeout_failures == 1


def test_retries_transient_errors(mocker: MockerFixture) -> None:
    mocker.patch("ci_tools.testbench_timeout_report.teamcity_history.time.sleep")
    client, http_client = _client(
        mocker,
        [
            _json_response({}, status_code=503),
            _json_response({"testOccurrence": [{"name": "case_a"}]}),
        ],
    )

    names = list_test_names(client, "Delft3D_LinuxTest")

    assert names == {"case_a"}
    assert http_client.get.call_count == 2
