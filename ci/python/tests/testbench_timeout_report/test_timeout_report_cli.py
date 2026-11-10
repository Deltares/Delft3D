import os
from pathlib import Path

import pandas as pd
import pytest
from pytest_mock import MockerFixture

from ci_tools.teamcity.client import TeamcityClient
from ci_tools.testbench_timeout_report.cli import build_client, create_parser, limit_catalog, run


def test_build_client_prefers_token(mocker: MockerFixture) -> None:
    factory = mocker.patch.object(TeamcityClient, "with_bearer_token_auth")
    arguments = create_parser().parse_args(["--server", "https://example.test", "--token", "abc"])
    build_client(arguments)
    factory.assert_called_once()


def test_build_client_uses_env_password(mocker: MockerFixture, monkeypatch: pytest.MonkeyPatch) -> None:
    monkeypatch.setenv("TEAMCITY_USERNAME", "user")
    monkeypatch.setenv("TEAMCITY_PASSWORD", "secret")
    factory = mocker.patch.object(TeamcityClient, "with_basic_auth")
    arguments = create_parser().parse_args(["--server", "dpcbuild.example"])
    build_client(arguments)
    factory.assert_called_once()
    assert os.environ["TEAMCITY_PASSWORD"] == "secret"


def test_build_client_requires_credentials() -> None:
    arguments = create_parser().parse_args(["--server", "https://example.test"])
    with pytest.raises(ValueError, match="Provide --token"):
        build_client(arguments)


def test_limit_catalog_caps_per_platform() -> None:
    catalog = pd.DataFrame(
        {"platform": ["linux", "linux", "windows", "windows"], "name": ["a", "b", "c", "d"], "timeout_s": [300.0] * 4}
    )
    limited = limit_catalog(catalog, 1)
    assert list(zip(limited["platform"], limited["name"], strict=False)) == [("linux", "a"), ("windows", "c")]
    assert len(limit_catalog(catalog, 0)) == 4


def test_parser_email_to_is_optional() -> None:
    arguments = create_parser().parse_args(["--server", "https://example.test"])
    assert arguments.email_to == ""
    assert not hasattr(arguments, "full_report_url")
    with_recipient = create_parser().parse_args(
        ["--server", "https://example.test", "--email-to", "robin.vanwestrenen@deltares.nl"]
    )
    assert with_recipient.email_to == "robin.vanwestrenen@deltares.nl"


def test_run_sends_email_only_when_requested(tmp_path: Path, mocker: MockerFixture) -> None:
    catalog = pd.DataFrame({"platform": ["linux"], "name": ["a"], "timeout_s": [300.0]})
    mocker.patch("ci_tools.testbench_timeout_report.cli.load_catalog", return_value=catalog)
    mocker.patch("ci_tools.testbench_timeout_report.cli.build_client")
    mocker.patch(
        "ci_tools.testbench_timeout_report.cli.fetch_runs",
        return_value=(pd.DataFrame(columns=["platform", "name", "duration_s"]), pd.DataFrame()),
    )
    mocker.patch("ci_tools.testbench_timeout_report.cli.list_test_names", return_value=set())
    mocker.patch("ci_tools.testbench_timeout_report.cli.summarize", return_value=catalog)
    mocker.patch("ci_tools.testbench_timeout_report.cli.write_artifacts")
    send_report = mocker.patch("ci_tools.testbench_timeout_report.cli.send_report")

    skip = create_parser().parse_args(
        ["--server", "https://example.test", "--token", "abc", "--output-dir", str(tmp_path)]
    )
    assert run(skip) == 0
    send_report.assert_not_called()

    send = create_parser().parse_args(
        [
            "--server",
            "https://example.test",
            "--token",
            "abc",
            "--output-dir",
            str(tmp_path),
            "--email-to",
            "robin.vanwestrenen@deltares.nl",
        ]
    )
    assert run(send) == 0
    send_report.assert_called_once_with(tmp_path / "email.html", "robin.vanwestrenen@deltares.nl")
