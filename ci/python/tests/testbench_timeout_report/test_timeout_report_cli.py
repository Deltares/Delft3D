import os

import pytest
from pytest_mock import MockerFixture

from ci_tools.teamcity.client import TeamcityClient
from ci_tools.testbench_timeout_report.catalog import CaseTimeout
from ci_tools.testbench_timeout_report.cli import _limit_catalog, build_client, create_parser


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
    catalog = [
        CaseTimeout("linux", "a", 300.0, ("a.xml",), False),
        CaseTimeout("linux", "b", 300.0, ("b.xml",), False),
        CaseTimeout("windows", "c", 300.0, ("c.xml",), False),
        CaseTimeout("windows", "d", 300.0, ("d.xml",), False),
    ]
    limited = _limit_catalog(catalog, 1)
    assert [(case.platform, case.name) for case in limited] == [("linux", "a"), ("windows", "c")]
    assert _limit_catalog(catalog, 0) == catalog
