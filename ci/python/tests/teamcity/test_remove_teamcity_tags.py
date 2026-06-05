import httpx
import pytest
from httpx import Client, HTTPStatusError, Request, Response
from pytest_mock import MockerFixture

from ci_tools.teamcity.client import TeamcityClient


def test_client_create_client(mocker: MockerFixture) -> None:
    httpclient = mocker.Mock(spec=Client)
    httpclient.get.return_value = Response(418)
    server = "localhost"
    teamcityclient = TeamcityClient(client=httpclient, server=server)
    response = teamcityclient.call_teamcity_api(url="/app/rest/testrequest")
    assert response.status_code == 418


def test_list_tags_on_build_empty_list(mocker: MockerFixture) -> None:
    httpclient = mocker.Mock(spec=Client)
    httpclient.get.return_value = Response(200)
    httpclient = mocker.Mock(spec=Client)
    content = '{"count": 0 ,"tag" : []}'
    httpclient.get.return_value = Response(content=content, status_code=200, request=Request("GET", "/app"))
    build_configuration_id = "TestConfigurationId"
    server = "localhost"
    teamcityclient = TeamcityClient(client=httpclient, server=server)
    test_list_tags = teamcityclient.list_tags_on_build(build_configuration_id)
    assert 0 == len(test_list_tags)


def test_list_tags_on_build_list_filled(mocker: MockerFixture) -> None:
    httpclient = mocker.Mock(spec=Client)
    content = '{"count": 1 ,"tag" : [{"name" : "testTag"}]}'
    httpclient.get.return_value = Response(content=content, status_code=200, request=Request("GET", "/app"))
    build_configuration_id = "TestConfigurationId"
    server = "localhost"
    teamcityclient = TeamcityClient(client=httpclient, server=server)
    test_list_tags = teamcityclient.list_tags_on_build(build_configuration_id)
    assert "testTag" == test_list_tags[0]


def test_list_tags_on_build_api_error(mocker: MockerFixture) -> None:
    httpclient = mocker.Mock(spec=Client)
    httpclient.get.return_value = Response(500, request=Request("GET", "/app"))
    build_configuration_id = "TestConfigurationId"
    server = "localhost"
    teamcityclient = TeamcityClient(client=httpclient, server=server)

    with pytest.raises(HTTPStatusError) as context:
        teamcityclient.list_tags_on_build(build_configuration_id)
    assert httpx.HTTPStatusError == context.type
