import json

from httpx import Request, Response
from pytest_mock import MockerFixture

from ci_tools.teamcity.client import TeamcityClient


def test_base_url_accepts_host_or_url() -> None:
    assert TeamcityClient.base_url("dpcbuild.deltares.nl") == "https://dpcbuild.deltares.nl"
    assert TeamcityClient.base_url("https://dpcbuild.deltares.nl/") == "https://dpcbuild.deltares.nl"


def test_get_paginated_items_follows_nexthref(mocker: MockerFixture) -> None:
    http_client = mocker.Mock()
    http_client.get.side_effect = [
        Response(
            200,
            content=json.dumps({"tag": [{"name": "a"}], "nextHref": "/app/rest/tags?start=1"}).encode(),
            request=Request("GET", "https://example.test/app/rest/tags"),
            headers={"Content-Type": "application/json"},
        ),
        Response(
            200,
            content=json.dumps({"tag": [{"name": "b"}]}).encode(),
            request=Request("GET", "https://example.test/app/rest/tags?start=1"),
            headers={"Content-Type": "application/json"},
        ),
    ]
    client = TeamcityClient(client=http_client, server="example.test")

    tags = client.get_paginated_items("/app/rest/tags", "tag")

    assert tags == [{"name": "a"}, {"name": "b"}]
    assert http_client.get.call_count == 2
