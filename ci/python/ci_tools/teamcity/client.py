import json
from typing import Any, Generator

from httpx import Auth, Client, Request, Response


class TeamcityClient:
    """Create a client to commuicate to Teamcity."""

    def __init__(self, client: Client, server: str) -> None:
        self._client = client
        self._server = server

    @staticmethod
    def with_bearer_token_auth(token: str, server: str, verify: bool = True) -> "TeamcityClient":
        """Create a bearer authenticated client."""
        client = Client(auth=BearerTokenAuth(token=token), timeout=20.0, verify=verify, base_url=f"https://{server}")
        return TeamcityClient(client, server)

    def call_teamcity_api(self, url: str, payload: str = "", method: str = "GET") -> Response:
        """Call the teamcity api.

        Returns
        -------
        Response: response of the call
        """
        headers = {
            "user-agent": "teamcity_api_cli_tool/1.0",
            "Content-Type": "application/json",
            "Accept": "application/json",
        }
        match method:
            case "GET":
                return self._client.get(url, headers=headers)
            case "POST":
                return self._client.post(url, headers=headers, json=payload)
            case "PUT":
                return self._client.put(url, headers=headers, json=payload)
            case "DELETE":
                return self._client.request(method="DELETE", url=url, headers=headers, content=payload)
            case _:
                return Response(500, text="Unsupported http method")

    def list_tags_on_build(self, build_configuration_id: str) -> list[str]:
        """Get a list of all the tags in a build configuration."""
        list_of_tags = self.call_teamcity_api(
            url=f"/app/rest/buildTypes/id:{build_configuration_id}/buildTags?fields=tag(name)"
        )
        list_of_tags.raise_for_status()
        response_json = json.loads(list_of_tags.content)
        result: list[str] = [tag_obj["name"] for tag_obj in response_json["tag"]]
        return result

    def remove_tag_from_build(self, build_configuration_id: str, tag_name: str) -> None:
        """Call teamcity and remove tags the tag from the buildconfiguration."""
        payload = f'{{"tag": [{{"name": "{tag_name}"}}]}}'
        urlbase = "/app/rest/builds/multiple"
        extra_filters = "defaultFilter:false,lookupLimit:1000000000"
        resturl = f"{urlbase}/buildType:{build_configuration_id},{extra_filters},tag:name:{tag_name}/tags"
        response = self.call_teamcity_api(
            url=resturl,
            method="DELETE",
            payload=payload,
        )
        response.raise_for_status()


class BearerTokenAuth(Auth):
    """Authenticated a request with a Bearer token."""

    def __init__(self, token: str) -> None:
        self._token = token

    def auth_flow(self, request: Request) -> Generator[Request, Any, None]:
        """Add the authorization header."""
        request.headers["Authorization"] = f"Bearer {self._token}"
        yield request
