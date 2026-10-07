import json
from typing import Any, Generator
from urllib.parse import parse_qsl, urlparse

from httpx import Auth, BasicAuth, Client, Request, Response


class TeamcityClient:
    """Create a client to commuicate to Teamcity."""

    def __init__(self, client: Client, server: str) -> None:
        self._client = client
        self._server = server

    @staticmethod
    def base_url(server: str) -> str:
        """Normalize a TeamCity host or URL to a base URL.

        Parameters
        ----------
        server : str
            Hostname (``dpcbuild.deltares.nl``) or full URL.

        Returns
        -------
        str
            Base URL without a trailing slash.
        """
        if server.startswith("http://") or server.startswith("https://"):
            return server.rstrip("/")
        return f"https://{server}"

    @staticmethod
    def with_bearer_token_auth(token: str, server: str, verify: bool = True, timeout: float = 20.0) -> "TeamcityClient":
        """Create a bearer authenticated client."""
        client = Client(
            auth=BearerTokenAuth(token=token),
            timeout=timeout,
            verify=verify,
            base_url=TeamcityClient.base_url(server),
        )
        return TeamcityClient(client, server)

    @staticmethod
    def with_basic_auth(
        username: str,
        password: str,
        server: str,
        verify: bool = True,
        timeout: float = 60.0,
    ) -> "TeamcityClient":
        """Create a basic-auth authenticated client.

        Parameters
        ----------
        username : str
            TeamCity username.
        password : str
            TeamCity password or access token used as a password.
        server : str
            Hostname or full URL.
        verify : bool, optional
            Verify TLS certificates. Defaults to True.
        timeout : float, optional
            HTTP timeout in seconds. Defaults to 60.

        Returns
        -------
        TeamcityClient
            Authenticated client.
        """
        client = Client(
            auth=BasicAuth(username, password),
            timeout=timeout,
            verify=verify,
            base_url=TeamcityClient.base_url(server),
        )
        return TeamcityClient(client, server)

    def call_teamcity_api(
        self,
        url: str,
        payload: str = "",
        method: str = "GET",
        params: dict[str, str] | None = None,
    ) -> Response:
        """Call the teamcity api.

        Parameters
        ----------
        url : str
            REST path, for example ``/app/rest/tests``.
        payload : str, optional
            JSON payload for POST/PUT/DELETE.
        method : str, optional
            HTTP method. Defaults to GET.
        params : dict[str, str] | None, optional
            Query string parameters.

        Returns
        -------
        Response
            Response of the call.
        """
        headers = {
            "user-agent": "teamcity_api_cli_tool/1.0",
            "Content-Type": "application/json",
            "Accept": "application/json",
        }
        match method:
            case "GET":
                return self._client.get(url, headers=headers, params=params)
            case "POST":
                return self._client.post(url, headers=headers, json=payload, params=params)
            case "PUT":
                return self._client.put(url, headers=headers, json=payload, params=params)
            case "DELETE":
                return self._client.request(method="DELETE", url=url, headers=headers, content=payload, params=params)
            case _:
                return Response(500, text="Unsupported http method")

    def get_json(self, url: str, params: dict[str, str] | None = None) -> dict[str, Any]:
        """GET a JSON TeamCity REST resource.

        Parameters
        ----------
        url : str
            REST path or ``nextHref`` value.
        params : dict[str, str] | None, optional
            Query string parameters. Ignored when ``url`` already contains a query.

        Returns
        -------
        dict[str, Any]
            Parsed JSON body.
        """
        path, query_params = _split_href(url)
        merged = dict(query_params)
        if params:
            merged.update(params)
        response = self.call_teamcity_api(path, params=merged or None)
        response.raise_for_status()
        payload: dict[str, Any] = response.json()
        return payload

    def get_paginated_items(self, url: str, item_key: str, params: dict[str, str] | None = None) -> list[Any]:
        """GET a paginated TeamCity collection until ``nextHref`` is exhausted.

        Parameters
        ----------
        url : str
            REST path of the first page.
        item_key : str
            JSON array key, for example ``testOccurrence``.
        params : dict[str, str] | None, optional
            Query string parameters for the first page.

        Returns
        -------
        list[Any]
            Concatenated items from all pages.
        """
        items: list[Any] = []
        next_url: str | None = url
        next_params = params
        while next_url:
            data = self.get_json(next_url, params=next_params)
            items.extend(data.get(item_key, []))
            next_href = data.get("nextHref")
            if not next_href:
                break
            next_url = str(next_href)
            next_params = None
        return items

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


def _split_href(url: str) -> tuple[str, dict[str, str]]:
    """Split a REST path or absolute nextHref into path and query params."""
    parsed = urlparse(url)
    if not parsed.query:
        return url, {}
    path = parsed.path or url
    if not path.startswith("/"):
        path = "/" + path
    query_params = dict(parse_qsl(parsed.query, keep_blank_values=True))
    return path, query_params


class BearerTokenAuth(Auth):
    """Authenticated a request with a Bearer token."""

    def __init__(self, token: str) -> None:
        self._token = token

    def auth_flow(self, request: Request) -> Generator[Request, Any, None]:
        """Add the authorization header."""
        request.headers["Authorization"] = f"Bearer {self._token}"
        yield request
