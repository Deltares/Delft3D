from unittest.mock import Mock, patch

from ci_tools.dimrset_delivery.dimr_context import Credentials, DimrAutomationContext
from ci_tools.dimrset_delivery.lib.jira import Jira

# test credentials
test_user = ""
test_token = "dummy_value"


def make_jira(dry_run: bool = False) -> Jira:
    mock_context = Mock(spec=DimrAutomationContext)
    mock_context.dry_run = dry_run
    return Jira(credentials=Credentials(username=test_user, password=test_token), context=mock_context)


def test_test_connection_success() -> None:
    jira = make_jira()
    with patch("ci_tools.dimrset_delivery.lib.jira.requests.get") as mock_get:
        mock_response = Mock()
        mock_response.status_code = 200
        mock_response.content = b"ok"
        mock_get.return_value = mock_response
        # Act
        result = jira.test_connection()
        # Assert
        assert result is True
        # Verify PAT Bearer auth is used (not basic auth)
        mock_get.assert_called_once()
        call_kwargs = mock_get.call_args.kwargs
        headers = call_kwargs.get("headers", {})
        assert headers.get("Authorization") == f"Bearer {test_token}"
        assert "auth" not in call_kwargs or call_kwargs.get("auth") is None


def test_test_connection_failure() -> None:
    jira = make_jira()
    with patch("ci_tools.dimrset_delivery.lib.jira.requests.get") as mock_get:
        mock_response = Mock()
        mock_response.status_code = 401
        mock_response.content = b"unauthorized"
        mock_get.return_value = mock_response
        # Act
        result = jira.test_connection()
        # Assert
        assert result is False
        # Verify PAT Bearer auth header (even on failure)
        call_kwargs = mock_get.call_args.kwargs
        headers = call_kwargs.get("headers", {})
        assert headers.get("Authorization") == f"Bearer {test_token}"


def test_test_connection_dry_run() -> None:
    jira = make_jira(dry_run=True)

    # No HTTP call should be made, just a mocked result
    result = jira.test_connection()

    assert result is True  # dry-run always returns 200


def test_get_issue_success() -> None:
    jira = make_jira()
    issue_key = "DEVOPSDSC-200"
    with patch("ci_tools.dimrset_delivery.lib.jira.requests.get") as mock_get:
        mock_response = Mock()
        mock_response.status_code = 200
        mock_response.json.return_value = {"key": issue_key, "fields": {"summary": "Some summary"}}
        mock_get.return_value = mock_response

        issue = jira.get_issue(issue_key)

        assert issue is not None
        assert issue["key"] == issue_key
        assert "summary" in issue["fields"]
        # Verify Bearer auth header used
        call_kwargs = mock_get.call_args.kwargs
        headers = call_kwargs.get("headers", {})
        assert headers.get("Authorization") == f"Bearer {test_token}"


def test_get_issue_failure() -> None:
    jira = make_jira()
    issue_key = "DEVOPSDSC-404"
    with patch("ci_tools.dimrset_delivery.lib.jira.requests.get") as mock_get:
        mock_response = Mock()
        mock_response.status_code = 404
        mock_response.content = b"not found"
        mock_get.return_value = mock_response

        issue = jira.get_issue(issue_key)

        assert issue is None
        # Verify Bearer auth header used even on failure
        call_kwargs = mock_get.call_args.kwargs
        headers = call_kwargs.get("headers", {})
        assert headers.get("Authorization") == f"Bearer {test_token}"


def test_get_issue_dry_run() -> None:
    jira = make_jira(dry_run=True)
    issue_key = "DEVOPSDSC-999"

    issue = jira.get_issue(issue_key)

    assert issue["key"] == issue_key
    assert "[dry-run mock]" in issue["fields"]["summary"]


def test_test_connection_403_basic_auth_disabled() -> None:
    """Specific regression test for the case where Jira has disabled basic auth (returns 403).

    The client must use Bearer PAT, not basic auth, to succeed.
    """
    jira = make_jira()
    with patch("ci_tools.dimrset_delivery.lib.jira.requests.get") as mock_get:
        mock_response = Mock()
        mock_response.status_code = 403
        mock_response.content = b'{"message":"Basic Authentication has been disabled on this instance."}'
        mock_get.return_value = mock_response

        result = jira.test_connection()

        assert result is False
        call_kwargs = mock_get.call_args.kwargs
        headers = call_kwargs.get("headers", {})
        assert headers.get("Authorization") == f"Bearer {test_token}"
        assert "auth" not in call_kwargs or call_kwargs.get("auth") is None
