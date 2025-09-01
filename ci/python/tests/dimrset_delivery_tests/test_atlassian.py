from unittest.mock import Mock, patch

from ci_tools.dimrset_delivery.dimr_context import Credentials, DimrAutomationContext
from ci_tools.dimrset_delivery.lib.atlassian import Atlassian

# Arrange: common test credentials
test_username = "user"
test_password = "pass"


def test_test_connection_success() -> None:
    # Arrange
    mock_context = Mock(spec=DimrAutomationContext)
    mock_context.dry_run = False
    atlassian = Atlassian(credentials=Credentials(test_username, test_password), context=mock_context)
    with patch("ci_tools.dimrset_delivery.lib.atlassian.requests.get") as mock_get:
        mock_response = Mock()
        mock_response.status_code = 200
        mock_response.content = b"ok"
        mock_get.return_value = mock_response
        # Act
        result = atlassian.test_connection()
        # Assert
        assert result is True


def test_test_connection_failure() -> None:
    # Arrange
    mock_context = Mock(spec=DimrAutomationContext)
    mock_context.dry_run = False
    atlassian = Atlassian(credentials=Credentials(test_username, test_password), context=mock_context)
    with patch("ci_tools.dimrset_delivery.lib.atlassian.requests.get") as mock_get:
        mock_response = Mock()
        mock_response.status_code = 401
        mock_response.content = b"unauthorized"
        mock_get.return_value = mock_response
        # Act
        result = atlassian.test_connection()
        # Assert
        assert result is False


def test_get_page_info_for_parent_page_success() -> None:
    # Arrange
    mock_context = Mock(spec=DimrAutomationContext)
    atlassian = Atlassian(credentials=Credentials(test_username, test_password), context=mock_context)
    with patch("ci_tools.dimrset_delivery.lib.atlassian.requests.get") as mock_get:
        mock_response = Mock()
        mock_response.status_code = 200
        mock_response.json.return_value = {"results": ["page1", "page2"]}
        mock_get.return_value = mock_response
        # Act
        result = atlassian.get_page_info_for_parent_page("1234")
        # Assert
        assert result == {"results": ["page1", "page2"]}


def test_get_page_info_for_parent_page_failure() -> None:
    # Arrange
    mock_context = Mock(spec=DimrAutomationContext)
    atlassian = Atlassian(credentials=Credentials(test_username, test_password), context=mock_context)
    with patch("ci_tools.dimrset_delivery.lib.atlassian.requests.get") as mock_get:
        mock_response = Mock()
        mock_response.status_code = 404
        mock_response.content = b"not found"
        mock_get.return_value = mock_response
        # Act
        result = atlassian.get_page_info_for_parent_page("1234")
        # Assert
        assert result is None


def test_create_public_wiki_page_success() -> None:
    # Arrange
    mock_context = Mock(spec=DimrAutomationContext)
    atlassian = Atlassian(credentials=Credentials(test_username, test_password), context=mock_context)
    with patch("ci_tools.dimrset_delivery.lib.atlassian.requests.post") as mock_post:
        mock_response = Mock()
        mock_response.status_code = 200
        mock_response.json.return_value = {"id": "5678"}
        mock_post.return_value = mock_response
        # Act
        result = atlassian.create_public_wiki_page("title", "SPACE", "1234", "body")
        # Assert
        assert result == "5678"


def test_create_public_wiki_page_failure() -> None:
    # Arrange
    mock_context = Mock(spec=DimrAutomationContext)
    atlassian = Atlassian(credentials=Credentials(test_username, test_password), context=mock_context)
    with patch("ci_tools.dimrset_delivery.lib.atlassian.requests.post") as mock_post:
        mock_response = Mock()
        mock_response.status_code = 400
        mock_response.content = b"bad request"
        mock_post.return_value = mock_response
        # Act
        result = atlassian.create_public_wiki_page("title", "SPACE", "1234", "body")
        # Assert
        assert result is None


def test_update_page_success() -> None:
    # Arrange
    mock_context = Mock(spec=DimrAutomationContext)
    atlassian = Atlassian(credentials=Credentials(test_username, test_password), context=mock_context)
    with patch.object(Atlassian, "_Atlassian__get_page_version", return_value=1):
        with patch("ci_tools.dimrset_delivery.lib.atlassian.requests.put") as mock_put:
            mock_response = Mock()
            mock_response.status_code = 200
            mock_put.return_value = mock_response
            # Act
            result = atlassian.update_page("5678", "title", "content")
            # Assert
            assert result is True


def test_update_page_failure() -> None:
    # Arrange
    mock_context = Mock(spec=DimrAutomationContext)
    atlassian = Atlassian(credentials=Credentials(test_username, test_password), context=mock_context)
    with patch.object(Atlassian, "_Atlassian__get_page_version", return_value=1):
        with patch("ci_tools.dimrset_delivery.lib.atlassian.requests.put") as mock_put:
            mock_response = Mock()
            mock_response.status_code = 500
            mock_response.content = b"error"
            mock_put.return_value = mock_response
            # Act
            result = atlassian.update_page("5678", "title", "content")
            # Assert
            assert result is False


def test_update_page_cannot_get_version() -> None:
    # Arrange
    mock_context = Mock(spec=DimrAutomationContext)
    atlassian = Atlassian(credentials=Credentials(test_username, test_password), context=mock_context)
    with patch.object(Atlassian, "_Atlassian__get_page_version", return_value=None):
        # Act
        result = atlassian.update_page("5678", "title", "content")
        # Assert
        assert result is False
