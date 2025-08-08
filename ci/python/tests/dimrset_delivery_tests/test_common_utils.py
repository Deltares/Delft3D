"""Tests for common_utils.py."""

from io import StringIO
from unittest.mock import Mock, mock_open, patch

import pytest

from ci_tools.dimrset_delivery.common_utils import (
    get_previous_testbank_result_parser,
    get_tag_from_build_info,
    get_testbank_result_parser,
    initialize_clients,
    print_dry_run_message,
)
from ci_tools.dimrset_delivery.dimr_context import DimrAutomationContext
from ci_tools.dimrset_delivery.helpers.git_client import GitClient
from ci_tools.dimrset_delivery.helpers.result_testbank_parser import ResultTestBankParser
from ci_tools.dimrset_delivery.helpers.ssh_client import SshClient
from ci_tools.dimrset_delivery.lib.atlassian import Atlassian
from ci_tools.dimrset_delivery.lib.teamcity import TeamCity


class TestInitializeClients:
    """Test cases for initialize_clients function."""

    @patch("ci_tools.dimrset_delivery.common_utils.GitClient")
    @patch("ci_tools.dimrset_delivery.common_utils.SshClient")
    @patch("ci_tools.dimrset_delivery.common_utils.TeamCity")
    @patch("ci_tools.dimrset_delivery.common_utils.Atlassian")
    def test_initialize_clients_creates_all_wrappers(
        self,
        mock_atlassian: Mock,
        mock_teamcity: Mock,
        mock_ssh_client: Mock,
        mock_git_client: Mock,
    ) -> None:
        """Test that initialize_clients creates all required client wrappers."""
        # Arrange
        username = "test_user"
        password = "test_password"
        personal_access_token = "test_token"

        mock_atlassian_instance = Mock(spec=Atlassian)
        mock_teamcity_instance = Mock(spec=TeamCity)
        mock_ssh_client_instance = Mock(spec=SshClient)
        mock_git_client_instance = Mock(spec=GitClient)

        mock_atlassian.return_value = mock_atlassian_instance
        mock_teamcity.return_value = mock_teamcity_instance
        mock_ssh_client.return_value = mock_ssh_client_instance
        mock_git_client.return_value = mock_git_client_instance

        # Act
        result = initialize_clients(username, password, personal_access_token)

        # Assert
        assert len(result) == 4
        atlassian_wrapper, teamcity_wrapper, ssh_client_wrapper, git_client_wrapper = result

        assert atlassian_wrapper == mock_atlassian_instance
        assert teamcity_wrapper == mock_teamcity_instance
        assert ssh_client_wrapper == mock_ssh_client_instance
        assert git_client_wrapper == mock_git_client_instance

        # Verify correct initialization parameters
        mock_atlassian.assert_called_once_with(username=username, password=password)
        mock_teamcity.assert_called_once_with(username=username, password=password)
        mock_ssh_client.assert_called_once_with(username=username, password=password, connect_timeout=30)
        mock_git_client.assert_called_once_with(
            "https://github.com/Deltares/Delft3D.git", username, personal_access_token
        )

    @patch("ci_tools.dimrset_delivery.common_utils.GitClient")
    @patch("ci_tools.dimrset_delivery.common_utils.SshClient")
    @patch("ci_tools.dimrset_delivery.common_utils.TeamCity")
    @patch("ci_tools.dimrset_delivery.common_utils.Atlassian")
    def test_initialize_clients_with_empty_credentials(
        self,
        mock_atlassian: Mock,
        mock_teamcity: Mock,
        mock_ssh_client: Mock,
        mock_git_client: Mock,
    ) -> None:
        """Test initialize_clients with empty credentials."""
        # Arrange
        username = ""
        password = ""
        personal_access_token = ""

        # Act
        initialize_clients(username, password, personal_access_token)

        # Assert
        mock_atlassian.assert_called_once_with(username="", password="")
        mock_teamcity.assert_called_once_with(username="", password="")
        mock_ssh_client.assert_called_once_with(username="", password="", connect_timeout=30)
        mock_git_client.assert_called_once_with("https://github.com/Deltares/Delft3D.git", "", "")


class TestGetTestResultTestBankParser:
    """Test cases for get_testbank_result_parser function."""

    @patch("ci_tools.dimrset_delivery.common_utils.ResultTestBankParser")
    @patch("ci_tools.dimrset_delivery.common_utils.open", new_callable=mock_open, read_data=b"test data")
    def test_get_testbank_result_parser_reads_file_and_creates_parser(
        self, mock_file: Mock, mock_parser_class: Mock
    ) -> None:
        """Test that get_testbank_result_parser reads file and creates parser correctly."""
        # Arrange
        mock_parser_instance = Mock(spec=ResultTestBankParser)
        mock_parser_class.return_value = mock_parser_instance

        # Act
        result = get_testbank_result_parser()

        # Assert
        assert result == mock_parser_instance
        mock_file.assert_called_once_with("teamcity_test_results.txt", "rb")
        mock_parser_class.assert_called_once_with("test data")

    @patch("ci_tools.dimrset_delivery.common_utils.ResultTestBankParser")
    @patch("ci_tools.dimrset_delivery.common_utils.open", new_callable=mock_open, read_data=b"")
    def test_get_testbank_result_parser_with_empty_file(self, mock_file: Mock, mock_parser_class: Mock) -> None:
        """Test get_testbank_result_parser with empty file."""
        # Arrange
        mock_parser_instance = Mock(spec=ResultTestBankParser)
        mock_parser_class.return_value = mock_parser_instance

        # Act
        result = get_testbank_result_parser()

        # Assert
        assert result == mock_parser_instance
        mock_parser_class.assert_called_once_with("")

    @patch("ci_tools.dimrset_delivery.common_utils.ResultTestBankParser")
    @patch("ci_tools.dimrset_delivery.common_utils.open")
    def test_get_testbank_result_parser_handles_file_not_found(self, mock_file: Mock, mock_parser_class: Mock) -> None:
        """Test that get_testbank_result_parser handles file not found error."""
        # Arrange
        mock_file.side_effect = FileNotFoundError("File not found")

        # Act & Assert
        with pytest.raises(FileNotFoundError, match="File not found"):
            get_testbank_result_parser()

    @patch("ci_tools.dimrset_delivery.common_utils.ResultTestBankParser")
    @patch("ci_tools.dimrset_delivery.common_utils.open", new_callable=mock_open, read_data=b"\xc3\xa9test data")
    def test_get_testbank_result_parser_handles_unicode_data(self, mock_file: Mock, mock_parser_class: Mock) -> None:
        """Test get_testbank_result_parser with unicode data."""
        # Arrange
        mock_parser_instance = Mock(spec=ResultTestBankParser)
        mock_parser_class.return_value = mock_parser_instance

        # Act
        result = get_testbank_result_parser()

        # Assert
        assert result == mock_parser_instance
        mock_parser_class.assert_called_once_with("étest data")


class TestPrintDryRunMessage:
    """Test cases for print_dry_run_message function."""

    @patch("sys.stdout", new_callable=StringIO)
    def test_print_dry_run_message_when_dry_run_true(self, mock_stdout: StringIO) -> None:
        """Test that print_dry_run_message prints message when dry_run is True."""
        # Act
        print_dry_run_message(True)

        # Assert
        output = mock_stdout.getvalue()
        assert "[DRY-RUN] - no changes will be made\n" == output

    @patch("sys.stdout", new_callable=StringIO)
    def test_print_dry_run_message_when_dry_run_false(self, mock_stdout: StringIO) -> None:
        """Test that print_dry_run_message prints nothing when dry_run is False."""
        # Act
        print_dry_run_message(False)

        # Assert
        output = mock_stdout.getvalue()
        assert output == ""

    @patch("builtins.print")
    def test_print_dry_run_message_calls_print_with_correct_message(self, mock_print: Mock) -> None:
        """Test that print_dry_run_message calls print with correct message."""
        # Act
        print_dry_run_message(True)

        # Assert
        mock_print.assert_called_once_with("[DRY-RUN] - no changes will be made")

    @patch("builtins.print")
    def test_print_dry_run_message_does_not_call_print_when_false(self, mock_print: Mock) -> None:
        """Test that print_dry_run_message does not call print when dry_run is False."""
        # Act
        print_dry_run_message(False)

        # Assert
        mock_print.assert_not_called()


class TestGetPreviousTestbankResultParser:
    """Test cases for get_previous_testbank_result_parser function."""

    def test_no_teamcity_client_raises_error(self) -> None:
        """Test that missing TeamCity client raises ValueError."""
        # Arrange
        mock_context = Mock(spec=DimrAutomationContext)
        mock_context.teamcity = None

        # Act & Assert
        with pytest.raises(ValueError, match="TeamCity client is required but not initialized"):
            get_previous_testbank_result_parser(mock_context)

    def test_no_current_build_info_returns_none(self) -> None:
        """Test that missing current build info returns None."""
        # Arrange
        mock_context = Mock(spec=DimrAutomationContext)
        mock_context.build_id = "12345"
        mock_context.teamcity = Mock()
        mock_context.teamcity.get_full_build_info_for_build_id.return_value = None

        # Act
        result = get_previous_testbank_result_parser(mock_context)

        # Assert
        assert result is None
        mock_context.teamcity.get_full_build_info_for_build_id.assert_called_once_with("12345")

    def test_no_build_type_id_returns_none(self) -> None:
        """Test that missing buildTypeId returns None."""
        # Arrange
        mock_context = Mock(spec=DimrAutomationContext)
        mock_context.build_id = "12345"
        mock_context.teamcity = Mock()
        mock_context.teamcity.get_full_build_info_for_build_id.return_value = {}

        # Act
        result = get_previous_testbank_result_parser(mock_context)

        # Assert
        assert result is None

    def test_no_builds_returns_none(self) -> None:
        """Test that no builds available returns None."""
        # Arrange
        mock_context = Mock(spec=DimrAutomationContext)
        mock_context.build_id = "12345"
        mock_context.teamcity = Mock()
        mock_context.teamcity.get_full_build_info_for_build_id.return_value = {
            "buildTypeId": "bt123",
            "tags": {"tag": [{"name": "DIMRset_1.2.3"}]},
        }
        mock_context.teamcity.get_builds_for_build_type_id.return_value = None

        # Act
        result = get_previous_testbank_result_parser(mock_context)

        # Assert
        assert result is None

    @patch("ci_tools.dimrset_delivery.common_utils.ResultTestBankParser")
    def test_successful_previous_parser_retrieval(self, mock_parser_class: Mock) -> None:
        """Test successful retrieval of previous testbank result parser."""
        # Arrange
        mock_context = Mock(spec=DimrAutomationContext)
        mock_context.build_id = "12345"
        mock_context.teamcity = Mock()

        current_build_info = {"buildTypeId": "bt123", "tags": {"tag": [{"name": "DIMRset_1.2.3"}]}}

        builds_response = {
            "build": [
                {"id": 12346},  # Different build
                {"id": 12344},  # Previous build
            ]
        }

        previous_build_info = {"buildTypeId": "bt123", "tags": {"tag": [{"name": "DIMRset_1.2.2"}]}}

        mock_artifact_content = b"artifact content"

        mock_context.teamcity.get_full_build_info_for_build_id.side_effect = [
            current_build_info,  # For current build
            None,  # For first loop build (12346)
            previous_build_info,  # For second loop build (12344)
        ]
        mock_context.teamcity.get_builds_for_build_type_id.return_value = builds_response
        mock_context.teamcity.get_build_artifact.return_value = mock_artifact_content

        mock_parser_instance = Mock(spec=ResultTestBankParser)
        mock_parser_class.return_value = mock_parser_instance

        # Act
        result = get_previous_testbank_result_parser(mock_context)

        # Assert
        assert result == mock_parser_instance
        mock_parser_class.assert_called_once_with("artifact content")
        mock_context.teamcity.get_build_artifact.assert_called_once()

    def test_no_previous_version_found_returns_none(self) -> None:
        """Test that no previous version found returns None."""
        # Arrange
        mock_context = Mock(spec=DimrAutomationContext)
        mock_context.build_id = "12345"
        mock_context.teamcity = Mock()

        current_build_info = {"buildTypeId": "bt123", "tags": {"tag": [{"name": "DIMRset_1.2.3"}]}}

        builds_response = {
            "build": [
                {"id": 12346},  # Different build
            ]
        }

        other_build_info = {
            "buildTypeId": "bt123",
            "tags": {"tag": [{"name": "DIMRset_1.2.4"}]},  # Higher version
        }

        mock_context.teamcity.get_full_build_info_for_build_id.side_effect = [
            current_build_info,  # For current build
            other_build_info,  # For other build
        ]
        mock_context.teamcity.get_builds_for_build_type_id.return_value = builds_response

        # Act
        result = get_previous_testbank_result_parser(mock_context)

        # Assert
        assert result is None

    def test_no_artifact_returns_none(self) -> None:
        """Test that missing artifact returns None."""
        # Arrange
        mock_context = Mock(spec=DimrAutomationContext)
        mock_context.build_id = "12345"
        mock_context.teamcity = Mock()

        current_build_info = {"buildTypeId": "bt123", "tags": {"tag": [{"name": "DIMRset_1.2.3"}]}}

        builds_response = {
            "build": [
                {"id": 12344},  # Previous build
            ]
        }

        previous_build_info = {"buildTypeId": "bt123", "tags": {"tag": [{"name": "DIMRset_1.2.2"}]}}

        mock_context.teamcity.get_full_build_info_for_build_id.side_effect = [
            current_build_info,  # For current build
            previous_build_info,  # For previous build
        ]
        mock_context.teamcity.get_builds_for_build_type_id.return_value = builds_response
        mock_context.teamcity.get_build_artifact.return_value = None

        # Act
        result = get_previous_testbank_result_parser(mock_context)

        # Assert
        assert result is None


class TestGetTagFromBuildInfo:
    """Test cases for get_tag_from_build_info function."""

    def test_no_tags_returns_default(self) -> None:
        """Test that build info without tags returns default tuple."""
        # Arrange
        build_info = {}

        # Act
        result = get_tag_from_build_info(build_info)

        # Assert
        assert result == (0, 0, 0)

    def test_empty_tags_returns_default(self) -> None:
        """Test that empty tags returns default tuple."""
        # Arrange
        build_info = {"tags": {"tag": []}}

        # Act
        result = get_tag_from_build_info(build_info)

        # Assert
        assert result == (0, 0, 0)

    def test_no_dimrset_tag_returns_default(self) -> None:
        """Test that tags without DIMRset prefix return default tuple."""
        # Arrange
        build_info = {"tags": {"tag": [{"name": "some_other_tag"}, {"name": "another_tag"}]}}

        # Act
        result = get_tag_from_build_info(build_info)

        # Assert
        assert result == (0, 0, 0)

    def test_valid_dimrset_tag_returns_version(self) -> None:
        """Test that valid DIMRset tag returns parsed version."""
        # Arrange
        build_info = {"tags": {"tag": [{"name": "some_other_tag"}, {"name": "DIMRset_1.2.3"}, {"name": "another_tag"}]}}

        # Act
        result = get_tag_from_build_info(build_info)

        # Assert
        assert result == (1, 2, 3)

    def test_multiple_dimrset_tags_returns_last_valid(self) -> None:
        """Test that multiple DIMRset tags returns the last valid one."""
        # Arrange
        build_info = {
            "tags": {"tag": [{"name": "DIMRset_1.0.0"}, {"name": "DIMRset_2.3.4"}, {"name": "some_other_tag"}]}
        }

        # Act
        result = get_tag_from_build_info(build_info)

        # Assert
        assert result == (2, 3, 4)

    def test_valid_non_standard_dimrset_tag_returns_version(self) -> None:
        """Test that valid DIMRset tag with non-standard format returns parsed version."""
        # Arrange
        build_info = {
            "tags": {
                "tag": [
                    {"name": "some_other_tag"},
                    {"name": "DIMRset_1.2"},  # Valid but only major.minor
                    {"name": "another_tag"},
                ]
            }
        }

        # Act
        result = get_tag_from_build_info(build_info)

        # Assert
        assert result == (1, 2)

    def test_invalid_dimrset_tag_returns_default(self) -> None:
        """Test that invalid DIMRset tag format returns default tuple."""
        # Arrange
        build_info = {
            "tags": {
                "tag": [
                    {"name": "DIMRset_invalid"},
                    {"name": "DIMRset_a.b.c"},  # Completely invalid
                ]
            }
        }

        # Act
        result = get_tag_from_build_info(build_info)

        # Assert
        assert result == (0, 0, 0)
