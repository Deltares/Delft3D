"""Tests for common_utils.py."""

from unittest.mock import Mock, mock_open, patch

import pytest

from ci_tools.dimrset_delivery.common_utils import (
    ResultTestBankParser,
    SummaryResults,
    get_previous_testbank_result_parser,
    get_tag_from_build_info,
    get_testbank_result_parser,
)
from ci_tools.dimrset_delivery.dimr_context import DimrAutomationContext
from ci_tools.dimrset_delivery.lib.teamcity import TeamCity
from ci_tools.dimrset_delivery.services import Services
from ci_tools.dimrset_delivery.settings.teamcity_settings import Settings

MOCK_TEST_RESULTS = f"""
Summary: All
{SummaryResults.TOTAL_TESTS.value}   :   42
    {SummaryResults.PASSED.value}    :   40
    {SummaryResults.NOT_PASSED.value}:      2
    Failed    :      2
    {SummaryResults.EXCEPTION.value} :      1
    Ignored   :      0
    Muted     :      0
    {SummaryResults.PERCENTAGE.value}: 95.24
"""


class TestResultTestBankParser:
    @pytest.mark.parametrize(
        ("summary_key", "expected"),
        [
            (SummaryResults.TOTAL_TESTS, "42"),
            (SummaryResults.PASSED, "40"),
            (SummaryResults.NOT_PASSED, "2"),
            (SummaryResults.EXCEPTION, "1"),
            (SummaryResults.PERCENTAGE, "95.24"),
        ],
    )
    def test_get_value(self, summary_key: SummaryResults, expected: str) -> None:
        # Arrange
        self.parser = ResultTestBankParser(MOCK_TEST_RESULTS)

        # Act
        value = self.parser.get_value(summary_key)

        # Assert
        assert value == expected

    def test_get_value_keyerror(self) -> None:
        # Arrange
        self.parser = ResultTestBankParser("wrong file")

        # Act & Assert
        with pytest.raises(KeyError):
            self.parser.get_value(SummaryResults.TOTAL_TESTS)


class TestGetTestResultTestBankParser:
    """Test cases for get_testbank_result_parser function."""

    @patch("ci_tools.dimrset_delivery.common_utils.ResultTestBankParser")
    @patch("ci_tools.dimrset_delivery.common_utils.open", new_callable=mock_open, read_data=b"test data")
    def test_get_testbank_result_parser_reads_file_and_creates_parser(
        self, mock_file: Mock, mock_parser_class: Mock
    ) -> None:
        """Test that get_testbank_result_parser reads file and creates parser correctly."""
        # Arrange
        mock_context = Mock(spec=DimrAutomationContext)
        mock_context.settings = Mock(spec=Settings)
        mock_context.dry_run = False
        mock_context.settings.path_to_release_test_results_artifact = "abc/teamcity_test_results.txt"
        mock_parser_instance = Mock(spec=ResultTestBankParser)
        mock_parser_class.return_value = mock_parser_instance

        # Act
        result = get_testbank_result_parser(mock_context)

        # Assert
        assert result == mock_parser_instance
        mock_file.assert_called_once_with(mock_context.settings.path_to_release_test_results_artifact, "rb")
        mock_parser_class.assert_called_once_with("test data")

    @patch("ci_tools.dimrset_delivery.common_utils.ResultTestBankParser")
    @patch("ci_tools.dimrset_delivery.common_utils.open", new_callable=mock_open, read_data=b"")
    def test_get_testbank_result_parser_with_empty_file(self, mock_file: Mock, mock_parser_class: Mock) -> None:
        """Test get_testbank_result_parser with empty file."""
        # Arrange
        mock_context = Mock(spec=DimrAutomationContext)
        mock_context.settings = Mock(spec=Settings)
        mock_context.dry_run = False
        mock_context.settings.path_to_release_test_results_artifact = "abc/teamcity_test_results.txt"
        mock_parser_instance = Mock(spec=ResultTestBankParser)
        mock_parser_class.return_value = mock_parser_instance

        # Act
        result = get_testbank_result_parser(mock_context)

        # Assert
        assert result == mock_parser_instance
        mock_parser_class.assert_called_once_with("")

    @patch("ci_tools.dimrset_delivery.common_utils.ResultTestBankParser")
    @patch("ci_tools.dimrset_delivery.common_utils.open")
    def test_get_testbank_result_parser_handles_file_not_found(self, mock_file: Mock, mock_parser_class: Mock) -> None:
        """Test that get_testbank_result_parser handles file not found error."""
        # Arrange
        mock_context = Mock(spec=DimrAutomationContext)
        mock_context.settings = Mock(spec=Settings)
        mock_context.dry_run = False
        mock_context.settings.path_to_release_test_results_artifact = "abc/teamcity_test_results.txt"
        mock_file.side_effect = FileNotFoundError("File not found")

        # Act & Assert
        with pytest.raises(FileNotFoundError, match="File not found"):
            get_testbank_result_parser(mock_context)

    @patch("ci_tools.dimrset_delivery.common_utils.ResultTestBankParser")
    @patch("ci_tools.dimrset_delivery.common_utils.open", new_callable=mock_open, read_data=b"\xc3\xa9test data")
    def test_get_testbank_result_parser_handles_unicode_data(self, mock_file: Mock, mock_parser_class: Mock) -> None:
        """Test get_testbank_result_parser with unicode data."""
        # Arrange
        mock_context = Mock(spec=DimrAutomationContext)
        mock_context.settings = Mock(spec=Settings)
        mock_context.dry_run = False
        mock_context.settings.path_to_release_test_results_artifact = "abc/teamcity_test_results.txt"
        mock_parser_instance = Mock(spec=ResultTestBankParser)
        mock_parser_class.return_value = mock_parser_instance

        # Act
        result = get_testbank_result_parser(mock_context)

        # Assert
        assert result == mock_parser_instance
        mock_parser_class.assert_called_once_with("étest data")


class TestGetPreviousTestbankResultParser:
    """Test cases for get_previous_testbank_result_parser function."""

    def test_no_teamcity_client_raises_error(self) -> None:
        """Test that missing TeamCity client raises ValueError."""
        # Arrange
        mock_context = Mock(spec=DimrAutomationContext)
        mock_context.dry_run = False
        mock_services = Mock(spec=Services)
        mock_services.teamcity = None

        # Act & Assert
        with pytest.raises(ValueError, match="TeamCity client is required but not initialized"):
            get_previous_testbank_result_parser(mock_context, mock_services)

    def test_no_current_build_info_returns_none(self) -> None:
        """Test that missing current build info returns None."""
        # Arrange
        mock_context = Mock(spec=DimrAutomationContext)
        mock_context.dry_run = False
        mock_context.build_id = "12345"
        mock_services = Mock(spec=Services)
        mock_services.teamcity = Mock(spec=TeamCity)
        mock_services.teamcity.get_full_build_info_for_build_id.return_value = None

        # Act
        result = get_previous_testbank_result_parser(mock_context, mock_services)

        # Assert
        assert result is None
        mock_services.teamcity.get_full_build_info_for_build_id.assert_called_once_with("12345")

    def test_no_build_type_id_returns_none(self) -> None:
        """Test that missing buildTypeId returns None."""
        # Arrange
        mock_context = Mock(spec=DimrAutomationContext)
        mock_context.dry_run = False
        mock_context.build_id = "12345"
        mock_services = Mock(spec=Services)
        mock_services.teamcity = Mock(spec=TeamCity)
        mock_services.teamcity.get_full_build_info_for_build_id.return_value = {}

        # Act
        result = get_previous_testbank_result_parser(mock_context, mock_services)

        # Assert
        assert result is None

    def test_no_builds_returns_none(self) -> None:
        """Test that no builds available returns None."""
        # Arrange
        mock_context = Mock(spec=DimrAutomationContext)
        mock_context.dry_run = False
        mock_context.build_id = "12345"
        mock_services = Mock(spec=Services)
        mock_services.teamcity = Mock(spec=TeamCity)
        mock_services.teamcity.get_full_build_info_for_build_id.return_value = {
            "buildTypeId": "bt123",
            "tags": {"tag": [{"name": "DIMRset_1.2.3"}]},
        }
        mock_services.teamcity.get_builds_for_build_configuration_id.return_value = None

        # Act
        result = get_previous_testbank_result_parser(mock_context, mock_services)

        # Assert
        assert result is None

    @patch("ci_tools.dimrset_delivery.common_utils.ResultTestBankParser")
    def test_successful_previous_parser_retrieval(self, mock_parser_class: Mock) -> None:
        """Test successful retrieval of previous testbank result parser."""
        # Arrange
        mock_context = Mock(spec=DimrAutomationContext)
        mock_context.dry_run = False
        mock_context.build_id = "12345"
        mock_context.settings = Mock(spec=Settings)
        mock_context.settings.path_to_release_test_results_artifact = "test_results.txt"
        mock_services = Mock(spec=Services)
        mock_services.teamcity = Mock(spec=TeamCity)

        current_build_info = {"buildTypeId": "bt123", "tags": {"tag": [{"name": "DIMRset_1.2.3"}]}}

        builds_response = {
            "build": [
                {"id": 12346},  # Different build
                {"id": 12344},  # Previous build
            ]
        }

        previous_build_info = {"buildTypeId": "bt123", "tags": {"tag": [{"name": "DIMRset_1.2.2"}]}}

        mock_artifact_content = b"artifact content"

        mock_services.teamcity.get_full_build_info_for_build_id.side_effect = [
            current_build_info,  # For current build
            None,  # For first loop build (12346)
            previous_build_info,  # For second loop build (12344)
        ]
        mock_services.teamcity.get_builds_for_build_configuration_id.return_value = builds_response
        mock_services.teamcity.get_build_artifact.return_value = mock_artifact_content

        mock_parser_instance = Mock(spec=ResultTestBankParser)
        mock_parser_class.return_value = mock_parser_instance

        # Act
        result = get_previous_testbank_result_parser(mock_context, mock_services)

        # Assert
        assert result == mock_parser_instance
        mock_parser_class.assert_called_once_with("artifact content")
        mock_services.teamcity.get_build_artifact.assert_called_once()

    def test_no_previous_version_found_returns_none(self) -> None:
        """Test that no previous version found returns None."""
        # Arrange
        mock_context = Mock(spec=DimrAutomationContext)
        mock_context.dry_run = False
        mock_context.build_id = "12345"
        mock_services = Mock(spec=Services)
        mock_services.teamcity = Mock(spec=TeamCity)

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

        mock_services.teamcity.get_full_build_info_for_build_id.side_effect = [
            current_build_info,  # For current build
            other_build_info,  # For other build
        ]
        mock_services.teamcity.get_builds_for_build_configuration_id.return_value = builds_response

        # Act
        result = get_previous_testbank_result_parser(mock_context, mock_services)

        # Assert
        assert result is None

    def test_no_artifact_returns_none(self) -> None:
        """Test that missing artifact returns None."""
        # Arrange
        mock_context = Mock(spec=DimrAutomationContext)
        mock_context.dry_run = False
        mock_context.build_id = "12345"
        mock_context.settings = Mock(spec=Settings)
        mock_context.settings.path_to_release_test_results_artifact = "test_results.txt"
        mock_services = Mock(spec=Services)
        mock_services.teamcity = Mock(spec=TeamCity)

        current_build_info = {"buildTypeId": "bt123", "tags": {"tag": [{"name": "DIMRset_1.2.3"}]}}

        builds_response = {
            "build": [
                {"id": 12344},  # Previous build
            ]
        }

        previous_build_info = {"buildTypeId": "bt123", "tags": {"tag": [{"name": "DIMRset_1.2.2"}]}}

        mock_services.teamcity.get_full_build_info_for_build_id.side_effect = [
            current_build_info,  # For current build
            previous_build_info,  # For previous build
        ]
        mock_services.teamcity.get_builds_for_build_configuration_id.return_value = builds_response
        mock_services.teamcity.get_build_artifact.return_value = None

        # Act
        result = get_previous_testbank_result_parser(mock_context, mock_services)

        # Assert
        assert result is None


class TestGetTagFromBuildInfo:
    """Test cases for get_tag_from_build_info function."""

    def test_no_tags_returns_default(self) -> None:
        """Test that build info without tags returns default tuple."""
        # Arrange
        build_info: dict = {}

        # Act
        result = get_tag_from_build_info(build_info)

        # Assert
        assert result == (0, 0, 0)

    def test_empty_tags_returns_default(self) -> None:
        """Test that empty tags returns default tuple."""
        # Arrange
        build_info: dict = {"tags": {"tag": []}}

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

    def test_multiple_dimrset_tags_returns_first_valid(self) -> None:
        """Test that multiple DIMRset tags returns the first valid one."""
        # Arrange
        build_info = {
            "tags": {"tag": [{"name": "DIMRset_1.0.0"}, {"name": "DIMRset_2.3.4"}, {"name": "some_other_tag"}]}
        }

        # Act
        result = get_tag_from_build_info(build_info)

        # Assert
        assert result == (1, 0, 0)

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
