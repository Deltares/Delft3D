"""Tests for update_excel_sheet.py."""

from unittest.mock import Mock, call, patch

from ci_tools.dimrset_delivery.common_utils import ResultTestBankParser
from ci_tools.dimrset_delivery.dimr_context import DimrAutomationContext
from ci_tools.dimrset_delivery.lib.ssh_client import Direction, SshClient
from ci_tools.dimrset_delivery.lib.teamcity import TeamCity
from ci_tools.dimrset_delivery.services import Services
from ci_tools.dimrset_delivery.settings.teamcity_settings import Settings
from ci_tools.dimrset_delivery.update_excel_sheet import ExcelHelper
from ci_tools.example_utils.logger import LogLevel


class TestUpdateExcelSheet:
    """Test cases for the update_excel_sheet function."""

    def setup_method(self) -> None:
        """Set up test fixtures before each test method."""
        # Arrange - Common test data
        self.mock_context = Mock(spec=DimrAutomationContext)
        self.mock_context.dry_run = False
        self.mock_context.ssh_client = Mock(spec=SshClient)
        self.mock_context.teamcity = Mock(spec=TeamCity)
        self.mock_context.settings = Mock(spec=Settings)
        self.mock_context.settings.path_to_release_test_results_artifact = "path/to/artifact"
        self.mock_context.settings.versions_excel_filename = "path/to/versions.xlsx"
        self.mock_context.settings.dry_run_prefix = "[TEST]"
        self.mock_context.settings.sheet_name = "sheet1"
        self.mock_context.settings.name_column = "column1"

        # Configure context methods
        self.mock_context.kernel_versions = {"build.vcs.number": "12345"}
        self.mock_context.dimr_version = "v1.2.3"

    @patch("ci_tools.dimrset_delivery.update_excel_sheet.get_testbank_result_parser")
    def test_update_excel_sheet_successful_execution(self, mock_get_parser: Mock) -> None:
        """Test successful execution of update_excel_sheet function."""
        # Arrange
        mock_parser = Mock(spec=ResultTestBankParser)
        mock_get_parser.return_value = mock_parser

        mock_services = Mock(spec=Services)
        helper = ExcelHelper(context=self.mock_context, services=mock_services)

        # Act
        helper.execute_step()

        # Assert
        self.mock_context.log.assert_has_calls(
            [call("Updating Excel sheet..."), call("Excel sheet update completed successfully!")]
        )
        assert mock_services.ssh.secure_copy.call_count == 2
        mock_services.ssh.secure_copy.assert_has_calls(
            [
                call("path/to/versions.xlsx", "/p/d-hydro/dimrset/path/to/versions.xlsx", Direction.FROM),
                call("path/to/versions.xlsx", "/p/d-hydro/dimrset/path/to/versions.xlsx", Direction.TO),
            ],
            any_order=False,
        )

    @patch("builtins.print")
    def test_update_excel_sheet_dry_run_mode(self, mock_print: Mock) -> None:
        """Test update_excel_sheet function in dry run mode."""
        # Arrange
        self.mock_context.dry_run = True
        mock_services = Mock(spec=Services)
        helper = ExcelHelper(context=self.mock_context, services=mock_services)

        # Act
        helper.execute_step()

        # Assert
        expected_calls = [
            call("Updating Excel sheet..."),
            call(
                f"Would update Excel sheet with DIMR version: {self.mock_context.dimr_version}",
            ),
            call("Would download Excel from network drive"),
            call("Would append new row with release information"),
            call("Would upload updated Excel back to network drive"),
        ]
        self.mock_context.log.assert_has_calls(expected_calls)

        # Verify no actual operations were performed
        assert (
            not hasattr(self.mock_context.ssh_client, "secure_copy")
            or self.mock_context.ssh_client.secure_copy.call_count == 0
        )

    @patch("ci_tools.dimrset_delivery.update_excel_sheet.get_testbank_result_parser")
    def test_update_excel_sheet_raises_error_when_ssh_client_missing(self, mock_get_parser: Mock) -> None:
        """Test that update_excel_sheet raises ValueError when SSH client is missing."""
        # Arrange
        mock_parser = Mock(spec=ResultTestBankParser)
        mock_get_parser.return_value = mock_parser
        mock_services = Mock(spec=Services)
        mock_services.ssh = None
        helper = ExcelHelper(context=self.mock_context, services=mock_services)

        # Act
        result = helper.execute_step()

        # Assert
        assert result is False
        self.mock_context.log.assert_any_call("SSH client is required but not initialized", severity=LogLevel.ERROR)

    @patch("ci_tools.dimrset_delivery.update_excel_sheet.get_testbank_result_parser")
    def test_update_excel_sheet_raises_error_when_teamcity_missing(self, mock_get_parser: Mock) -> None:
        """Test that update_excel_sheet raises ValueError when TeamCity client is missing."""
        # Arrange
        mock_parser = Mock(spec=ResultTestBankParser)
        mock_get_parser.return_value = mock_parser
        mock_services = Mock(spec=Services)
        mock_services.teamcity = None
        helper = ExcelHelper(context=self.mock_context, services=mock_services)

        # Act & Assert
        result = helper.execute_step()
        assert result is False
        self.mock_context.log.assert_any_call("TeamCity client is required but not initialized")
