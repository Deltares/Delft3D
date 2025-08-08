"""Tests for prepare_email.py."""

from unittest.mock import Mock, patch

from ci_tools.dimrset_delivery.common_utils import (
    parse_version,
)
from ci_tools.dimrset_delivery.dimr_context import DimrAutomationContext
from ci_tools.dimrset_delivery.helpers.result_testbank_parser import ResultTestBankParser
from ci_tools.dimrset_delivery.prepare_email import prepare_email


class TestPrepareEmail:
    """Test cases for prepare_email function."""

    @patch("ci_tools.dimrset_delivery.prepare_email.EmailHelper")
    @patch("ci_tools.dimrset_delivery.prepare_email.get_testbank_result_parser")
    @patch("ci_tools.dimrset_delivery.prepare_email.get_previous_testbank_result_parser")
    def test_prepare_email_success(
        self,
        mock_get_previous_parser: Mock,
        mock_get_parser: Mock,
        mock_email_helper: Mock,
    ) -> None:
        """Test successful email preparation."""
        # Arrange
        mock_context = Mock(spec=DimrAutomationContext)
        mock_context.dry_run = False
        mock_context.get_kernel_versions.return_value = {"kernel1": "1.0.0", "kernel2": "2.0.0"}
        mock_context.get_dimr_version.return_value = "1.2.3"

        mock_current_parser = Mock(spec=ResultTestBankParser)
        mock_previous_parser = Mock(spec=ResultTestBankParser)
        mock_get_parser.return_value = mock_current_parser
        mock_get_previous_parser.return_value = mock_previous_parser

        mock_helper_instance = Mock()
        mock_email_helper.return_value = mock_helper_instance

        # Act
        prepare_email(mock_context)

        # Assert
        mock_context.print_status.assert_called_once_with("Preparing email template...")
        mock_context.get_kernel_versions.assert_called_once()
        mock_context.get_dimr_version.assert_called_once()
        mock_get_parser.assert_called_once()
        mock_get_previous_parser.assert_called_once_with(mock_context)

        mock_email_helper.assert_called_once_with(
            dimr_version="1.2.3",
            kernel_versions={"kernel1": "1.0.0", "kernel2": "2.0.0"},
            current_parser=mock_current_parser,
            previous_parser=mock_previous_parser,
        )
        mock_helper_instance.generate_template.assert_called_once()

    @patch("ci_tools.dimrset_delivery.prepare_email.get_testbank_result_parser")
    @patch("builtins.print")
    def test_prepare_email_dry_run(
        self,
        mock_print: Mock,
        mock_get_parser: Mock,
    ) -> None:
        """Test prepare_email in dry run mode."""
        # Arrange
        mock_context = Mock(spec=DimrAutomationContext)
        mock_context.dry_run = True
        mock_context.get_kernel_versions.return_value = {"kernel1": "1.0.0"}
        mock_context.get_dimr_version.return_value = "1.2.3"

        # Act
        prepare_email(mock_context)

        # Assert
        mock_context.print_status.assert_called_once_with("Preparing email template...")
        mock_context.get_kernel_versions.assert_called_once()
        mock_context.get_dimr_version.assert_called_once()
        mock_get_parser.assert_not_called()

        # Check that print was called with the correct arguments
        mock_print.assert_called_once_with("[DRY-RUN] Would prepare email template for DIMR version:", "1.2.3")


class TestParseVersion:
    """Test cases for parse_version function."""

    def test_valid_dimrset_tag_returns_tuple(self) -> None:
        """Test that valid DIMRset tag returns version tuple."""
        # Act & Assert
        assert parse_version("DIMRset_1.2.3") == (1, 2, 3)
        assert parse_version("DIMRset_10.20.30") == (10, 20, 30)
        assert parse_version("DIMRset_0.1.0") == (0, 1, 0)
        # These are also valid even if not standard semantic versions
        assert parse_version("DIMRset_1.2") == (1, 2)
        assert parse_version("DIMRset_1.2.3.4") == (1, 2, 3, 4)

    def test_invalid_prefix_returns_none(self) -> None:
        """Test that tag without DIMRset prefix returns None."""
        # Act & Assert
        assert parse_version("NotDIMRset_1.2.3") is None
        assert parse_version("1.2.3") is None
        assert parse_version("") is None

    def test_invalid_version_format_returns_none(self) -> None:
        """Test that invalid version format returns None."""
        # Act & Assert
        # Note: "1.2" is actually valid and returns (1, 2)
        # Note: "1.2.3.4" is actually valid and returns (1, 2, 3, 4)
        # Only truly invalid formats return None
        assert parse_version("DIMRset_a.b.c") is None
        assert parse_version("DIMRset_") is None
        assert parse_version("DIMRset_1.") is None
        assert parse_version("DIMRset_.1") is None

    def test_none_input_returns_none(self) -> None:
        """Test that None input returns None."""
        # Act & Assert
        assert parse_version(None) is None

    def test_empty_string_returns_none(self) -> None:
        """Test that empty string returns None."""
        # Act & Assert
        assert parse_version("") is None

    def test_dimrset_prefix_only_returns_none(self) -> None:
        """Test that DIMRset prefix without version returns None."""
        # Act & Assert
        assert parse_version("DIMRset_") is None


class TestIntegration:
    """Integration tests for prepare_email functionality."""

    @patch("ci_tools.dimrset_delivery.prepare_email.EmailHelper")
    @patch("ci_tools.dimrset_delivery.prepare_email.get_testbank_result_parser")
    @patch("ci_tools.dimrset_delivery.prepare_email.get_previous_testbank_result_parser")
    def test_prepare_email_with_no_previous_parser(
        self,
        mock_get_previous_parser: Mock,
        mock_get_parser: Mock,
        mock_email_helper: Mock,
    ) -> None:
        """Test email preparation when no previous parser is available."""
        # Arrange
        mock_context = Mock(spec=DimrAutomationContext)
        mock_context.dry_run = False
        mock_context.get_kernel_versions.return_value = {"kernel1": "1.0.0"}
        mock_context.get_dimr_version.return_value = "1.2.3"

        mock_current_parser = Mock(spec=ResultTestBankParser)
        mock_get_parser.return_value = mock_current_parser
        mock_get_previous_parser.return_value = None  # No previous parser available

        mock_helper_instance = Mock()
        mock_email_helper.return_value = mock_helper_instance

        # Act
        prepare_email(mock_context)

        # Assert
        mock_email_helper.assert_called_once_with(
            dimr_version="1.2.3",
            kernel_versions={"kernel1": "1.0.0"},
            current_parser=mock_current_parser,
            previous_parser=None,
        )
        mock_helper_instance.generate_template.assert_called_once()
