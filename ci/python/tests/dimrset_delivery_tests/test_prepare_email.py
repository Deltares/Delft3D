import os
from pathlib import Path
from typing import Dict, Optional
from unittest.mock import Mock, patch

from ci_tools.dimrset_delivery.common_utils import ResultTestBankParser, SummaryResults, parse_version
from ci_tools.dimrset_delivery.dimr_context import DimrAutomationContext
from ci_tools.dimrset_delivery.services import Services
from ci_tools.dimrset_delivery.settings.teamcity_settings import Settings
from ci_tools.dimrset_delivery.step_5_prepare_email import EmailHelper


class TestEmailHelper:
    """Unit tests for EmailHelper class methods."""

    def make_helper(
        self,
        dimr_version: str = "1.2.3",
        kernel_versions: Optional[Dict[str, str]] = None,
        template_path: Path = Path("/dump/location"),
        lower_bound: str = "100",
        passing: str = "95",
        exceptions: str = "0",
    ) -> EmailHelper:
        mock_context = Mock(spec=DimrAutomationContext)
        mock_context.dry_run = False
        mock_context.build_id = "123456789"
        mock_context.dimr_version = dimr_version
        mock_context.settings = Mock(spec=Settings)
        mock_context.settings.relative_path_to_email_template = template_path.name
        mock_context.settings.relative_path_to_output_folder = "output/"
        mock_context.settings.lower_bound_percentage_successful_tests = lower_bound
        mock_context.settings.path_to_release_test_results_artifact = "test_results.txt"
        if kernel_versions is None:
            kernel_versions = {"kernelA": "123", "kernelB": "456"}
        mock_context.kernel_versions = kernel_versions
        mock_services = Mock(spec=Services)
        mock_services.teamcity.get_full_build_info_for_build_id.return_value = {"tags": {"tag": ["DIMRset_1.2.3"]}}

        # Configure the parser mock with realistic return values
        mock_parser = Mock(spec=ResultTestBankParser)
        mock_parser.get_value.side_effect = lambda key: {
            SummaryResults.PERCENTAGE: passing,
            SummaryResults.TOTAL_TESTS: "100",
            SummaryResults.PASSED: passing,
            SummaryResults.EXCEPTION: exceptions,
        }.get(key)

        with patch(
            "ci_tools.dimrset_delivery.step_5_prepare_email.get_testbank_result_parser", return_value=mock_parser
        ):
            return EmailHelper(mock_context, mock_services)  # type: ignore

    def test_generate_template_calls_all(self) -> None:
        helper = self.make_helper()
        # Just check that the public method runs without error
        with (
            patch.object(helper, "_EmailHelper__load_template") as mock_load,
            patch.object(helper, "_EmailHelper__insert_summary_table_header") as mock_header,
            patch.object(helper, "_EmailHelper__insert_summary_table") as mock_table,
            patch.object(helper, "_EmailHelper__save_template") as mock_save,
        ):
            helper.execute_step()

            # Verify all internal methods were called
            mock_load.assert_called_once()
            mock_header.assert_called_once()
            mock_table.assert_called_once()
            mock_save.assert_called_once()

    def test_load_and_save_template(self, tmp_path: Path) -> None:
        # Integration-style: test that generate_template creates an output file
        template_content = "Hello @@@DIMR_VERSION@@@ @@@SUMMARY_TABLE_BODY@@@"
        template_path = tmp_path / "template.html"
        template_path.write_text(template_content)
        helper = self.make_helper(template_path=template_path)

        # Create the expected output directory structure
        output_dir = tmp_path / "output"
        output_dir.mkdir()

        with patch.object(os.path, "dirname", lambda _: str(tmp_path)):
            helper.execute_step()
        # Find the output file by pattern in the output subdirectory
        out_files = list(output_dir.glob("DIMRset_*.html"))
        assert out_files, "No output file generated"
        content = out_files[0].read_text()
        assert "Hello" in content

    @patch("ci_tools.dimrset_delivery.step_5_prepare_email.KERNELS", [])
    def test_get_email_friendly_kernel_name_empty_kernels(self) -> None:
        """Test kernel name mapping with empty KERNELS list."""
        helper = self.make_helper()
        result = helper._EmailHelper__get_email_friendly_kernel_name("unknown_kernel")  # type: ignore
        assert result == ""

    @patch("ci_tools.dimrset_delivery.step_5_prepare_email.KERNELS")
    def test_get_email_friendly_kernel_name_found(self, mock_kernels: Mock) -> None:
        """Test kernel name mapping when kernel is found."""
        # Mock kernel config object
        mock_kernel = Mock()
        mock_kernel.name_for_extracting_revision = "kernel_internal"
        mock_kernel.name_for_email = "Kernel Display Name"
        mock_kernels.__iter__.return_value = [mock_kernel]

        helper = self.make_helper()
        result = helper._EmailHelper__get_email_friendly_kernel_name("kernel_internal")  # type: ignore
        assert result == "Kernel Display Name"

    @patch("ci_tools.dimrset_delivery.step_5_prepare_email.KERNELS")
    def test_get_email_friendly_kernel_name_not_found(self, mock_kernels: Mock) -> None:
        """Test kernel name mapping when kernel is not found."""
        # Mock kernel config object that doesn't match
        mock_kernel = Mock()
        mock_kernel.name_for_extracting_revision = "other_kernel"
        mock_kernel.name_for_email = "Other Kernel"
        mock_kernels.__iter__.return_value = [mock_kernel]

        helper = self.make_helper()
        result = helper._EmailHelper__get_email_friendly_kernel_name("unknown_kernel")  # type: ignore
        assert result == ""

    def test_generate_summary_table_html_no_previous_parser(self) -> None:
        """Test HTML table generation without previous parser."""
        kernel_versions = {"kernel1": "1.0.0", "kernel2": "2.0.0", "DIMRset_ver": "should_be_skipped"}
        helper = self.make_helper(
            dimr_version="1.2.3", kernel_versions=kernel_versions, passing="95", exceptions="0", lower_bound="100"
        )

        with patch.object(helper, "_EmailHelper__get_email_friendly_kernel_name", side_effect=lambda k: f"Display_{k}"):
            html = helper._EmailHelper__generate_summary_table_html()  # type: ignore

        # Check that DIMRset_ver is skipped
        assert "DIMRset_ver" not in html
        assert "should_be_skipped" not in html

        # Check kernel information is included
        assert "Display_kernel1" in html
        assert "1.0.0" in html
        assert "Display_kernel2" in html
        assert "2.0.0" in html

        # Check test results (95% < 100% threshold, so should be fail class)
        assert '<span class="fail">95%</span>' in html
        assert "Total tests: 100" in html
        assert "Passed&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;: 95" in html

        # Check no previous data message
        assert "no previous data" in html

        # Check exceptions (0 should be success class)
        assert '<span class="success">0</span>' in html

    def test_generate_summary_table_html_with_previous_parser_success(self) -> None:
        """Test HTML table generation with previous parser - success case."""
        # Setup current parser values
        passing = "95"
        exceptions = "0"
        lower_bound = "90"
        kernel_versions = {"kernel1": "1.0.0"}

        # Setup previous parser mock
        mock_prev_parser = Mock(spec=ResultTestBankParser)
        mock_prev_parser.get_value.side_effect = lambda key: {
            SummaryResults.PERCENTAGE: "92",
            SummaryResults.TOTAL_TESTS: "100",
            SummaryResults.PASSED: "95",
            SummaryResults.EXCEPTION: "1",
        }.get(key)

        with patch(
            "ci_tools.dimrset_delivery.step_5_prepare_email.get_previous_testbank_result_parser",
            return_value=mock_prev_parser,
        ):
            helper = self.make_helper(
                dimr_version="1.2.3",
                kernel_versions=kernel_versions,
                lower_bound=lower_bound,
                passing=passing,
                exceptions=exceptions,
            )

            with patch.object(helper, "_EmailHelper__get_email_friendly_kernel_name", return_value="Kernel1"):
                html = helper._EmailHelper__generate_summary_table_html()  # type: ignore

            # Check test results (95% > 90% threshold, so should be success class)
            assert '<span class="success">95%</span>' in html

            # Check previous results (92% > 90% threshold, so should be success class)
            assert 'Green testbank was (<span class="success">92%</span>)' in html

            # Check comparative data
            assert "Total tests: 100 was (100)" in html
            assert "Passed&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;: 95 was (95)" in html

            # Check exceptions (0 should be success, 1 should be fail)
            assert '<span class="success">0</span>' in html
            assert 'was <span class="fail">1</span>' in html

    def test_insert_summary_table_header(self) -> None:
        """Test summary table header insertion."""
        helper = self.make_helper(dimr_version="2.29.03")
        helper._EmailHelper__template = "Version: @@@DIMR_VERSION@@@"  # type: ignore

        helper._EmailHelper__insert_summary_table_header()  # type: ignore
        expected_template = "Version: 2.29.03"

        assert helper._EmailHelper__template == expected_template  # type: ignore

    def test_insert_summary_table(self) -> None:
        """Test summary table insertion."""
        helper = self.make_helper()
        helper._EmailHelper__template = "Table: @@@SUMMARY_TABLE_BODY@@@"  # type: ignore

        with patch.object(helper, "_EmailHelper__generate_summary_table_html", return_value="<table>content</table>"):
            helper._EmailHelper__insert_summary_table()  # type: ignore

        assert helper._EmailHelper__template == "Table: <table>content</table>"  # type: ignore


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
        assert parse_version("DIMRset_") is None
        assert parse_version("NotDIMRset_1.2.3") is None
        assert parse_version("1.2.3") is None
        assert parse_version(None) is None
        assert parse_version("") is None


class TestIntegration:
    """Integration tests for prepare_email functionality."""

    # @patch("ci_tools.dimrset_delivery.step_5_prepare_email.EmailHelper")
    @patch("ci_tools.dimrset_delivery.step_5_prepare_email.get_testbank_result_parser")
    def test_prepare_email_with_no_previous_parser(self, mock_get_parser: Mock) -> None:
        """Test email preparation when no previous parser is available."""
        # Arrange
        mock_current_parser = Mock(spec=ResultTestBankParser)
        mock_current_parser.get_value.side_effect = lambda key: {
            SummaryResults.PERCENTAGE: "95",
            SummaryResults.TOTAL_TESTS: "100",
            SummaryResults.PASSED: "95",
            SummaryResults.EXCEPTION: "0",
        }.get(key)
        mock_get_parser.return_value = mock_current_parser

        mock_context = Mock(spec=DimrAutomationContext)
        mock_context.dry_run = False
        mock_context.kernel_versions = {"kernel1": "1.0.0"}
        mock_context.dimr_version = "1.2.3"
        mock_context.build_id = "12345"
        mock_context.settings = Mock(spec=Settings)
        mock_context.settings.path_to_release_test_results_artifact = "abc/teamcity_test_results.txt"
        mock_context.settings.relative_path_to_email_template = "abc/template.html"
        mock_context.settings.lower_bound_percentage_successful_tests = "100"
        mock_context.settings.relative_path_to_output_folder = "output/"

        mock_services = Mock(spec=Services)
        mock_build_info = {"tags": {"tag": []}}  # or provide a list of tags if needed
        mock_services.teamcity.get_full_build_info_for_build_id.return_value = mock_build_info

        helper = EmailHelper(mock_context, mock_services)

        # Act
        def fake_load_template() -> None:
            helper._EmailHelper__template = "kernel1 @@@SUMMARY_TABLE_BODY@@@"  # minimal template for test

        with patch.object(helper, "_EmailHelper__load_template", side_effect=fake_load_template):
            helper.execute_step()

        # Assert
        template = helper._EmailHelper__template  # type: ignore

        assert "kernel1" in template
        assert "1.0.0" in template
        assert '<span class="fail">95%</span>' in template  # 95 < 100 threshold
        assert "Total tests: 100" in template
        assert "Passed&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;: 95" in template
        assert "no previous data" in template
        assert '<span class="success">0</span>' in template  # exceptions
