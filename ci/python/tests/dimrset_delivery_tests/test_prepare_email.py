import os
from pathlib import Path, PosixPath
from typing import Any, Dict, Optional
from unittest.mock import Mock, patch

from ci_tools.dimrset_delivery import prepare_email as pe
from ci_tools.dimrset_delivery.common_utils import ResultTestBankParser, parse_version
from ci_tools.dimrset_delivery.dimr_context import DimrAutomationContext
from ci_tools.dimrset_delivery.prepare_email import EmailHelper, prepare_email
from ci_tools.dimrset_delivery.settings.teamcity_settings import Settings


class DummyParser:
    def __init__(
        self, passing: str = "95", total: str = "100", passing_count: str = "95", exceptions: str = "0"
    ) -> None:
        self._passing = passing
        self._total = total
        self._passing_count = passing_count
        self._exceptions = exceptions

    def get_percentage_total_passing(self) -> str:
        return self._passing

    def get_total_tests(self) -> str:
        return self._total

    def get_total_passing(self) -> str:
        return self._passing_count

    def get_total_exceptions(self) -> str:
        return self._exceptions


class TestEmailHelper:
    """Unit tests for EmailHelper class methods."""

    def make_helper(
        self,
        dimr_version: str = "1.2.3",
        kernel_versions: Optional[Dict[str, str]] = None,
        passing: str = "95",
        exceptions: str = "0",
        prev_passing: Optional[str] = None,
        prev_exceptions: Optional[str] = None,
        template_path: Path = Path("/dump/location"),
        lower_bound: str = "100",
    ) -> EmailHelper:
        mock_context = Mock(spec=DimrAutomationContext)
        mock_context.get_dimr_version.return_value = dimr_version
        mock_context.settings = Mock(spec=Settings)
        mock_context.settings.relative_path_to_email_template = template_path.name
        mock_context.settings.relative_path_to_output_folder = "output/"
        mock_context.settings.lower_bound_percentage_successful_tests = lower_bound

        if kernel_versions is None:
            kernel_versions = {"kernelA": "123", "kernelB": "456"}
        parser = DummyParser(passing=passing, exceptions=exceptions)
        prev_parser = (
            DummyParser(passing=prev_passing or passing, exceptions=prev_exceptions or exceptions)
            if prev_passing is not None
            else None
        )
        return EmailHelper(mock_context, kernel_versions, parser, prev_parser)  # type: ignore

    def test_generate_template_calls_all(self) -> None:
        helper = self.make_helper()
        # Just check that the public method runs without error
        with (
            patch.object(helper, "_EmailHelper__load_template") as mock_load,
            patch.object(helper, "_EmailHelper__insert_summary_table_header") as mock_header,
            patch.object(helper, "_EmailHelper__insert_summary_table") as mock_table,
            patch.object(helper, "_EmailHelper__save_template") as mock_save,
        ):
            helper.generate_template()

            # Verify all internal methods were called
            mock_load.assert_called_once()
            mock_header.assert_called_once()
            mock_table.assert_called_once()
            mock_save.assert_called_once()

    def test_load_and_save_template(self, tmp_path: Path) -> None:
        # Integration-style: test that generate_template creates an output file
        template_content = "Hello @@@DIMR_VERSION@@@ @@@LINK_TO_PUBLIC_WIKI@@@ @@@SUMMARY_TABLE_BODY@@@"
        template_path = tmp_path / "template.html"
        template_path.write_text(template_content)
        helper = self.make_helper(template_path=template_path)

        # Create the expected output directory structure
        output_dir = tmp_path / "output"
        output_dir.mkdir()

        with patch.object(os.path, "dirname", lambda _: str(tmp_path)):
            helper.generate_template()
        # Find the output file by pattern in the output subdirectory
        out_files = list(output_dir.glob("DIMRset_*.html"))
        assert out_files, "No output file generated"
        content = out_files[0].read_text()
        assert "Hello" in content

    def test_generate_wiki_link(self) -> None:
        """Test wiki link generation for different version formats."""
        helper = self.make_helper(dimr_version="1.2.3")
        # Access the private method using name mangling
        wiki_link = helper._EmailHelper__generate_wiki_link()  # type: ignore

        expected_url = "https://publicwiki.deltares.nl/display/PROJ/DIMRset+release+1.2.3"
        expected_link = f'<a href="{expected_url}">{expected_url}</a>'
        assert wiki_link == expected_link

    def test_generate_wiki_link_with_different_versions(self) -> None:
        """Test wiki link generation with different version formats."""
        test_cases = ["2025.01", "10.20.30", "1.0.0", "2.29.03"]

        for version in test_cases:
            helper = self.make_helper(dimr_version=version)
            wiki_link = helper._EmailHelper__generate_wiki_link()  # type: ignore

            expected_url = f"https://publicwiki.deltares.nl/display/PROJ/DIMRset+release+{version}"
            expected_link = f'<a href="{expected_url}">{expected_url}</a>'
            assert wiki_link == expected_link

    @patch("ci_tools.dimrset_delivery.prepare_email.KERNELS", [])
    def test_get_email_friendly_kernel_name_empty_kernels(self) -> None:
        """Test kernel name mapping with empty KERNELS list."""
        helper = self.make_helper()
        result = helper._EmailHelper__get_email_friendly_kernel_name("unknown_kernel")  # type: ignore
        assert result == ""

    @patch("ci_tools.dimrset_delivery.prepare_email.KERNELS")
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

    @patch("ci_tools.dimrset_delivery.prepare_email.KERNELS")
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
        helper = self.make_helper(
            dimr_version="1.2.3",
            kernel_versions={"kernel1": "1.0.0"},
            passing="95",
            exceptions="0",
            prev_passing="92",
            prev_exceptions="1",
            lower_bound="90",
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
        helper._EmailHelper__template = "Version: @@@DIMR_VERSION@@@ Link: @@@LINK_TO_PUBLIC_WIKI@@@"  # type: ignore

        helper._EmailHelper__insert_summary_table_header()  # type: ignore

        expected_link = '<a href="https://publicwiki.deltares.nl/display/PROJ/DIMRset+release+2.29.03">https://publicwiki.deltares.nl/display/PROJ/DIMRset+release+2.29.03</a>'
        expected_template = f"Version: 2.29.03 Link: {expected_link}"

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

    @patch("ci_tools.dimrset_delivery.prepare_email.get_testbank_result_parser")
    @patch("ci_tools.dimrset_delivery.prepare_email.get_previous_testbank_result_parser")
    @patch("ci_tools.dimrset_delivery.prepare_email.EmailHelper")
    def test_prepare_email_with_no_previous_parser(
        self,
        mock_email_helper: Mock,
        mock_get_previous_parser: Mock,
        mock_get_parser: Mock,
    ) -> None:
        """Test email preparation when no previous parser is available."""
        # Arrange
        mock_context = Mock(spec=DimrAutomationContext)
        mock_context.dry_run = False
        mock_context.get_kernel_versions.return_value = {"kernel1": "1.0.0"}
        mock_context.get_dimr_version.return_value = "1.2.3"
        mock_context.settings = Mock(spec=Settings)
        mock_context.settings.path_to_release_test_results_artifact = "\random-location"

        mock_current_parser = Mock(spec=ResultTestBankParser)
        mock_get_parser.return_value = mock_current_parser
        mock_get_previous_parser.return_value = None  # No previous parser available

        mock_helper_instance = Mock()
        mock_email_helper.return_value = mock_helper_instance

        # Act
        prepare_email(mock_context)

        # Assert
        mock_email_helper.assert_called_once_with(
            context=mock_context,
            kernel_versions={"kernel1": "1.0.0"},
            current_parser=mock_current_parser,
            previous_parser=None,
        )
        mock_helper_instance.generate_template.assert_called_once()
