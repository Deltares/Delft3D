"""Tests for update_public_wiki.py."""

from unittest.mock import Mock, patch

from ci_tools.dimrset_delivery.dimr_context import DimrAutomationContext
from ci_tools.dimrset_delivery.lib.atlassian import Atlassian
from ci_tools.dimrset_delivery.lib.teamcity import TeamCity
from ci_tools.dimrset_delivery.settings.teamcity_settings import Settings, TeamcityIds
from ci_tools.dimrset_delivery.update_public_wiki import PublicWikiHelper, update_public_wiki


class TestPublicWikiHelper:
    """Test cases for PublicWikiHelper class."""

    def setup_method(self) -> None:
        """Set up mocks for each test."""
        self.mock_context = Mock(spec=DimrAutomationContext)
        self.mock_context.dry_run = False
        self.mock_context.get_dimr_version.return_value = "2.13.03"
        self.mock_context.build_id = "123456"
        self.mock_context.atlassian = Mock(spec=Atlassian)
        self.mock_context.teamcity = Mock(spec=TeamCity)
        self.mock_context.settings = Mock(spec=Settings)
        self.mock_context.settings.dry_run_prefix = "[TEST]"
        self.mock_context.settings.path_to_release_test_results_artifact = "path/to/release_test_results.txt"
        self.mock_context.settings.path_to_windows_version_artifact = "path/to/windows_artifact.txt"
        self.mock_context.settings.path_to_linux_version_artifact = "path/to/linux_artifact.txt"
        self.mock_context.settings.relative_path_to_wiki_template = "relative/path/to/wiki_template.html"
        self.mock_context.settings.dimr_root_page_id = "123456789"
        self.mock_context.settings.dimr_space_id = "987654321"
        self.mock_context.settings.dimr_subpage_prefix = "prefix__"
        self.mock_context.settings.dimr_subpage_suffix = "__suffix"
        self.mock_context.settings.teamcity_ids = Mock(spec=TeamcityIds)
        self.mock_context.settings.teamcity_ids.delft3d_windows_collect_build_type_id = "windows_build_type"
        self.mock_context.settings.teamcity_ids.delft3d_linux_collect_build_type_id = "linux_build_type"
        self.mock_context.settings.dimr_major_page_prefix = "major"
        self.mock_context.settings.dimr_minor_page_prefix = "minor"
        self.mock_context.settings.dimr_patch_page_prefix = "patch"
        self.public_wiki_helper = PublicWikiHelper(self.mock_context)

    def test_init_creates_instance_successfully(self) -> None:
        """Test that PublicWikiHelper can be instantiated successfully."""
        # Arrange & Act
        helper = PublicWikiHelper(self.mock_context)

        # Assert
        assert helper is not None
        assert isinstance(helper, PublicWikiHelper)

    def test_update_public_wiki_executes_successfully(self) -> None:
        """Test that update_public_wiki method executes without errors."""
        # Arrange
        mock_update_main_page = Mock(return_value="main_page_123")
        mock_update_sub_page = Mock()

        # Act
        with (
            patch.object(self.public_wiki_helper, "_PublicWikiHelper__update_main_page", mock_update_main_page),
            patch.object(self.public_wiki_helper, "_PublicWikiHelper__update_sub_page", mock_update_sub_page),
        ):
            self.public_wiki_helper.update_public_wiki()

        # Assert
        mock_update_main_page.assert_called_once()
        mock_update_sub_page.assert_called_once()

    @patch("ci_tools.dimrset_delivery.update_public_wiki.PublicWikiHelper")
    def test_update_public_wiki_success(self, mock_helper_class: Mock) -> None:
        """Test successful execution of update_public_wiki function."""
        # Arrange
        self.mock_context.log = Mock()
        mock_helper_instance = Mock(spec=PublicWikiHelper)
        mock_helper_class.return_value = mock_helper_instance

        # Act
        update_public_wiki(self.mock_context)

        # Assert
        mock_helper_class.assert_called_once_with(context=self.mock_context)
        mock_helper_instance.update_public_wiki.assert_called_once()

    @patch("builtins.print")
    def test_update_public_wiki_dry_run(self, mock_print: Mock) -> None:
        """Test that dry run mode prints a message instead of executing."""
        # Arrange
        self.mock_context.dry_run = True
        self.mock_context.log = Mock()

        # Act
        update_public_wiki(self.mock_context)

        # Assert
        mock_print.assert_called_once_with("[TEST] Would update public wiki for DIMR version:", "2.13.03")
