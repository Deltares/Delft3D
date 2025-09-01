import sys
from unittest.mock import MagicMock, Mock, patch

from ci_tools.dimrset_delivery.dimr_context import Credentials, DimrAutomationContext
from ci_tools.dimrset_delivery.lib.teamcity import TeamCity
from ci_tools.dimrset_delivery.settings.teamcity_settings import Settings


class TestTeamCity:
    """Unit tests for TeamCity class methods."""

    def setup_method(self) -> None:
        self.mock_context = Mock(spec=DimrAutomationContext)
        self.mock_context.settings = Mock(spec=Settings)

    def test_test_connection_success(self) -> None:
        self.mock_context.dry_run = False
        tc = TeamCity(credentials=Credentials("user", "pass"), context=self.mock_context)
        with patch("ci_tools.dimrset_delivery.lib.teamcity.requests.get") as mock_get:
            mock_get.return_value = Mock(status_code=200, content=b"ok")
            assert tc.test_connection() is True

    def test_test_connection_fail(self) -> None:
        self.mock_context.dry_run = False
        tc = TeamCity(credentials=Credentials("user", "pass"), context=self.mock_context)
        with patch("ci_tools.dimrset_delivery.lib.teamcity.requests.get") as mock_get:
            mock_get.return_value = Mock(status_code=401, content=b"fail")
            assert tc.test_connection() is False

    def test_test_connection_dry_run(self) -> None:
        self.mock_context.dry_run = True
        self.mock_context.settings.dry_run_prefix = "[TEST]"
        tc = TeamCity(credentials=Credentials("user", "pass"), context=self.mock_context)
        assert tc.test_connection() is True

    def test_get_builds_for_build_configuration_id_success(self) -> None:
        tc = TeamCity(credentials=Credentials("user", "pass"), context=self.mock_context)
        with patch("ci_tools.dimrset_delivery.lib.teamcity.requests.get") as mock_get:
            mock_get.return_value = Mock(status_code=200, json=lambda: {"build": [1, 2, 3]})
            result = tc.get_builds_for_build_configuration_id("buildtype", limit=2, include_failed_builds=True)
            assert result == {"build": [1, 2, 3]}

    def test_get_builds_for_build_configuration_id_fail(self) -> None:
        tc = TeamCity(credentials=Credentials("user", "pass"), context=self.mock_context)
        with (
            patch("ci_tools.dimrset_delivery.lib.teamcity.requests.get") as mock_get,
            patch.object(sys, "exit") as mock_exit,
        ):
            mock_get.return_value = Mock(status_code=404, content=b"fail")
            tc.get_builds_for_build_configuration_id("buildtype", limit=2, include_failed_builds=True)
            mock_exit.assert_called_once()

    def test_get_build_info_for_build_id_success(self) -> None:
        tc = TeamCity(credentials=Credentials("user", "pass"), context=self.mock_context)
        with patch("ci_tools.dimrset_delivery.lib.teamcity.requests.get") as mock_get:
            mock_get.return_value = Mock(status_code=200, json=lambda: {"id": 123})
            result = tc.get_build_info_for_build_id("123")
            assert result == {"id": 123}

    def test_get_build_info_for_build_id_fail(self) -> None:
        tc = TeamCity(credentials=Credentials("user", "pass"), context=self.mock_context)
        with (
            patch("ci_tools.dimrset_delivery.lib.teamcity.requests.get") as mock_get,
            patch.object(sys, "exit") as mock_exit,
        ):
            mock_get.return_value = Mock(status_code=404, content=b"fail")
            tc.get_build_info_for_build_id("123")
            mock_exit.assert_called_once()

    def test_get_full_build_info_for_build_id_success(self) -> None:
        tc = TeamCity(credentials=Credentials("user", "pass"), context=self.mock_context)
        with patch("ci_tools.dimrset_delivery.lib.teamcity.requests.get") as mock_get:
            mock_get.return_value = Mock(status_code=200, json=lambda: {"id": 123})
            result = tc.get_full_build_info_for_build_id("123")
            assert result == {"id": 123}

    def test_get_full_build_info_for_build_id_fail(self) -> None:
        tc = TeamCity(credentials=Credentials("user", "pass"), context=self.mock_context)
        with (
            patch("ci_tools.dimrset_delivery.lib.teamcity.requests.get") as mock_get,
        ):
            mock_get.return_value = Mock(status_code=404, content=b"fail")
            result = tc.get_full_build_info_for_build_id("123")
            assert result is None

    def test_get_build_artifact_names_success(self) -> None:
        tc = TeamCity(credentials=Credentials("user", "pass"), context=self.mock_context)
        with patch("ci_tools.dimrset_delivery.lib.teamcity.requests.get") as mock_get:
            mock_get.return_value = Mock(status_code=200, json=lambda: {"files": ["a", "b"]})
            with patch.object(tc, "_TeamCity__get_put_request_headers", return_value={}):
                result = tc.get_build_artifact_names("123")
                assert result == {"files": ["a", "b"]}

    def test_get_build_artifact_names_fail(self) -> None:
        tc = TeamCity(credentials=Credentials("user", "pass"), context=self.mock_context)
        with (
            patch("ci_tools.dimrset_delivery.lib.teamcity.requests.get") as mock_get,
            patch.object(tc, "_TeamCity__get_put_request_headers", return_value={}),
            patch.object(sys, "exit") as mock_exit,
        ):
            mock_get.return_value = Mock(status_code=404, content=b"fail")
            tc.get_build_artifact_names("123")
            mock_exit.assert_called_once()

    def test_get_build_artifact_success(self) -> None:
        tc = TeamCity(credentials=Credentials("user", "pass"), context=self.mock_context)
        with patch("ci_tools.dimrset_delivery.lib.teamcity.requests.get") as mock_get:
            mock_get.return_value = Mock(status_code=200, content=b"artifact")
            result = tc.get_build_artifact("123", "path/to/artifact")
            assert result == b"artifact"

    def test_get_build_artifact_fail(self) -> None:
        tc = TeamCity(credentials=Credentials("user", "pass"), context=self.mock_context)
        with (
            patch("ci_tools.dimrset_delivery.lib.teamcity.requests.get") as mock_get,
            patch.object(sys, "exit") as mock_exit,
        ):
            mock_get.return_value = Mock(status_code=404, content=b"fail")
            tc.get_build_artifact("123", "path/to/artifact")
            mock_exit.assert_called_once()

    def test_pin_build_success(self) -> None:
        tc = TeamCity(credentials=Credentials("user", "pass"), context=self.mock_context)
        mock_csrf = Mock()
        mock_csrf.status_code = 200
        mock_csrf.content = b"csrf-token"
        mock_response = Mock()
        mock_response.status_code = 200
        mock_response.content = b"ok"
        with patch("ci_tools.dimrset_delivery.lib.teamcity.requests.get", return_value=mock_csrf):
            with patch("ci_tools.dimrset_delivery.lib.teamcity.requests.put", return_value=mock_response):
                assert tc.pin_build("123") is True

    def test_pin_build_fail(self) -> None:
        tc = TeamCity(credentials=Credentials("user", "pass"), context=self.mock_context)
        mock_csrf = Mock()
        mock_csrf.status_code = 200
        mock_csrf.content = b"csrf-token"

        mock_response = MagicMock()
        mock_response.status_code = 401
        mock_response.content = b"fail"
        mock_response.__bool__.return_value = False
        with patch("ci_tools.dimrset_delivery.lib.teamcity.requests.get", return_value=mock_csrf):
            with patch("ci_tools.dimrset_delivery.lib.teamcity.requests.put", return_value=mock_response):
                tc.pin_build("123")
                calls = [call.args for call in self.mock_context.log.call_args_list]
                assert calls == [("Could not pin build with build id 123:",), ("401 - fail",)]

    def test_get_build_test_results_from_teamcity_dry_run_mode(self) -> None:
        """Test get_build_test_results_from_teamcity in dry run mode."""
        # Arrange
        self.mock_context.dry_run = True
        self.mock_context.settings.dry_run_prefix = "[TEST]"
        tc = TeamCity(credentials=Credentials("user", "pass"), context=self.mock_context)

        # Act
        result = tc.get_build_test_results_from_teamcity("12345")

        # Assert
        assert result is not None
        assert result.name == f"{self.mock_context.settings.dry_run_prefix} Test Configuration / Build 12345"
        assert result.build_nr == "12345"
        assert result.test_result.passed == 85
        assert result.test_result.failed == 0
        assert result.test_result.ignored == 0
        assert result.test_result.muted == 0
        assert result.status_text == f"{self.mock_context.settings.dry_run_prefix} SUCCESS"

    def test_get_build_test_results_from_teamcity_no_build_info(self) -> None:
        """Test get_build_test_results_from_teamcity when no build info is returned."""
        # Arrange
        self.mock_context.dry_run = False
        tc = TeamCity(credentials=Credentials("user", "pass"), context=self.mock_context)

        # Act
        result = tc.get_build_test_results_from_teamcity("12345")

        # Assert
        assert result is None

    def test_get_build_test_results_from_teamcity_no_test_occurrences(self) -> None:
        """Test get_build_test_results_from_teamcity when no test occurrences exist."""
        # Arrange
        self.mock_context.dry_run = False
        tc = TeamCity(credentials=Credentials("user", "pass"), context=self.mock_context)

        build_info = {
            "number": "2023.01.123",
            "status": "SUCCESS",
            "buildType": {"name": "Windows Integration Tests", "projectName": "Delft3D"},
        }
        tc.get_full_build_info_for_build_id = Mock(return_value=build_info)

        # Act
        result = tc.get_build_test_results_from_teamcity("12345")

        # Assert
        assert result is None

    def test_get_build_test_results_from_teamcity_zero_test_count(self) -> None:
        """Test get_build_test_results_from_teamcity when test count is zero."""
        # Arrange
        self.mock_context.dry_run = False
        tc = TeamCity(credentials=Credentials("user", "pass"), context=self.mock_context)

        build_info = {
            "number": "2023.01.123",
            "status": "SUCCESS",
            "buildType": {"name": "Windows Integration Tests", "projectName": "Delft3D"},
            "testOccurrences": {"count": "0"},
        }
        tc.get_full_build_info_for_build_id = Mock(return_value=build_info)

        # Act
        result = tc.get_build_test_results_from_teamcity("12345")

        # Assert
        assert result is None

    def test_get_build_test_results_from_teamcity_valid_test_results(self) -> None:
        """Test get_build_test_results_from_teamcity with valid test results."""
        # Arrange
        self.mock_context.dry_run = False
        tc = TeamCity(credentials=Credentials("user", "pass"), context=self.mock_context)

        build_info = {
            "number": "1.23.34",
            "status": "SUCCESS",
            "buildType": {"name": "Windows Integration Tests", "projectName": "Delft3D"},
            "testOccurrences": {"count": "100", "passed": "85", "failed": "10", "ignored": "3", "muted": "2"},
        }
        tc.get_full_build_info_for_build_id = Mock(return_value=build_info)

        # Act
        result = tc.get_build_test_results_from_teamcity("12345")

        # Assert
        assert result is not None
        assert result.name == "Delft3D / Windows Integration Tests"
        assert result.build_nr == "1.23.34"
        assert result.status_text == "SUCCESS"
        assert result.test_result.passed == 85
        assert result.test_result.failed == 10
        assert result.test_result.ignored == 3
        assert result.test_result.muted == 2
        assert result.test_result.exception == 0
        assert result.test_result.muted_exception == 0
