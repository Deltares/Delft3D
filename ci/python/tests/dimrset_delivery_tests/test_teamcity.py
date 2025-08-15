import sys
from unittest.mock import Mock, patch

from ci_tools.dimrset_delivery.lib.teamcity import TeamCity
from ci_tools.dimrset_delivery.settings.teamcity_settings import Settings


class TestTeamCity:
    """Unit tests for TeamCity class methods."""

    def setup_method(self) -> None:
        self.mock_settings = Mock(spec=Settings)

    def test_test_api_connection_success(self) -> None:
        tc = TeamCity("user", "pass", settings=self.mock_settings)
        with patch("ci_tools.dimrset_delivery.lib.teamcity.requests.get") as mock_get:
            mock_get.return_value = Mock(status_code=200, content=b"ok")
            assert tc.test_api_connection(dry_run=False) is True

    def test_test_api_connection_fail(self) -> None:
        tc = TeamCity("user", "pass", settings=self.mock_settings)
        with patch("ci_tools.dimrset_delivery.lib.teamcity.requests.get") as mock_get:
            mock_get.return_value = Mock(status_code=401, content=b"fail")
            assert tc.test_api_connection(dry_run=False) is False

    def test_test_api_connection_dry_run(self) -> None:
        self.mock_settings.dry_run_prefix = "[TEST]"
        tc = TeamCity("user", "pass", settings=self.mock_settings)
        assert tc.test_api_connection(dry_run=True) is True

    def test_get_builds_for_build_configuration_id_success(self) -> None:
        tc = TeamCity("user", "pass", settings=self.mock_settings)
        with patch("ci_tools.dimrset_delivery.lib.teamcity.requests.get") as mock_get:
            mock_get.return_value = Mock(status_code=200, json=lambda: {"build": [1, 2, 3]})
            result = tc.get_builds_for_build_configuration_id("buildtype", limit=2, include_failed_builds=True)
            assert result == {"build": [1, 2, 3]}

    def test_get_builds_for_build_configuration_id_fail(self) -> None:
        tc = TeamCity("user", "pass", settings=self.mock_settings)
        with (
            patch("ci_tools.dimrset_delivery.lib.teamcity.requests.get") as mock_get,
            patch.object(sys, "exit") as mock_exit,
        ):
            mock_get.return_value = Mock(status_code=404, content=b"fail")
            tc.get_builds_for_build_configuration_id("buildtype", limit=2, include_failed_builds=True)
            mock_exit.assert_called_once()

    def test_get_build_info_for_build_id_success(self) -> None:
        tc = TeamCity("user", "pass", settings=self.mock_settings)
        with patch("ci_tools.dimrset_delivery.lib.teamcity.requests.get") as mock_get:
            mock_get.return_value = Mock(status_code=200, json=lambda: {"id": 123})
            result = tc.get_build_info_for_build_id("123")
            assert result == {"id": 123}

    def test_get_build_info_for_build_id_fail(self) -> None:
        tc = TeamCity("user", "pass", settings=self.mock_settings)
        with (
            patch("ci_tools.dimrset_delivery.lib.teamcity.requests.get") as mock_get,
            patch.object(sys, "exit") as mock_exit,
        ):
            mock_get.return_value = Mock(status_code=404, content=b"fail")
            tc.get_build_info_for_build_id("123")
            mock_exit.assert_called_once()

    def test_get_full_build_info_for_build_id_success(self) -> None:
        tc = TeamCity("user", "pass", settings=self.mock_settings)
        with patch("ci_tools.dimrset_delivery.lib.teamcity.requests.get") as mock_get:
            mock_get.return_value = Mock(status_code=200, json=lambda: {"id": 123})
            result = tc.get_full_build_info_for_build_id("123")
            assert result == {"id": 123}

    def test_get_full_build_info_for_build_id_fail(self) -> None:
        tc = TeamCity("user", "pass", settings=self.mock_settings)
        with (
            patch("ci_tools.dimrset_delivery.lib.teamcity.requests.get") as mock_get,
            patch.object(sys, "exit") as mock_exit,
        ):
            mock_get.return_value = Mock(status_code=404, content=b"fail")
            tc.get_full_build_info_for_build_id("123")
            mock_exit.assert_called_once()

    def test_get_build_artifact_names_success(self) -> None:
        tc = TeamCity("user", "pass", settings=self.mock_settings)
        with patch("ci_tools.dimrset_delivery.lib.teamcity.requests.get") as mock_get:
            mock_get.return_value = Mock(status_code=200, json=lambda: {"files": ["a", "b"]})
            with patch.object(tc, "_TeamCity__get_put_request_headers", return_value={}):
                result = tc.get_build_artifact_names("123")
                assert result == {"files": ["a", "b"]}

    def test_get_build_artifact_names_fail(self) -> None:
        tc = TeamCity("user", "pass", settings=self.mock_settings)
        with (
            patch("ci_tools.dimrset_delivery.lib.teamcity.requests.get") as mock_get,
            patch.object(tc, "_TeamCity__get_put_request_headers", return_value={}),
            patch.object(sys, "exit") as mock_exit,
        ):
            mock_get.return_value = Mock(status_code=404, content=b"fail")
            tc.get_build_artifact_names("123")
            mock_exit.assert_called_once()

    def test_get_build_artifact_success(self) -> None:
        tc = TeamCity("user", "pass", settings=self.mock_settings)
        with patch("ci_tools.dimrset_delivery.lib.teamcity.requests.get") as mock_get:
            mock_get.return_value = Mock(status_code=200, content=b"artifact")
            result = tc.get_build_artifact("123", "path/to/artifact")
            assert result == b"artifact"

    def test_get_build_artifact_fail(self) -> None:
        tc = TeamCity("user", "pass", settings=self.mock_settings)
        with (
            patch("ci_tools.dimrset_delivery.lib.teamcity.requests.get") as mock_get,
            patch.object(sys, "exit") as mock_exit,
        ):
            mock_get.return_value = Mock(status_code=404, content=b"fail")
            tc.get_build_artifact("123", "path/to/artifact")
            mock_exit.assert_called_once()

    def test_pin_build_success(self) -> None:
        tc = TeamCity("user", "pass", settings=self.mock_settings)
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
        tc = TeamCity("user", "pass", settings=self.mock_settings)
        mock_csrf = Mock()
        mock_csrf.status_code = 200
        mock_csrf.content = b"csrf-token"
        from unittest.mock import MagicMock

        mock_response = MagicMock()
        mock_response.status_code = 401
        mock_response.content = b"fail"
        mock_response.__bool__.return_value = False
        with patch("ci_tools.dimrset_delivery.lib.teamcity.requests.get", return_value=mock_csrf):
            with patch("ci_tools.dimrset_delivery.lib.teamcity.requests.put", return_value=mock_response):
                import sys as _sys
                from io import StringIO

                captured_out = StringIO()
                captured_err = StringIO()
                old_stdout = _sys.stdout
                old_stderr = _sys.stderr
                _sys.stdout = captured_out
                _sys.stderr = captured_err
                try:
                    tc.pin_build("123")
                finally:
                    _sys.stdout = old_stdout
                    _sys.stderr = old_stderr
                output = captured_out.getvalue() + captured_err.getvalue()
                assert "Could not pin build with build id 123:" in output
                assert "401 - fail" in output
