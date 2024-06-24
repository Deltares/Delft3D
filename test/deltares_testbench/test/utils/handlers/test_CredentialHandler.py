import io
import sys
from pathlib import Path

import pytest
from pyfakefs.fake_filesystem import FakeFilesystem
from pytest_mock import MockerFixture

from src.config.credentials import Credentials
from src.utils.handlers.credential_handler import CredentialHandler, CredentialStatus
from src.utils.logging.console_logger import ConsoleLogger


class TestCredentialHandler:
    @pytest.mark.parametrize(
        "username, password, status",
        [
            ("", "", CredentialStatus.MISSING),
            ("testname", "", CredentialStatus.USERNAME),
            ("", "testpass", CredentialStatus.PASSWORD),
            ("testname", "testpass", CredentialStatus.BOTH),
        ],
    )
    def test_credentialstatus(self, username, password, status):
        # Arrange
        credentials = Credentials()
        credentials.username = username
        credentials.password = password
        handler = CredentialHandler(credentials=credentials)

        # Act
        result = handler.credential_status_detect()

        # Assert
        assert result == status

    def test_get_credentials(self, fs: FakeFilesystem):
        # Arrange
        credentials = Credentials()
        credentials.username = "test"
        credentials.password = "test"
        handler = CredentialHandler(credentials=credentials)
        fs.create_file("cred_file", contents=self.setup_content())
        handler.cred_path = Path("cred_file")

        # Act
        handler.get_credentials()

        # Assert
        assert credentials.username == "testname"
        assert credentials.password == "testpass"

    @pytest.mark.parametrize(
        "username, password, status",
        [
            ("", "", CredentialStatus.MISSING),
            ("testname", "", CredentialStatus.USERNAME),
            ("", "testpass", CredentialStatus.PASSWORD),
            ("testname", "testpass", CredentialStatus.BOTH),
        ],
    )
    def test_setup_credentials(self, username, password, status, mocker: MockerFixture, fs: FakeFilesystem):
        # Arrange
        credentials = Credentials()
        credentials.username = username
        credentials.password = password
        mocked_logger = mocker.Mock(spec=ConsoleLogger)

        # Act
        handler = CredentialHandler(credentials=credentials)
        handler.logger = mocked_logger
        handler.cred_path = Path("no-file")
        handler.setup_credentials(False)

        # Assert
        warningcalls = mocked_logger.warning.call_args_list
        errorcalls = mocked_logger.error.call_args_list
        if status == CredentialStatus.BOTH:
            assert mocker.call("Using commandline credentials!") in warningcalls
        if status == CredentialStatus.USERNAME:
            assert mocker.call("Only username was provided on the commandline.") in warningcalls
            assert mocker.call("No credential file found attempting run with only username.") in errorcalls
        if status == CredentialStatus.PASSWORD:
            assert mocker.call("Only password was provided on the commandline.") in warningcalls
            assert mocker.call("No credential file found attempting run with only password.") in errorcalls
        if status == CredentialStatus.MISSING:
            assert mocker.call("No credentials provided and no credential file is available.") in errorcalls
        assert mocker.call(self.setup_warning(handler)) in warningcalls

    def test_credential_creation(self, fs: FakeFilesystem):
        # Arrange
        credentials = Credentials()
        credentials.username = "testname"
        credentials.password = "testpass"
        handler = CredentialHandler(credentials=credentials)
        sys.stdin = io.StringIO("yes")
        handler.cred_path = Path("cred_dir/sub_dir/cred_file")

        # Act
        handler.setup_credentials(True)

        # Assert
        with open(handler.cred_path) as file:
            lines = file.readlines()
            assert "[default]\n" == lines[0]
            assert "aws_access_key_id = testname\n" == lines[1]
            assert "aws_secret_access_key = testpass\n" == lines[2]

    def test_setup_file__no_credentials__log_error(self, mocker: MockerFixture) -> None:
        # Arrange
        handler = CredentialHandler()
        logger_mock = mocker.Mock()
        handler.logger = logger_mock

        # Act
        handler.setup_file()

        # Assert
        logger_mock.error.assert_called_once()
        assert "missing" in logger_mock.error.call_args.args[0]

    def setup_content(self):
        return "\n".join(["[default]", "aws_access_key_id = testname", "aws_secret_access_key = testpass", ""])

    def setup_warning(self, handler: CredentialHandler):
        return "\n".join(
            [
                f"To authenticate, create a 'credentials' file in your home directory: {handler.cred_path.expanduser()}",
                "Example content of the credentials file:",
                "[default]",
                "aws_access_key_id = $ACCESS_KEY_ID",
                "aws_secret_access_key = $SECRET_ACCESS_KEY",
                "",
                "Where $ACCESS_KEY_ID and $SECRET_ACCESS_KEY are your personal access and secret keys.",
                "Create your own personal keys for MinIO at: https://s3-console.deltares.nl",
                "Details: https://docs.aws.amazon.com/cli/latest/userguide/cli-configure-files.html",
            ]
        )
