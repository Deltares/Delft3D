import pytest
from unittest.mock import MagicMock, Mock
from unittest.mock import patch
from src.config.credentials import Credentials
from src.config.program_config import ProgramConfig
from src.config.types.handler_type import HandlerType
from src.utils.handlers.handler_factory import HandlerFactory
from src.suite.program import Program


class TestSvnHandler:
    @staticmethod
    @patch('src.utils.handlers.resolve_handler.ResolveHandler.detect', return_value=HandlerType.SVN)
    @patch('src.suite.program.Program', autospec=True)
    def test_svn_download_on_exception_mask_credentials(mock_detect, mock_program):
        # Arrange
        from_path = "https://repos.deltares.nl"
        version = "2023.10.20T09:00"
        credentials = Credentials()
        credentials.name = "safe_credentials"
        credentials.username = "John_doe"
        credentials.password = "hello-world"
        logger = Mock()
        program_config = ProgramConfig()
        program_config.name = "svn"
        mock_program = MagicMock(spec=Program)
        mock_program_instance = mock_program.return_value
        error_string = version + " --username " + credentials.username + " --password " + credentials.password
        error_string_masked = version + " --username " + credentials.username + " --password ******"
        mock_program_instance.getError.return_value = error_string
        mock_program_instance.name = "svn"
        programs = [mock_program_instance]

        # Act
        with pytest.raises(RuntimeError) as excinfo:
            HandlerFactory.download(
                from_path,
                "test/data",
                programs,
                logger,
                credentials,
                version,
            )

        # Assert
        assert "Errors during svn download: " + error_string_masked in str(excinfo.value)