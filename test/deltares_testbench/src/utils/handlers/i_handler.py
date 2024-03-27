from abc import ABC, abstractmethod
from typing import Optional

from src.config.credentials import Credentials
from src.utils.logging.i_logger import ILogger


class IHandler(ABC):
    @abstractmethod
    def prepare_upload(
        self, from_path: str, to_path: str, credentials: Credentials, logger: ILogger
    ) -> None:
        """Prepares upload

        Args:
            from_path (str): original path
            to_path (str): destination path
            credentials (Credentials): credentials needed for connection
            logger (ILogger): logger to use

        Raises:
            NotImplementedError: this method could be invalid for some handlers
        """

    @abstractmethod
    def upload(
        self, from_path: str, to_path: str, credentials: Credentials, logger: ILogger
    ) -> None:
        """Upload file using the specified from and to path

        Args:
            from_path (str): original path
            to_path (str): destination path
            credentials (Credentials): credentials needed for connection
            logger (ILogger): logger to use

        Raises:
            NotImplementedError: this method could be invalid for some handlers
        """

    @abstractmethod
    def download(
        self,
        from_path: str,
        to_path: str,
        credentials: Credentials,
        version: Optional[str],
        logger: ILogger,
    ):
        """Download a file from the specified location

        Args:
            from_path (str): original path
            to_path (str): destination path
            credentials (Credentials): credentials needed for connection
            version (str): version to use
            logger (ILogger): logger to use

        Raises:
            NotImplementedError: this method could be invalid for some handlers
        """
