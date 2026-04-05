from abc import ABC, abstractmethod
from typing import Optional

from src.config.credentials import Credentials
from src.utils.logging.i_logger import ILogger


class IHandler(ABC):
    @abstractmethod
    def download(
        self,
        from_path: str,
        to_path: str,
        credentials: Credentials,
        version: Optional[str],
        logger: ILogger,
    ):
        """Download a file from the specified location.

        Parameters
        ----------
        from_path : str
            Original path.
        to_path : str
            Destination path.
        credentials : Credentials
            Credentials needed for connection.
        version : str
            Version to use.
        logger : ILogger
            Logger to use.

        Raises
        ------
        NotImplementedError
            This method could be invalid for some handlers.
        """
