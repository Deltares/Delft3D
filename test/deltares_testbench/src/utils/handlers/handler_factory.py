"""Handler Factory.

Copyright (C)  Stichting Deltares, 2026
"""

import os
from abc import ABC
from typing import List, Optional

from src.config.credentials import Credentials
from src.config.types.handler_type import HandlerType
from src.suite.program import Program
from src.utils.handlers.dvc_handler import DvcHandler
from src.utils.handlers.i_handler import IHandler
from src.utils.handlers.resolve_handler import ResolveHandler
from src.utils.logging.i_logger import ILogger
from src.utils.paths import Paths
from src.utils.unzipper import Unzipper


class HandlerFactory(ABC):
    """Chooses which type of handler is used for upload and download actions."""

    @classmethod
    def __get_handler(cls, to_path: str, programs: List[Program], logger: ILogger) -> IHandler:
        """Create handler based on destination path.

        Parameters
        ----------
        to_path : str
            Destination path.
        credentials : Credentials, optional
            Credentials needed for connection. Defaults to None.

        Raises
        ------
        AttributeError
            Of handler could not be detected.

        Returns
        -------
        IHandler
            Specific handler.
        """
        handler_type = ResolveHandler.detect(to_path, logger)
        handler: IHandler

        if handler_type == HandlerType.DVC:
            logger.debug(f"using DVC handler for {to_path}")
            handler = DvcHandler()
        if handler_type == HandlerType.NONE:
            raise AttributeError("upload :: no type specified")

        return handler

    @classmethod
    def download(
        cls,
        from_path: str,
        to_path: str,
        programs: List[Program],
        logger: ILogger,
        credentials: Optional[Credentials] = None,
        version: Optional[str] = None,
        unzip: bool = False,
    ) -> None:
        """Download data from location.

        Parameters
        ----------
        from_path : str
            Source path.
        to_path : str
            Target path.
        credentials : Optional[Credentials], optional
            Credentials to use. Defaults to None.
        version : Optional[str], optional
            Version. Defaults to None.
        unzip : bool, optional
            Try to unzip file. Defaults to False.
        """
        rtp = Paths().rebuildToLocalPath(to_path)

        handler = cls.__get_handler(from_path, programs, logger, credentials)

        # DVC manages file placement itself based on the .dvc file;
        # creating to_path would leave an empty directory.
        if not isinstance(handler, DvcHandler):
            os.makedirs(rtp, exist_ok=True)

        handler.download(from_path, rtp, credentials, version, logger)
        if unzip:
            Unzipper().recursive(rtp, logger)
