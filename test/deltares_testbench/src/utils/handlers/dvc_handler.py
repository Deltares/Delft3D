"""Executes DVC commands.

Copyright (C)  Stichting Deltares, 2025
"""

from typing import Optional

from src.config.credentials import Credentials
from src.utils.handlers.i_handler import IHandler
from src.utils.logging.i_logger import ILogger


class DVCHandler(IHandler):
    """DVC wrapper, has handler interface."""

    def download(
        self, from_path: str, to_path: str, credentials: Credentials, version: Optional[str], logger: ILogger
    ) -> None:
        """Set up a DVC client connection.

        You can specify the download source and destination.

        Parameters
        ----------
        from_path : str
            DVC URL.
        to_path : str
            Download location.
        credentials : Credentials
            DVC credentials.
        version : str
            MD5 hash string.
        logger : ILogger
            The logger that logs to a file.
        """
        logger.info(f"Starting DVC download from: {from_path}")
        logger.info(f"Download destination: {to_path}")
        if version:
            logger.info(f"Version (MD5 hash): {version}")
        logger.info(f"Using credentials: {credentials.name}")

        # TODO: Implement actual DVC download logic
        logger.warning("DVC download method is not yet implemented")

