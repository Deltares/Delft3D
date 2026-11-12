"""Resolve Handler helper.

Copyright (C)  Stichting Deltares, 2026
"""

from abc import ABC

from src.config.types.handler_type import HandlerType
from src.utils.logging.i_logger import ILogger


class ResolveHandler(ABC):
    """Detect type of handler behind a path."""

    @classmethod
    def detect(cls, path: str, logger: ILogger) -> HandlerType:
        """Detect which protocol handler is needed for the path.

        Parameters
        ----------
        path : str
            URL.
        credentials : Optional[Credentials]
            Credentials to use.

        Returns
        -------
        HandlerType
            Detected handler type.
        """
        logger.debug(f"detecting handler for {path}")

        if path.endswith(".dvc"):
            return HandlerType.DVC
        else:
            return HandlerType.NONE
