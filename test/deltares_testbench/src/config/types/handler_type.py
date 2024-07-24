"""Handler type.

Copyright (C)  Stichting Deltares, 2024
"""

from enum import Enum


class HandlerType(Enum):
    """Enum for Handler used in HandlerFactory."""

    NONE = 0
    WEB = 1
    SVN = 2
    FTP = 3
    NET = 4
    PATH = 5
    MINIO = 6
