"""Handler type.

Copyright (C)  Stichting Deltares, 2026
"""

from enum import Enum


class HandlerType(Enum):
    """Enum for Handler used in HandlerFactory."""

    NONE = 0
    DVC = 1
