"""Enum for runner mode.

Copyright (C)  Stichting Deltares, 2024
"""

from enum import Enum


class ModeType(Enum):
    """Enum for run mode."""

    REFERENCE = 1
    COMPARE = 2
    LIST = 3
    TEST_CASE_LIST = 4
