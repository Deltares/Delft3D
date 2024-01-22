"""
Description: Enum for presence type
-----------------------------------------------------
Copyright (C)  Stichting Deltares, 2023
"""

from enum import Enum


class PresenceType(Enum):
    """Type of file presence"""

    NONE = 0
    PRESENT = 1
    ABSENT = 2
