"""Type of result for test case.

Copyright (C)  Stichting Deltares, 2024
"""

from enum import IntEnum


class TestResultType(IntEnum):
    """Type of result for test case."""

    __test__ = False

    Empty = (1,)
    Error = (2,)
    Differences = (3,)
    Exception = (4,)
    Passed = 5
