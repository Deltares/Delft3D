"""
Description: type of result for test case
-----------------------------------------------------
Copyright (C)  Stichting Deltares, 2023
"""

from enum import IntEnum


class TestResultType(IntEnum):
    __test__ = False
    """Type of result for test case"""

    Empty = (1,)
    Error = (2,)
    Differences = (3,)
    Exception = (4,)
    Passed = 5
