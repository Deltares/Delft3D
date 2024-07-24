"""TestCase Failure Class.

Copyright (C)  Stichting Deltares, 2024
"""


class TestCaseFailure(Exception):
    """Custom error for test failures."""

    def __init__(self, value: str) -> None:
        self.__value = value

    def __str__(self) -> str:
        return repr(self.__value)
