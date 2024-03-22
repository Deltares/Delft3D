"""
Description: TestCasePath Data Class
-----------------------------------------------------
Copyright (C)  Stichting Deltares, 2024
"""

from typing import Optional


class TestCasePath:
    """Class for registering path to test case data"""

    __test__ = False  # Pytest gets confused by classes with names starting with 'Test'.

    def __init__(self, prefix: str, version: Optional[str] = None):
        self.__prefix = prefix
        self.__version = version

    @property
    def prefix(self) -> str:
        """Get path prefix to the test case data."""
        return self.__prefix

    @prefix.setter
    def path(self, value: str) -> None:
        """Set path prefix to the test case data."""
        self.__prefix = value

    @property
    def version(self) -> Optional[str]:
        """Get version of test case data."""
        return self.__version

    @version.setter
    def version(self, value: Optional[str]) -> None:
        """Set version of test case data."""
        self.__version = value
