"""Local Paths Data Class.

Copyright (C)  Stichting Deltares, 2024
"""


class LocalPaths:
    """Class containing locations to given root directories."""

    def __init__(self) -> None:
        self.__cases_path: str = "cases"
        self.__engines_path: str = "engines"
        self.__reference_path: str = "references"

    @property
    def cases_path(self) -> str:
        """Path to the data of the test cases."""
        return self.__cases_path

    @cases_path.setter
    def cases_path(self, value: str) -> None:
        self.__cases_path = value

    @property
    def engines_path(self) -> str:
        """Path to the engines (executables to run)."""
        return self.__engines_path

    @engines_path.setter
    def engines_path(self, value: str) -> None:
        self.__engines_path = value

    @property
    def reference_path(self) -> str:
        """Path to the reference data."""
        return self.__reference_path

    @reference_path.setter
    def reference_path(self, value: str) -> None:
        self.__reference_path = value
