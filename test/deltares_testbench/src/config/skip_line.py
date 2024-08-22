"""SkipLine Data Class.

Copyright (C)  Stichting Deltares, 2024
"""


class SkipLine:
    """Skipped line information."""

    def __init__(self) -> None:
        self.__name = ""

    @property
    def name(self):
        return self.__name

    @name.setter
    def name(self, value) -> None:
        self.__name = value
