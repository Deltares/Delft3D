"""
Description: SkipLine Data Class
-----------------------------------------------------
Copyright (C)  Stichting Deltares, 2023
"""


class SkipLine:
    """Skipped line information"""

    def __init__(self):
        self.__name = ""

    @property
    def name(self):
        return self.__name

    @name.setter
    def name(self, value):
        self.__name = value
