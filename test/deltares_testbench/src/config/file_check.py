"""File Check Data Class.

Copyright (C)  Stichting Deltares, 2024
"""

from typing import Dict, List

from src.config.parameter import Parameter
from src.config.skip_line import SkipLine
from src.config.types.file_type import FileType
from src.config.types.presence_type import PresenceType


class FileCheck:
    """Engine configuration."""

    def __init__(self) -> None:
        self.__name = ""
        self.__type = FileType.NONE
        self.__parameters: Dict[str, List[Parameter]] = {}
        self.__skip_lines: Dict[str, List[SkipLine]] = {}
        self.__ignore = False
        self.__presence = PresenceType.NONE

    @property
    def name(self) -> str:
        return self.__name

    @name.setter
    def name(self, value: str) -> None:
        self.__name = value

    @property
    def type(self) -> FileType:
        return self.__type

    @type.setter
    def type(self, value: FileType) -> None:
        self.__type = value

    @property
    def parameters(self) -> Dict[str, List[Parameter]]:
        return self.__parameters

    @parameters.setter
    def parameters(self, value: Dict[str, List[Parameter]]) -> None:
        self.__parameters = value

    @property
    def skip_lines(self) -> Dict[str, List[SkipLine]]:
        return self.__skip_lines

    @skip_lines.setter
    def skip_lines(self, value: Dict[str, List[SkipLine]]) -> None:
        self.__skip_lines = value

    @property
    def ignore(self) -> bool:
        return self.__ignore

    @ignore.setter
    def ignore(self, value: bool) -> None:
        self.__ignore = value

    @property
    def presence(self) -> PresenceType:
        return self.__presence

    @presence.setter
    def presence(self, value: PresenceType) -> None:
        self.__presence = value
