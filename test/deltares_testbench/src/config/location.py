"""Network Path Data Class.

Copyright (C)  Stichting Deltares, 2024
"""

from typing import Optional

from src.config.credentials import Credentials
from src.config.types.path_type import PathType


class Location:
    """Network path configuration."""

    def __init__(self) -> None:
        self.__name: str = ""
        self.__type: PathType = PathType.NONE
        self.__credentials: Credentials = Credentials()
        self.__root: str = ""
        self.__from: str = ""
        self.__to: str = ""
        self.__version: Optional[str] = None

    @property
    def name(self) -> str:
        """Name of path."""
        return self.__name

    @name.setter
    def name(self, value: str) -> None:
        self.__name = value

    @property
    def type(self) -> PathType:
        """Type object definition for network (check, reference or input)."""
        return self.__type

    @type.setter
    def type(self, value: PathType) -> None:
        self.__type = value

    @property
    def credentials(self) -> Credentials:
        """Credentials object."""
        return self.__credentials

    @credentials.setter
    def credentials(self, value: Credentials) -> None:
        self.__credentials = value

    @property
    def root(self) -> str:
        """Root of the network path (http(s), net, disk)."""
        return self.__root

    @root.setter
    def root(self, value: str) -> None:
        self.__root = value

    @property
    def from_path(self) -> str:
        """From subpath including trailing escape character (e.g. /)."""
        return self.__from

    @from_path.setter
    def from_path(self, value: str) -> None:
        self.__from = value

    @property
    def to_path(self) -> str:
        """Path the root + from is copied to, sub directory of specified local path."""
        if self.__to == "":
            return self.__from
        return self.__to

    @to_path.setter
    def to_path(self, value: str) -> None:
        self.__to = value

    @property
    def version(self) -> Optional[str]:
        """Version of application (mainly used for subversion)."""
        return self.__version

    @version.setter
    def version(self, value: Optional[str]) -> None:
        self.__version = value
