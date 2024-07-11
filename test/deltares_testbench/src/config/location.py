"""
Description: Network Path Data Class
-----------------------------------------------------
Copyright (C)  Stichting Deltares, 2023
"""

from typing import Optional

from src.config.credentials import Credentials
from src.config.types.path_type import PathType


class Location:
    """Network path configuration"""

    def __init__(self):
        self.__name: str = ""
        self.__type: PathType = PathType.NONE
        self.__credentials: Credentials = Credentials()
        self.__root: str = ""
        self.__from: str = ""
        self.__to: str = ""
        self.__version: Optional[str] = None

    @property
    def name(self) -> str:
        """name of path"""
        return self.__name

    @name.setter
    def name(self, value: str):
        self.__name = value

    @property
    def type(self) -> PathType:
        """type object definition for network (check, reference or input)"""
        return self.__type

    @type.setter
    def type(self, value: PathType):
        self.__type = value

    @property
    def credentials(self) -> Credentials:
        """credentials object"""
        return self.__credentials

    @credentials.setter
    def credentials(self, value: Credentials):
        self.__credentials = value

    @property
    def root(self) -> str:
        """root of the network path (http(s), net, disk)"""
        return self.__root

    @root.setter
    def root(self, value: str):
        self.__root = value

    @property
    def from_path(self) -> str:
        """from subpath including trailing escape character (e.g. /)"""
        return self.__from

    @from_path.setter
    def from_path(self, value: str):
        self.__from = value

    @property
    def to_path(self) -> str:
        """path the root + from is copied to, sub directory of specified local path"""
        if self.__to == "":
            return self.__from
        return self.__to

    @to_path.setter
    def to_path(self, value: str):
        self.__to = value

    @property
    def version(self) -> Optional[str]:
        """version of application (mainly used for subversion)"""
        return self.__version

    @version.setter
    def version(self, value: Optional[str]):
        self.__version = value
