"""Credentials Data Class.

Copyright (C)  Stichting Deltares, 2024
"""


class Credentials:
    """Credentials for login on."""

    def __init__(self) -> None:
        self.__name: str = ""
        self.__username: str = ""
        self.__password: str = ""

    @property
    def name(self) -> str:
        """Name assigned to this credential object."""
        return self.__name

    @name.setter
    def name(self, value: str) -> None:
        self.__name = value

    @property
    def username(self) -> str:
        """Username to use for logging in."""
        return self.__username

    @username.setter
    def username(self, value: str) -> None:
        self.__username = value

    @property
    def password(self) -> str:
        """Password to use for logging in."""
        return self.__password

    @password.setter
    def password(self, value: str) -> None:
        self.__password = value
